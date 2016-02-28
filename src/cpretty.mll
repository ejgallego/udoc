(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*s Utility functions for the scanners *)

{
  open Format
  open Lexing

  module type S = sig

  module OutB : Output.S

  val coq_file        : string -> Cdglobals.coq_module -> unit

  end

  (* Output Back End *)
  module Make (Out : Output.S) = struct
  module OutB = Out

  (* A list function we need *)
  let rec take n ls =
    if n = 0 then [] else
      match ls with
        | [] -> []
        | (l :: ls) -> l :: (take (n-1) ls)

  (* count the number of spaces at the beginning of a string *)
  let count_spaces s =
    let n = String.length s in
    let rec count c i =
      if i == n then c,i else match s.[i] with
        | '\t' -> count (c + (8 - (c mod 8))) (i + 1)
        | ' ' -> count (c + 1) (i + 1)
        | _ -> c,i
    in
      count 0 0

  let remove_newline s =
    let n = String.length s in
    let rec count i = if i == n || s.[i] <> '\n' then i else count (i + 1) in
    let i = count 0 in
    i, String.sub s i (n - i)

  let count_dashes s =
    let c = ref 0 in
    for i = 0 to String.length s - 1 do if s.[i] = '-' then incr c done;
    !c

  let cut_head_tail_spaces s =
    let n = String.length s in
    let rec look_up i = if i == n || s.[i] <> ' ' then i else look_up (i+1) in
    let rec look_dn i = if i == -1 || s.[i] <> ' ' then i else look_dn (i-1) in
    let l = look_up 0 in
    let r = look_dn (n-1) in
    if l <= r then String.sub s l (r-l+1) else s

  let sec_title s =
    let rec count lev i =
      if s.[i] = '*' then
        count (succ lev) (succ i)
      else
        let t = String.sub s i (String.length s - i) in
        lev, cut_head_tail_spaces t
    in
    count 0 (String.index s '*')

  let strip_eol s =
    let eol = s.[String.length s - 1] = '\n' in
    (eol, if eol then String.sub s 1 (String.length s - 1) else s)


  let formatted = ref false
  let brackets = ref 0
  let comment_level = ref 0
  let in_proof = ref None

  let in_env start stop =
    let r = ref false in
    let start_env () = r := true; start () in
    let stop_env () = if !r then stop (); r := false in
      (fun x -> !r), start_env, stop_env

  let in_emph, start_emph, stop_emph = in_env OutB.start_emph OutB.stop_emph
  let in_quote, start_quote, stop_quote = in_env OutB.start_quote OutB.stop_quote

  let url_buffer = Buffer.create 40
  let url_name_buffer = Buffer.create 40

  let backtrack lexbuf = lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p

  let backtrack_past_newline lexbuf =
    let buf = lexeme lexbuf in
    let splits = Str.bounded_split_delim (Str.regexp "['\n']") buf 2 in
      match splits with
        | [] -> ()
        | (_ :: []) -> ()
        | (s1 :: rest :: _) ->
            let length_skip = 1 + String.length s1 in
              lexbuf.lex_curr_pos <- lexbuf.lex_start_pos + length_skip

  (* Reset the globals *)

  let reset () =
    formatted := false;
    brackets := 0;
    comment_level := 0

  (* erasing of Section/End *)

  let section_re = Str.regexp "[ \t]*Section"
  let end_re = Str.regexp "[ \t]*End"
  let is_section s = Str.string_match section_re s 0
  let is_end s = Str.string_match end_re s 0

  let sections_to_close = ref 0

  let section_or_end s =
    if is_section s then begin
      incr sections_to_close; true
    end else if is_end s then begin
      if !sections_to_close > 0 then begin
        decr sections_to_close; true
      end else
        false
    end else
      true

  (* for item lists *)
  type list_compare =
      | Before
      | StartLevel of int
      | InLevel of int * bool

  (* Before : we're before any levels
     StartLevel : at the same column as the dash in a level
     InLevel : after the dash of this level, but before any deeper dashes.
               bool is true if this is the last level *)
  let find_level levels cur_indent =
    match levels with
    | [] -> Before
    | (l::ls) ->
        if cur_indent < l then Before
        else
          (* cur_indent will never be less than the head of the list *)
          let rec findind ls n =
            match ls with
            | [] -> InLevel (n,true)
            | (l :: []) -> if cur_indent = l then StartLevel n
                           else InLevel (n,true)
            | (l1 :: l2 :: ls) ->
                if cur_indent = l1 then StartLevel n
                else if cur_indent < l2 then InLevel (n,false)
                     else findind (l2 :: ls) (n+1)
          in
            findind (l::ls) 1

  type is_start_list =
    | Rule
    | List of int
    | Neither

  let check_start_list str =
    let n_dashes = count_dashes str in
    let (n_spaces,_) = count_spaces str in
      if n_dashes >= 4 then
        Rule
      else
        if n_dashes = 1 then
          List n_spaces
        else
          Neither
  (* tokens pretty-print *)

  let output_indented_keyword s lexbuf =
    let nbsp,isp = count_spaces s in
    OutB.indentation nbsp;
    let s = String.sub s isp (String.length s - isp) in
    OutB.keyword s (lexeme_start lexbuf + isp)

}

(*s Regular expressions *)

let space    = [' ' '\t']
let space_nl = [' ' '\t' '\n' '\r']
let nl       = "\r\n" | '\n'

let firstchar =
  ['A'-'Z' 'a'-'z' '_'] |
  (* superscript 1 *)
  '\194' '\185' |
  (* utf-8 latin 1 supplement *)
  '\195' ['\128'-'\150'] |
  '\195' ['\152'-'\182'] |
  '\195' ['\184'-'\191'] |
  (* utf-8 letterlike symbols *)
  '\206' (['\145'-'\161'] | ['\163'-'\191']) |
  '\207' (['\145'-'\191']) |
  '\226' ('\130' [ '\128'-'\137' ] (* subscripts *)
    | '\129' [ '\176'-'\187' ] (* superscripts *)
    | '\132' ['\128'-'\191'] | '\133' ['\128'-'\143'])
let identchar =
  firstchar | ['\'' '0'-'9' '@' ]
let id = firstchar identchar*
let pfx_id = (id '.')*
let identifier =
  id | pfx_id id

(* This misses unicode stuff, and it adds "[" and "]".  It's only an
   approximation of idents - used for detecting whether an underscore
   is part of an identifier or meant to indicate emphasis *)
let nonidentchar = [^ 'A'-'Z' 'a'-'z' '_' '[' ']' '\'' '0'-'9' '@' ]

let printing_token = [^ ' ' '\t']*

let thm_token =
  "Theorem"
  | "Lemma"
  | "Fact"
  | "Remark"
  | "Corollary"
  | "Proposition"
  | "Property"
  | "Goal"

let prf_token =
  "Next" space+ "Obligation"
  | "Proof" (space* "." | space+ "with" | space+ "using")

let immediate_prf_token =
  (* Approximation of a proof term, if not in the prf_token case *)
  (* To be checked after prf_token *)
  "Proof" space* [^ '.' 'w' 'u']

let def_token =
  "Definition"
  | "Let"
  | "Class"
  | "SubClass"
  | "Example"
  | "Fixpoint"
  | "Function"
  | "Boxed"
  | "CoFixpoint"
  | "Record"
  | "Variant"
  | "Structure"
  | "Scheme"
  | "Inductive"
  | "CoInductive"
  | "Equations"
  | "Instance"
  | "Declare" space+ "Instance"
  | "Global" space+ "Instance"
  | "Functional" space+ "Scheme"

let decl_token =
  "Hypothesis"
  | "Hypotheses"
  | "Parameter" 's'?
  | "Axiom" 's'?
  | "Conjecture"

let gallina_ext =
  "Module"
  | "Include" space+ "Type"
  | "Include"
  | "Declare" space+ "Module"
  | "Transparent"
  | "Opaque"
  | "Canonical"
  | "Coercion"
  | "Identity"
  | "Implicit"
  | "Tactic" space+ "Notation"
  | "Section"
  | "Context"
  | "Variable" 's'?
  | ("Hypothesis" | "Hypotheses")
  | "End"

let commands =
  "Pwd"
  | "Cd"
  | "Drop"
  | "ProtectedLoop"
  | "Quit"
  | "Restart"
  | "Load"
  | "Add"
  | "Remove" space+ "Loadpath"
  | "Print"
  | "Inspect"
  | "About"
  | "SearchAbout"
  | "SearchRewrite"
  | "Search"
  | "Locate"
  | "Eval"
  | "Reset"
  | "Check"
  | "Type"

  | "Section"
  | "Chapter"
  | "Variable" 's'?
  | ("Hypothesis" | "Hypotheses")
  | "End"

let end_kw =
  immediate_prf_token | "Qed" | "Defined" | "Save" | "Admitted" | "Abort"

let extraction =
  "Extraction"
  | "Recursive" space+ "Extraction"
  | "Extract"

let notation_kw =
    "Notation"
  | "Infix"
  | "Reserved" space+ "Notation"

let gallina_prim_kw = thm_token | def_token | decl_token | gallina_ext | commands | extraction | notation_kw

let prog_kw =
  "Program" space+ gallina_prim_kw
  | "Obligation"
  | "Obligations"
  | "Solve"

let gallina_kw = gallina_prim_kw | prog_kw

let section = "*" | "**" | "***" | "****"

let item_space = "    "

let begin_hide = "(*" space* "begin" space+ "hide" space* "*)" space* nl
let end_hide = "(*" space* "end" space+ "hide" space* "*)" space* nl

(*s Scanning Coq, at beginning of line *)

rule coq_bol = parse

  (* Rule for lines *)
  | space* nl+
      { if !in_proof = None
        then OutB.empty_line_of_code ();
        coq_bol lexbuf
      }

  (* Switching to a comment. *)
  | space* "(**" space_nl
      { OutB.end_coq (); OutB.start_doc ();
        let eol = doc_bol lexbuf in
        OutB.end_doc (); OutB.start_coq ();
        (* Now this will eat the lines of code after the comment, example:

         l1: (** Uhhh *)
         l2:
         l3: Lemma a

           Produces a textarea with l2 in it.

        *)
        if eol then coq_bol lexbuf else coq lexbuf }

  (* Hide *)
  | space* begin_hide
      { skip_hide lexbuf; coq_bol lexbuf }

  (* Theorem: sets `in_proof` mode *)
  | space* thm_token
      { let s = lexeme lexbuf in
        output_indented_keyword s lexbuf;
        let eol = body lexbuf in
        in_proof := Some eol;
        if eol then coq_bol lexbuf else coq lexbuf }

  (* `Proof` token, sets `in_proof` *)
  | space* prf_token
      { in_proof := Some true;
        let eol = backtrack lexbuf; body_bol lexbuf in
        if eol then coq_bol lexbuf else coq lexbuf
      }

  (* `Qed` token, unsets `in_proof` *)
  | space* end_kw {
      let eol =
        if !in_proof = None then
          begin backtrack lexbuf; body_bol lexbuf end
        else skip_to_dot lexbuf
      in
        in_proof := None;
        if eol then coq_bol lexbuf else coq lexbuf }

  (* `Definition`, notation, etc... *)
  | space* gallina_kw
      {
        in_proof := None;
        let s   = lexeme lexbuf in
        output_indented_keyword s lexbuf;
        let eol = body lexbuf   in
        if eol then coq_bol lexbuf else coq lexbuf
      }

  (* ?? *)
  | space* "(*"
      { comment_level := 1;
        begin
          let s = lexeme lexbuf in
          let nbsp,isp = count_spaces s in
            OutB.indentation nbsp;
            OutB.start_comment ();
        end;
        let eol = comment lexbuf in
          if eol then coq_bol lexbuf else coq lexbuf }
  | eof
      { () }

  | _
      { let eol =
            begin backtrack lexbuf; body_bol lexbuf end
        in
          if eol then coq_bol lexbuf else coq lexbuf }

(*s Scanning Coq elsewhere *)
and coq = parse
  | nl
      { if not (!in_proof <> None) then OutB.line_break(); coq_bol lexbuf }
  | "(**" space_nl
      { OutB.end_coq (); OutB.start_doc ();
        let eol = doc_bol lexbuf in
          OutB.end_doc (); OutB.start_coq ();
          if eol then coq_bol lexbuf else coq lexbuf }
  | "(*"
      { comment_level := 1;
        begin
          let s = lexeme lexbuf in
          let nbsp,isp = count_spaces s in
            OutB.indentation nbsp;
            OutB.start_comment ();
        end;
        let eol = comment lexbuf in
          if eol then coq_bol lexbuf
          else coq lexbuf
      }
  | nl+ space* "]]"
      { if not !formatted then
        begin
          (* Isn't this an anomaly *)
          let s = lexeme lexbuf in
          let nlsp,s = remove_newline s in
          let nbsp,isp = count_spaces s in
          OutB.indentation nbsp;
          let loc = lexeme_start lexbuf + isp + nlsp in
          OutB.sublexer ']' loc;
          OutB.sublexer ']' (loc+1);
          coq lexbuf
        end }
  | eof
      { () }
  | prf_token
      { let eol =
            begin backtrack lexbuf; body lexbuf end
        in if eol then coq_bol lexbuf else coq lexbuf }
  | end_kw {
      let eol = begin backtrack lexbuf; body lexbuf end
      in
      in_proof := None;
      if eol then coq_bol lexbuf else coq lexbuf }
  | gallina_kw
      { let s = lexeme lexbuf in
        OutB.ident s None;
        let eol = body lexbuf in
        if eol then coq_bol lexbuf else coq lexbuf
      }

  | space+ { OutB.char ' '; coq lexbuf }
  | eof
      { () }
  | _ { let eol =
          begin backtrack lexbuf; body lexbuf end
        in
          if eol then coq_bol lexbuf else coq lexbuf}

(*s Scanning documentation, at beginning of line *)

and doc_bol = parse
  | space* section space+ ([^'\n' '*'] | '*'+ [^'\n' ')' '*'])* ('*'+ '\n')?
      { let eol, lex = strip_eol (lexeme lexbuf) in
        let lev, s   = sec_title lex             in
        OutB.section lev (fun () -> ignore (doc None (from_string s)));
        if eol then doc_bol lexbuf else doc None lexbuf }
  | space_nl* '-'+
      { let buf' = lexeme lexbuf in
        let bufs = Str.split_delim (Str.regexp "['\n']") buf' in
        let lines = (List.length bufs) - 1 in
        let line =
          match bufs with
          | [] -> eprintf "Internal error bad_split1 - please report\n";
                  exit 1
          | _ -> List.nth bufs lines
        in
          match check_start_list line with
          | Neither -> backtrack_past_newline lexbuf; doc None lexbuf
          | List n -> OutB.paragraph ();
                      OutB.item 1; doc (Some [n]) lexbuf
          | Rule -> OutB.rule (); doc None lexbuf
      }
  | space* nl+
      { OutB.paragraph (); doc_bol lexbuf }
  | "<<" space*
      { OutB.start_verbatim false; verbatim false lexbuf; doc_bol lexbuf }
  | eof
      { true }
  | '_'
      { start_emph ();
        doc None lexbuf }
  | _
      { backtrack lexbuf; doc None lexbuf }

(*s Scanning lists - using whitespace *)
and doc_list_bol indents = parse
  | space* '-'
      { let (n_spaces,_) = count_spaces (lexeme lexbuf) in
        match find_level indents n_spaces with
        | Before -> backtrack lexbuf; doc_bol lexbuf
        | StartLevel n -> OutB.item n; doc (Some (take n indents)) lexbuf
        | InLevel (n,true) ->
            let items = List.length indents in
              OutB.item (items+1);
              doc (Some (List.append indents [n_spaces])) lexbuf
        | InLevel (_,false) ->
            backtrack lexbuf; doc_bol lexbuf
      }
  | "<<" space*
      { OutB.start_verbatim false;
        verbatim false lexbuf;
        doc_list_bol indents lexbuf }
  | "[[" nl
      { formatted := true;
        OutB.start_inline_coq_block ();
        ignore(body_bol lexbuf);
        OutB.end_inline_coq_block ();
        formatted := false;
        doc_list_bol indents lexbuf }
  | space* nl space* '-'
      { (* Like in the doc_bol production, these two productions
           exist only to deal properly with whitespace *)
        OutB.paragraph ();
        backtrack_past_newline lexbuf;
        doc_list_bol indents lexbuf }
  | space* nl space* _
      { let buf' = lexeme lexbuf in
        let buf =
          let bufs = Str.split_delim (Str.regexp "['\n']") buf' in
            match bufs with
              | (_ :: s :: []) -> s
              | (_ :: _ :: s :: _) -> s
              | _ -> eprintf "Internal error bad_split2 - please report\n";
                     exit 1
        in
        let (n_spaces,_) = count_spaces buf in
        match find_level indents n_spaces with
        | InLevel _ ->
            OutB.paragraph ();
            backtrack_past_newline lexbuf;
            doc_list_bol indents lexbuf
        | StartLevel n ->
            if n = 1 then
              begin
                OutB.stop_item ();
                backtrack_past_newline lexbuf;
                doc_bol lexbuf
              end
            else
              begin
                OutB.paragraph ();
                backtrack_past_newline lexbuf;
                doc_list_bol indents lexbuf
              end
        | Before ->
        (* Here we were at the beginning of a line, and it was blank.
           The next line started before any list items.  So: insert
           a paragraph for the empty line, rewind to whatever's just
           after the newline, then toss over to doc_bol for whatever
           comes next. *)
            OutB.stop_item ();
            OutB.paragraph ();
            backtrack_past_newline lexbuf;
            doc_bol lexbuf

      }
  | space* _
      { let (n_spaces,_) = count_spaces (lexeme lexbuf) in
        match find_level indents n_spaces with
        | Before -> OutB.stop_item (); backtrack lexbuf;
                    doc_bol lexbuf
        | StartLevel n ->
            (if n = 1 then
               OutB.stop_item ()
             else
               OutB.reach_item_level (n-1));
            backtrack lexbuf;
            doc (Some (take (n-1) indents)) lexbuf
        | InLevel (n,_) ->
            OutB.reach_item_level n;
            backtrack lexbuf;
            doc (Some (take n indents)) lexbuf
      }

(*s Scanning documentation elsewhere *)
and doc indents = parse
  | nl
      { OutB.char '\n';
        match indents with
        | Some ls -> doc_list_bol ls lexbuf
        | None -> doc_bol lexbuf }
  | "[[" nl
      {  (formatted := true;
              OutB.start_inline_coq_block ();
              let eol = body_bol lexbuf in
                OutB.end_inline_coq_block (); formatted := false;
                if eol then
                  match indents with
                  | Some ls -> doc_list_bol ls lexbuf
                  | None -> doc_bol lexbuf
                else doc indents lexbuf)}
  | "[]"
      { OutB.proofbox (); doc indents lexbuf }
  | "{{" { url lexbuf; doc indents lexbuf }
  | "["
      { (brackets := 1;
         OutB.start_inline_coq ();
         escaped_coq lexbuf;
         OutB.end_inline_coq ());
        doc indents lexbuf }
  | "(*"
      { backtrack lexbuf ;
        let bol_parse = match indents with
                        | Some is -> doc_list_bol is
                        | None   -> doc_bol
        in
        let eol = comment lexbuf in
          if eol then bol_parse lexbuf else doc indents lexbuf
      }
  | '*'* "*)" space_nl* "(**"
      {(match indents with
        | Some _ -> OutB.stop_item ()
        | None -> ());
       (* this says - if there is a blank line between the two comments,
          insert one in the output too *)
       let lines = List.length (Str.split_delim (Str.regexp "['\n']")
                                                (lexeme lexbuf))
       in
         if lines > 2 then OutB.paragraph ();
       doc_bol lexbuf
      }
  | '*'* "*)" space* nl
      { true }
  | '*'* "*)"
      { false }
  | "$"
      { OutB.start_latex_math (); escaped_math_latex lexbuf;
        doc indents lexbuf }
  | "$$"
      { OutB.char '$'; doc indents lexbuf }
  | "%"
      { escaped_latex lexbuf; doc indents lexbuf }
  | "%%"
      { OutB.char '%'; doc indents lexbuf }
  | "#"
      { escaped_html lexbuf; doc indents lexbuf }
  | "##"
      { OutB.char '#'; doc indents lexbuf }
  | nonidentchar '_' nonidentchar
      { List.iter (fun x -> OutB.char (lexeme_char lexbuf x)) [0;1;2];
        doc indents lexbuf}
  | nonidentchar '_'
      { OutB.char (lexeme_char lexbuf 0);
        start_emph () ;
        doc indents lexbuf }
  | '_' nonidentchar
      { stop_emph () ;
        OutB.char (lexeme_char lexbuf 1);
        doc indents lexbuf }
  | "<<" space*
      { OutB.start_verbatim true; verbatim true lexbuf; doc_bol lexbuf }
  | '"'
      { if in_quote ()
        then stop_quote ()
        else start_quote ();
        doc indents lexbuf }
  | eof
      { false }
  | _
      { OutB.char (lexeme_char lexbuf 0); doc indents lexbuf }

(*s Various escapings *)

and escaped_math_latex = parse
  | "$" { OutB.stop_latex_math () }
  | eof { OutB.stop_latex_math () }
  | "*)"
        { OutB.stop_latex_math (); backtrack lexbuf }
  | _   { OutB.latex_char (lexeme_char lexbuf 0); escaped_math_latex lexbuf }

and escaped_latex = parse
  | "%" { () }
  | eof { () }
  | "*)"
        { backtrack lexbuf }
  | _   { OutB.latex_char (lexeme_char lexbuf 0); escaped_latex lexbuf }

and escaped_html = parse
  | "#" { () }
  | "&#"
        { OutB.html_char '&'; OutB.html_char '#'; escaped_html lexbuf }
  | "##"
        { OutB.html_char '#'; escaped_html lexbuf }
  | eof { () }
  | "*)"
        { backtrack lexbuf }
  | _   { OutB.html_char (lexeme_char lexbuf 0); escaped_html lexbuf }

and verbatim inline = parse
  | nl ">>" space* nl { OutB.verbatim_char inline '\n'; OutB.stop_verbatim inline }
  | nl ">>" { OutB.verbatim_char inline '\n'; OutB.stop_verbatim inline }
  | ">>" { OutB.stop_verbatim inline }
  | "*)" { OutB.stop_verbatim inline; backtrack lexbuf }
  | eof { OutB.stop_verbatim inline }
  | _ { OutB.verbatim_char inline (lexeme_char lexbuf 0); verbatim inline lexbuf }

and url = parse
  | "}}" { OutB.url (Buffer.contents url_buffer) None; Buffer.clear url_buffer }
  | "}" { url_name lexbuf }
  | _ { Buffer.add_char url_buffer (lexeme_char lexbuf 0); url lexbuf }

and url_name = parse
  | "}" { OutB.url (Buffer.contents url_buffer) (Some (Buffer.contents url_name_buffer));
          Buffer.clear url_buffer; Buffer.clear url_name_buffer }
  | _ { Buffer.add_char url_name_buffer (lexeme_char lexbuf 0); url_name lexbuf }

(*s Coq, inside quotations *)

and escaped_coq = parse
  | "]"
      { decr brackets;
        if !brackets > 0 then
          (OutB.sublexer_in_doc ']'; escaped_coq lexbuf)
      }
  | "["
      { incr brackets;
        OutB.sublexer_in_doc '['; escaped_coq lexbuf }
  | "(*"
      { comment_level := 1;
        ignore (comment lexbuf); escaped_coq lexbuf }
  | "*)"
      {
        (* likely to be a syntax error: we escape *)
        backtrack lexbuf
      }
  | eof
      {   }
  | (identifier '.')* identifier
      { OutB.ident (lexeme lexbuf) None;
        escaped_coq lexbuf }

  | space_nl*
      { let str = lexeme lexbuf in
        OutB.end_inline_coq ();
        String.iter OutB.char str;
        OutB.start_inline_coq ();
        escaped_coq lexbuf
      }
  | _
      { OutB.sublexer_in_doc (lexeme_char lexbuf 0);
        escaped_coq lexbuf }

(*s Skip comments *)

and comment = parse
  | "(*" { incr comment_level;
           OutB.start_comment ();
           comment lexbuf }
  | "*)" space* nl {
      OutB.end_comment (); OutB.line_break ();
      decr comment_level;
      if !comment_level > 0 then comment lexbuf else true
    }
  | "*)" {
      OutB.end_comment ();
      decr comment_level;
      if !comment_level > 0 then comment lexbuf else false }
  | "[" {
      brackets := 1;
      OutB.start_inline_coq ();
      escaped_coq lexbuf;
      OutB.end_inline_coq ();
      comment lexbuf }
  | "[[" nl {
      formatted := true;
      OutB.start_inline_coq_block ();
      let _ = body_bol lexbuf in
      OutB.end_inline_coq_block ();
      formatted := false;
      comment lexbuf
    }
  | "$"
      { OutB.start_latex_math ();
        escaped_math_latex lexbuf;
        comment lexbuf }
  | "$$"
      { OutB.char '$'; OutB.char '$';
        doc None lexbuf
      }
  | "%"
      { escaped_latex lexbuf; comment lexbuf }
  | "%%"
      { comment lexbuf }
  | "#"
      { escaped_html lexbuf; comment lexbuf }
  | "##"
      { comment lexbuf }
  | eof  { false }
  | space+ {
      OutB.indentation (fst (count_spaces (lexeme lexbuf)));
      comment lexbuf
    }
  | nl   { OutB.line_break (); comment lexbuf }
  | _    { OutB.char (lexeme_char lexbuf 0);
           comment lexbuf
         }

and skip_to_dot = parse
  | '.' space* nl { true }
  | eof | '.' space+ { false }
  | "(*" { comment_level := 1; ignore (comment lexbuf); skip_to_dot lexbuf }
  | _ { skip_to_dot lexbuf }

and body_bol = parse
  | space+
      { OutB.indentation (fst (count_spaces (lexeme lexbuf))); body lexbuf }
  | _ { backtrack lexbuf; OutB.indentation 0; body lexbuf }

and body = parse
  | nl { OutB.line_break(); Lexing.new_line lexbuf; body_bol lexbuf}
  | nl+ space* "]]" space* nl
      { if not !formatted then
          begin
            let s = lexeme lexbuf in
            let nlsp,s = remove_newline s in
            let _,isp = count_spaces s in
            let loc = lexeme_start lexbuf + nlsp + isp in
            OutB.sublexer ']' loc;
            OutB.sublexer ']' (loc+1);
            body lexbuf
          end
        else
          begin
            OutB.paragraph ();
            true
          end }
  | "]]" space* nl
      { if not !formatted then
          begin
            let loc = lexeme_start lexbuf in
            OutB.sublexer ']' loc;
            OutB.sublexer ']' (loc+1);
            OutB.line_break();
            body lexbuf
          end
        else
          begin
            OutB.paragraph ();
            true
          end }
  | eof { false }
  | '.' space* nl | '.' space* eof
        { OutB.char '.'; OutB.line_break();
          if not !formatted then true else body_bol lexbuf }
  | '.' space* nl "]]" space* nl
        { OutB.char '.';
        if not !formatted then
          begin
            eprintf "Error: stray ]] at %d\n"  (lexeme_start lexbuf);
            flush stderr;
            exit 1
          end
          else
          begin
            OutB.paragraph ();
            true
          end
      }
  | '.' space+
        { OutB.char '.'; OutB.char ' ';
          if not !formatted then false else body lexbuf }
  | "(**" space_nl
      { OutB.end_coq (); OutB.start_doc ();
        let eol = doc_bol lexbuf in
          OutB.end_doc (); OutB.start_coq ();
          if eol then body_bol lexbuf else body lexbuf }
  | "(*" { comment_level := 1;
           OutB.start_comment ();
           body lexbuf
         }
  | "where"
      { OutB.ident (lexeme lexbuf) None;
        body lexbuf }
  | identifier
      { OutB.ident (lexeme lexbuf) (Some (lexeme_start lexbuf));
        body lexbuf }
  | ".."
      { OutB.char '.'; OutB.char '.';
        body lexbuf }
  | '"'
      { OutB.char '"';
        string lexbuf;
        body lexbuf }
  | space
      { OutB.char (lexeme_char lexbuf 0);
        body lexbuf }

  | _ { let c = lexeme_char lexbuf 0 in
        OutB.sublexer c (lexeme_start lexbuf);
        body lexbuf }

and string = parse
  | "\"\"" { OutB.char '"'; OutB.char '"'; string lexbuf }
  | '"'    { OutB.char '"' }
  | _      { let c = lexeme_char lexbuf 0 in OutB.char c; string lexbuf }

and skip_hide = parse
  | eof | end_hide { () }
  | _ { skip_hide lexbuf }

(*s Applying the scanners to files *)
{
  let coq_file f m =
    reset ();
    let c  = open_in f      in
    let lb = from_channel c in
    Index.current_library := m;
    OutB.start_module m;
    OutB.start_coq ();
    coq_bol lb;
    OutB.end_coq ();
    close_in c

end
}
