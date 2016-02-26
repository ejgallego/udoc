(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Format

open Cdglobals
open Index

(*s Coq keywords *)

let build_table l =
  let h = Hashtbl.create 101 in
  List.iter (fun key ->Hashtbl.add h  key ()) l;
  function s -> try Hashtbl.find h s; true with Not_found -> false

let is_keyword =
  build_table
    [ "About"; "AddPath"; "Axiom"; "Abort"; "Chapter"; "Check"; "Coercion"; "Compute"; "CoFixpoint";
      "CoInductive"; "Corollary"; "Defined"; "Definition"; "End"; "Eval"; "Example";
      "Export"; "Fact"; "Fix"; "Fixpoint"; "Function"; "Generalizable"; "Global"; "Grammar";
      "Guarded"; "Goal"; "Hint"; "Debug"; "On";
      "Hypothesis"; "Hypotheses";
      "Resolve"; "Unfold"; "Immediate"; "Extern"; "Constructors"; "Rewrite";
      "Implicit"; "Import"; "Inductive";
      "Infix"; "Lemma"; "Let"; "Load"; "Local"; "Ltac";
      "Module"; "Module Type"; "Declare Module"; "Include";
      "Mutual"; "Parameter"; "Parameters"; "Print"; "Printing"; "All"; "Proof"; "Proof with"; "Qed";
      "Record"; "Recursive"; "Remark"; "Require"; "Save"; "Scheme"; "Assumptions"; "Axioms"; "Universes";
      "Induction"; "for"; "Sort"; "Section"; "Show"; "Structure"; "Syntactic"; "Syntax"; "Tactic"; "Theorem";
      "Search"; "SearchAbout"; "SearchHead"; "SearchPattern"; "SearchRewrite";
      "Set"; "Types"; "Undo"; "Unset"; "Variable"; "Variables"; "Context";
      "Notation"; "Reserved Notation"; "Tactic Notation";
      "Delimit"; "Bind"; "Open"; "Scope"; "Inline";
      "Implicit Arguments"; "Add"; "Strict";
      "Typeclasses"; "Instance"; "Global Instance"; "Class"; "Instantiation";
      "subgoal"; "subgoals"; "vm_compute";
      "Opaque"; "Transparent"; "Time";
      "Extraction"; "Extract";
      "Variant";
      (* Program *)
      "Program Definition"; "Program Example"; "Program Fixpoint"; "Program Lemma";
      "Obligation"; "Obligations"; "Solve"; "using"; "Next Obligation"; "Next";
      "Program Instance"; "Equations"; "Equations_nocomp";
      (*i (* coq terms *) *)
      "forall"; "match"; "as"; "in"; "return"; "with"; "end"; "let"; "fun";
      "if"; "then"; "else"; "Prop"; "Set"; "Type"; ":="; "where"; "struct"; "wf"; "measure";
      "fix"; "cofix";
      (* Ltac *)
      "before"; "after"; "constr"; "ltac"; "goal"; "context"; "beta"; "delta"; "iota"; "zeta"; "lazymatch";
      (* Notations *)
      "level"; "associativity"; "no"
       ]

let is_tactic =
  build_table
    [ "intro"; "intros"; "apply"; "rewrite"; "refine"; "case"; "clear"; "injection";
      "elimtype"; "progress"; "setoid_rewrite"; "left"; "right"; "constructor"; 
      "econstructor"; "decide equality"; "abstract"; "exists"; "cbv"; "simple destruct";
      "info"; "fourier"; "field"; "specialize"; "evar"; "solve"; "instanciate";
      "quote"; "eexact"; "autorewrite";
      "destruct"; "destruction"; "destruct_call"; "dependent"; "elim"; "extensionality";
      "f_equal"; "generalize"; "generalize_eqs"; "generalize_eqs_vars"; "induction"; "rename"; "move"; "omega";
      "set"; "assert"; "do"; "repeat";
      "cut"; "assumption"; "exact"; "split"; "subst"; "try"; "discriminate";
      "simpl"; "unfold"; "red"; "compute"; "at"; "in"; "by";
      "reflexivity"; "symmetry"; "transitivity";
      "replace"; "setoid_replace"; "inversion"; "inversion_clear";
      "pattern"; "intuition"; "congruence"; "fail"; "fresh";
      "trivial"; "tauto"; "firstorder"; "ring";
      "clapply"; "program_simpl"; "program_simplify"; "eapply"; "auto"; "eauto";
      "change"; "fold"; "hnf"; "lazy"; "simple"; "eexists"; "debug"; "idtac"; "first"; "type of"; "pose";
      "eval"; "instantiate"; "until" ]

(*s Current Coq module *)

(*s Common to both LaTeX and HTML *)

let item_level = ref 0
let in_doc = ref false

(*s Customized and predefined pretty-print *)

let initialize_texmacs () =
  let ensuremath x = sprintf "<with|mode|math|\\<%s\\>>" x in
  List.fold_right (fun (s,t) tt -> Tokens.ttree_add tt s t)
    [ "*", ensuremath "times";
      "->", ensuremath "rightarrow";
      "<-", ensuremath "leftarrow";
      "<->", ensuremath "leftrightarrow";
      "=>", ensuremath "Rightarrow";
      "<=", ensuremath "le";
      ">=", ensuremath "ge";
      "<>", ensuremath "noteq";
      "~",  ensuremath "lnot";
      "/\\", ensuremath "land";
      "\\/", ensuremath "lor";
      "|-", ensuremath "vdash"
    ] Tokens.empty_ttree

let token_tree_texmacs = ref (initialize_texmacs ())

let token_tree_latex = ref Tokens.empty_ttree
let token_tree_html = ref Tokens.empty_ttree

let initialize_tex_html () =
  let if_utf8 = if !Cdglobals.utf8 then fun x -> Some x else fun _ -> None in
  let (tree_latex, tree_html) = List.fold_right (fun (s,l,l') (tt,tt') ->
      (Tokens.ttree_add tt s l,
       match l' with None -> tt' | Some l' -> Tokens.ttree_add tt' s l'))
      [ "*" ,  "\\ensuremath{\\times}", if_utf8 "×";
	"|", "\\ensuremath{|}", None;
	"->",  "\\ensuremath{\\rightarrow}", if_utf8 "→";
	"->~",  "\\ensuremath{\\rightarrow\\lnot}", None;
	"->~~",  "\\ensuremath{\\rightarrow\\lnot\\lnot}", None;
	"<-",  "\\ensuremath{\\leftarrow}", None;
	"<->", "\\ensuremath{\\leftrightarrow}", if_utf8 "↔";
	"=>",  "\\ensuremath{\\Rightarrow}", if_utf8 "⇒";
	"<=",  "\\ensuremath{\\le}", if_utf8 "≤";
	">=",  "\\ensuremath{\\ge}", if_utf8 "≥";
	"<>",  "\\ensuremath{\\not=}", if_utf8 "≠";
	"~",   "\\ensuremath{\\lnot}", if_utf8 "¬";
	"/\\", "\\ensuremath{\\land}", if_utf8 "∧";
	"\\/", "\\ensuremath{\\lor}", if_utf8 "∨";
	"|-",  "\\ensuremath{\\vdash}", None;
	"forall", "\\ensuremath{\\forall}", if_utf8 "∀";
	"exists", "\\ensuremath{\\exists}", if_utf8 "∃";
	"Π", "\\ensuremath{\\Pi}", if_utf8 "Π";
	"λ", "\\ensuremath{\\lambda}", if_utf8 "λ";
	(* "fun", "\\ensuremath{\\lambda}" ? *)
      ] (Tokens.empty_ttree,Tokens.empty_ttree) in
  token_tree_latex := tree_latex;
  token_tree_html  := tree_html

let add_printing_token s (t1,t2) =
  (match t1 with None -> () | Some t1 ->
    token_tree_latex := Tokens.ttree_add !token_tree_latex s t1);
  (match t2 with None -> () | Some t2 ->
    token_tree_html := Tokens.ttree_add !token_tree_html s t2)

let remove_printing_token s =
  token_tree_latex := Tokens.ttree_remove !token_tree_latex s;
  token_tree_html := Tokens.ttree_remove !token_tree_html s

(*s Table of contents *)

type toc_entry =
  | Toc_library of string * string option
  | Toc_section of int * (unit -> unit) * string

let (toc_q : toc_entry Queue.t) = Queue.create ()

let add_toc_entry e = Queue.add e toc_q

let new_label = let r = ref 0 in fun () -> incr r; "lab" ^ string_of_int !r

module type S = sig

(** XXX move to start_file  *)
val push_in_preamble : string -> unit

(** [support_files] List of support files to be copied along the output. *)
val support_files    : string list

(** [appendix toc index split_index standalone] Backend-specific
    function that outputs additional files. *)
val appendix : toc:bool -> index:bool -> split_index:bool -> standalone:bool -> unit

(** [start_file out toc index standalone] Start a logical output file
    to channel [out] [toc], [index], and [standalone] control whether
    the backend will generate a TOC, index, and header/trailers for the file.
*)
val start_file : Format.formatter -> toc:bool -> index:bool ->
                 split_index:bool -> standalone:bool -> unit

(** [end_file] Ends the file *)
val end_file : unit -> unit

(** [start_module mod] Starts a coq module. *)
val start_module : coq_module -> unit

(** [start_doc] Moves the backend to "document" mode. *)
val start_doc : unit -> unit
val end_doc : unit -> unit

val start_emph : unit -> unit
val stop_emph : unit -> unit

val start_comment : unit -> unit
val end_comment : unit -> unit

val start_coq : unit -> unit
val end_coq : unit -> unit

val start_inline_coq : unit -> unit
val end_inline_coq : unit -> unit

val start_inline_coq_block : unit -> unit
val end_inline_coq_block : unit -> unit

val indentation : int -> unit
val line_break : unit -> unit
val paragraph : unit -> unit
val empty_line_of_code : unit -> unit

val section : int -> (unit -> unit) -> unit

val item : int -> unit
val stop_item : unit -> unit
val reach_item_level : int -> unit

val rule : unit -> unit

val nbsp : unit -> unit
val char : char -> unit
val keyword : string -> loc -> unit
val ident : string -> loc option -> unit
val sublexer : char -> loc -> unit
val sublexer_in_doc : char -> unit

val proofbox : unit -> unit

val latex_char : char -> unit
val latex_string : string -> unit
val html_char : char -> unit
val html_string : string -> unit
val verbatim_char : bool -> char -> unit
val hard_verbatim_char : char -> unit

val start_latex_math : unit -> unit
val stop_latex_math : unit -> unit
val start_verbatim : bool -> unit
val stop_verbatim : bool -> unit
val start_quote : unit -> unit
val stop_quote : unit -> unit

val url : string -> string option -> unit

(* this outputs an inference rule in one go.  You pass it the list of
   assumptions, then the middle line info, then the conclusion (which
   is allowed to span multiple lines).

   In each case, the int is the number of spaces before the start of
   the line's text and the string is the text of the line with the
   leading trailing space trimmed.  For the middle rule, you can
   also optionally provide a name.

   We need the space info so that in modes where we aren't doing
   something smart we can just format the rule verbatim like the user did
*)
val inf_rule :  (int * string) list
             -> (int * string * (string option))
             -> (int * string) list
             -> unit

end

let inf_rule_dumb start_verbatim stop_verbatim char assumptions (midsp,midln,midnm) conclusions = 
  start_verbatim false;
  let dumb_line = 
       function (sp,ln) -> (String.iter char ((String.make sp ' ') ^ ln);
                            char '\n')
  in 
    (List.iter dumb_line assumptions;
     dumb_line (midsp, midln ^ (match midnm with 
                                | Some s -> " " ^ s 
                                | None -> ""));
     List.iter dumb_line conclusions);
  stop_verbatim false


(*s LaTeX output *)

module Latex : S = struct

  (* Private methods and values *)
  let oc              = ref (formatter_of_out_channel stdout)
  let printf        s = fprintf !oc s
  let output_char   c = printf "%c" c
  let output_string s = printf "%s" s

  let in_title = ref false

  let cur_mod = ref ""

  (*s Latex preamble *)

  let (preamble : string Queue.t) = Queue.create ()

  let utf8x_extra_support () =
    printf "\n";
    printf "%%Warning: tipa declares many non-standard macros used by utf8x to\n";
    printf "%%interpret utf8 characters but extra packages might have to be added\n";
    printf "%%such as \"textgreek\" for Greek letters not already in tipa\n";
    printf "%%or \"stmaryrd\" for mathematical symbols.\n";
    printf "%%Utf8 codes missing a LaTeX interpretation can be defined by using\n";
    printf "%%\\DeclareUnicodeCharacter{code}{interpretation}.\n";
    printf "%%Use coqdoc's option -p to add new packages or declarations.\n";
    printf "\\usepackage{tipa}\n";
    printf "\n"

  let header ~toc =
    if !header_trailer then begin
      printf "\\documentclass[12pt]{report}\n";
      if !inputenc != "" then printf "\\usepackage[%s]{inputenc}\n" !inputenc;
      if !inputenc = "utf8x" then utf8x_extra_support ();
      printf "\\usepackage[T1]{fontenc}\n";
      printf "\\usepackage{fullpage}\n";
      printf "\\usepackage{coqdoc}\n";
      printf "\\usepackage{amsmath,amssymb}\n";
      printf "\\usepackage{url}\n";
      (match !toc_depth with
       | None -> ()
       | Some n -> printf "\\setcounter{tocdepth}{%i}\n" n);
      Queue.iter (fun s -> printf "%s\n" s) preamble;
      printf "\\begin{document}\n"
    end;
    output_string
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
    output_string
      "%% This file has been automatically generated with the command\n";
    output_string "%% ";
    Array.iter (fun s -> printf "%s " s) Sys.argv;
    printf "\n";
    output_string
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
    if toc then printf "\\tableofcontents\n";
    ()

  let trailer () =
    if !header_trailer then begin
      printf "\\end{document}\n"
    end

  (*s Latex char escaping *)
  let escaped =
    let buff = Buffer.create 5 in
    fun s ->
      Buffer.clear buff;
      for i = 0 to String.length s - 1 do
	match s.[i] with
	| '\\' ->
	    Buffer.add_string buff "\\symbol{92}"
	| '$' | '#' | '%' | '&' | '{' | '}' | '_' as c ->
	    Buffer.add_char buff '\\'; Buffer.add_char buff c
	| '^' | '~' as c ->
	    Buffer.add_char buff '\\'; Buffer.add_char buff c;
	    Buffer.add_string buff "{}"
        | '\'' ->
            if i < String.length s - 1 && s.[i+1] = '\'' then begin
              Buffer.add_char buff '\''; Buffer.add_char buff '{';
              Buffer.add_char buff '}'
            end else
              Buffer.add_char buff '\''
	| c ->
	    Buffer.add_char buff c
      done;
      Buffer.contents buff

  let label_char c = match c with
    | '_' -> output_char ' '
    | '\\' | '$' | '#' | '%' | '&' | '{' | '}'
    | '^' | '~' -> printf "x%X" (Char.code c)
    | _ -> if c >= '\x80' then printf "x%X" (Char.code c) else output_char c

  let label_ident s =
    for i = 0 to String.length s - 1 do label_char s.[i] done

  let ident_ref m fid typ s =
    let id = if fid <> "" then (m ^ "." ^ fid) else m in
    match find_module m with
      | Local ->
	  if typ = Variable then
	    printf "\\coqdoc%s{%s}" (type_name typ) s
	  else
	    (printf "\\coqref{"; label_ident id;
	     printf "}{\\coqdoc%s{%s}}" (type_name typ) s)
      | External m when !externals ->
	  printf "\\coqexternalref{"; label_ident fid;
	  printf "}{%s}{\\coqdoc%s{%s}}" (escaped m) (type_name typ) s
      | External _ | Unknown ->
	  printf "\\coqdoc%s{%s}" (type_name typ) s

  let defref m id ty s =
    if ty <> Notation then
      (printf "\\coqdef{"; label_ident (m ^ "." ^ id);
       printf "}{%s}{\\coqdoc%s{%s}}" s (type_name ty) s)
    else
      (* Glob file still not able to say the exact extent of the definition *)
      (* so we currently renounce to highlight the notation location *)
      (printf "\\coqdef{"; label_ident (m ^ "." ^ id);
       printf "}{%s}{%s}" s s)

  let reference s = function
    | Def (fullid,typ) ->
      defref !cur_mod fullid typ s
    | Ref (m,fullid,typ) ->
	ident_ref m fullid typ s

  let output_sublexer_string doescape issymbchar tag s =
    let s = if doescape then escaped s else s in
    match tag with
    | Some ref -> reference s ref
    | None -> if issymbchar then output_string s else printf "\\coqdocvar{%s}" s

  (** Public values  *)

  let push_in_preamble s = Queue.add s preamble
  let support_files = ["coqdoc.sty"]

  let appendix ~toc ~index ~split_index ~standalone = ()

  (*  *)
  let start_file out ~toc ~index ~split_index ~standalone =
    oc := out;
    initialize_tex_html ();
    Tokens.token_tree := token_tree_latex;
    Tokens.outfun     := output_sublexer_string;
    header toc

  let end_file () =
    trailer ()

  (*s Latex low-level translation *)

  let nbsp () = output_char '~'

  let char c = match c with
    | '\\' ->
	printf "\\symbol{92}"
    | '$' | '#' | '%' | '&' | '{' | '}' | '_' ->
	output_char '\\'; output_char c
    | '^' | '~' ->
	output_char '\\'; output_char c; printf "{}"
    | _ ->
	output_char c

  let verbatim_char inline = if inline then char else output_char
  let hard_verbatim_char = output_char

  let latex_char = output_char
  let latex_string = output_string

  let html_char   _ = ()
  let html_string _ = ()

  (*s Latex reference and symbol translation *)
  let start_module coq_mod =
    let mod_name = coq_mod   in
    let ln       = !lib_name in
    cur_mod     := coq_mod;
    if not !short then begin
      printf "\\coqlibrary{";
      label_ident coq_mod;
      printf "}{";
      if ln <> "" then printf "%s " ln;
      printf "}{%s}\n\n" (escaped mod_name)
    end

  let start_latex_math () = output_char '$'

  let stop_latex_math () = output_char '$'

  let start_quote () = output_char '`'; output_char '`'
  let stop_quote () = output_char '\''; output_char '\''

  let start_verbatim inline =
    if inline then printf "\\texttt{"
    else printf "\\begin{verbatim}"

  let stop_verbatim inline =
    if inline then printf "}"
    else printf "\\end{verbatim}\n"

  let inf_rule = inf_rule_dumb start_verbatim stop_verbatim char

  let url addr name = 
    printf "%s\\footnote{\\url{%s}}"
      (match name with
       | None -> ""
       | Some n -> n)
      addr

  let indentation n =
    if n == 0 then
      printf "\\coqdocnoindent\n"
    else
      let space = 0.5 *. (float n) in
      printf "\\coqdocindent{%2.2fem}\n" space

  (*s The sublexer buffers symbol characters and attached
       uninterpreted ident and try to apply special translation such as,
       predefined, translation "->" to "\ensuremath{\rightarrow}" or,
       virtually, a user-level translation from "=_h" to "\ensuremath{=_{h}}" *)

  let last_was_in = ref false

  let sublexer c loc =
    if c = '*' && !last_was_in then begin
      Tokens.flush_sublexer ();
      output_char '*'
    end else begin
      let tag =
        try Some (Index.find !cur_mod loc) with Not_found -> None
      in
      Tokens.output_tagged_symbol_char tag c
    end;
    last_was_in := false

  let sublexer_in_doc c =
    if c = '*' && !last_was_in then begin
      Tokens.flush_sublexer ();
      output_char '*'
    end else
      Tokens.output_tagged_symbol_char None c;
    last_was_in := false

  (*s Interpreting ident with fallback on sublexer if unknown ident *)

  let translate s =
    match Tokens.translate s with Some s -> s | None -> escaped s

  let keyword s loc = 
    printf "\\coqdockw{%s}" (translate s)

  let ident s loc =
    last_was_in := s = "in";
    try
      match loc with
      | None -> raise Not_found
      | Some loc ->
          let tag = Index.find !cur_mod loc in
          reference (translate s) tag
    with Not_found ->
      if is_tactic s then
	printf "\\coqdoctac{%s}" (translate s)
      else if is_keyword s then
	printf "\\coqdockw{%s}" (translate s)
      else if !Cdglobals.interpolate && !in_doc (* always a var otherwise *)
      then
	try
	  let tag = Index.find_string !cur_mod s in
	  reference (translate s) tag
	with _ -> Tokens.output_tagged_ident_string s
      else Tokens.output_tagged_ident_string s

  let ident s l =
    if !in_title then (
      printf "\\texorpdfstring{\\protect";
      ident s l;
      printf "}{%s}" (translate s))
    else
      ident s l

  (*s Translating structure *)

  let proofbox () = printf "\\ensuremath{\\Box}"

  let rec reach_item_level n =
    if !item_level < n then begin
      printf "\n\\begin{itemize}\n\\item "; incr item_level;
      reach_item_level n
    end else if !item_level > n then begin
      printf "\n\\end{itemize}\n"; decr item_level;
      reach_item_level n
    end

  let item n =
    let old_level = !item_level in
    reach_item_level n;
    if n <= old_level then printf "\n\\item "

  let stop_item () = reach_item_level 0

  let start_doc () = in_doc := true

  let end_doc () = in_doc := false; stop_item ()

  (* This is broken if we are in math mode, but coqdoc currently isn't
     tracking that *)
  let start_emph () = printf "\\textit{"

  let stop_emph () = printf "}"

  let start_comment () = printf "\\begin{coqdoccomment}\n"

  let end_comment () = printf "\\end{coqdoccomment}\n"

  let start_coq () = printf "\\begin{coqdoccode}\n"

  let end_coq () = printf "\\end{coqdoccode}\n"

  let section_kind = function
    | 1 -> "\\section{"
    | 2 -> "\\subsection{"
    | 3 -> "\\subsubsection{"
    | 4 -> "\\paragraph{"
    | _ -> assert false

  let section lev f =
    stop_item ();
    output_string (section_kind lev);
    in_title := true; f (); in_title := false;
    printf "}\n\n"

  let rule () =
    printf "\\par\n\\noindent\\hrulefill\\par\n\\noindent{}"

  let paragraph () = printf "\n\n"

  let line_break () = printf "\\coqdoceol\n"

  let empty_line_of_code () = printf "\\coqdocemptyline\n"

  let start_inline_coq_block () = line_break (); empty_line_of_code ()

  let end_inline_coq_block () = empty_line_of_code ()

  let start_inline_coq () = ()

  let end_inline_coq () = ()

end


(*s HTML output *)

module Html : S = struct

  (* Private methods and values *)
  let finalizers : (unit -> unit) Queue.t = Queue.create ()

  let oc              = ref (formatter_of_out_channel stdout)
  let printf        s = fprintf !oc s
  let output_char   c = printf "%c" c
  let output_string s = printf "%s" s

  let page_title = ref ""
  let cur_mod = ref ""

  let header () =
    if !header_trailer then
    if !header_file_spec then
      let cin = Pervasives.open_in !header_file in
      try
        while true do
	  let s = Pervasives.input_line cin in
          printf "%s\n" s
	done
      with End_of_file -> Pervasives.close_in cin
      else
	begin
	  printf "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n";
	  printf "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n";
	  printf "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n";
	  printf "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=%s\" />\n" !charset;
	  printf "<link href=\"coqdoc.css\" rel=\"stylesheet\" type=\"text/css\" />\n";
	  printf "<title>%s</title>\n</head>\n\n" !page_title;
	  printf "<body>\n\n<div id=\"page\">\n\n<div id=\"header\">\n</div>\n\n";
	  printf "<div id=\"main\">\n\n"
	end

  let trailer () =
    if !header_trailer && !footer_file_spec then
	let cin = Pervasives.open_in !footer_file in
	  try
	    while true do
	      let s = Pervasives.input_line cin in
		printf "%s\n" s
	    done
	  with End_of_file -> Pervasives.close_in cin
    else
      begin
        if !index && !cur_mod <> "Index" then
          printf "</div>\n\n<div id=\"footer\">\n<hr/><a href=\"%s.html\">Index</a>" !index_name;
	printf "<hr/>This page has been generated by ";
	printf "<a href=\"%s\">udoc</a>\n" Cdglobals.wwwcoq;
	printf "</div>\n\n</div>\n\n</body>\n</html>"
      end

  let escaped =
    let buff = Buffer.create 5 in
    fun s ->
      Buffer.clear buff;
      for i = 0 to String.length s - 1 do
	match s.[i] with
	| '<' -> Buffer.add_string buff "&lt;"
	| '>' -> Buffer.add_string buff "&gt;"
	| '&' -> Buffer.add_string buff "&amp;"
        | '\"' -> Buffer.add_string buff "&quot;"
	| c -> Buffer.add_char buff c
      done;
      Buffer.contents buff

  let sanitize_name s =
    let rec loop esc i =
      if i < 0 then if esc then escaped s else s
      else match s.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' -> loop esc (i-1)
      | '<' | '>' | '&' | '\'' | '\"' -> loop true (i-1)
      | _ ->
        (* This name contains complex characters:
           this is probably a notation string, we simply hash it. *)
        Digest.to_hex (Digest.string s)
    in loop false (String.length s - 1)

  let ident_ref m fid typ s =
    match find_module m with
    | Local ->
	printf "<a class=\"idref\" href=\"%s.html#%s\">" m (sanitize_name fid);
	printf "<span class=\"id\" title=\"%s\">%s</span></a>" typ s
    | External m when !externals ->
	printf "<a class=\"idref\" href=\"%s.html#%s\">" m (sanitize_name fid);
	printf "<span class=\"id\" title=\"%s\">%s</span></a>" typ s
    | External _ | Unknown ->
	printf "<span class=\"id\" title=\"%s\">%s</span>" typ s

  let reference s r =
    match r with
    | Def (fullid,ty) ->
	printf "<a name=\"%s\">" (sanitize_name fullid);
	printf "<span class=\"id\" title=\"%s\">%s</span></a>" (type_name ty) s
    | Ref (m,fullid,ty) ->
	ident_ref m fullid (type_name ty) s

  let output_sublexer_string doescape issymbchar tag s =
    let s = if doescape then escaped s else s in
    match tag with
    | Some ref -> reference s ref
    | None ->
	if issymbchar then output_string s
	else printf "<span class=\"id\" title=\"var\">%s</span>" s

  (** Public methods  *)
  let push_in_preamble _ = ()

  let start_file out ~toc ~index ~split_index ~standalone =
    oc := out;
    initialize_tex_html();
    Tokens.token_tree := token_tree_html;
    Tokens.outfun := output_sublexer_string;
    header ()

  let end_file () =
    (* Queue add *)
    trailer ()

  let support_files = ["coqdoc.css"]

  let start_module coq_mod =
    let mod_name = coq_mod   in
    let ln       = !lib_name in
    cur_mod      := coq_mod;
    if not !short then begin
      let (m,sub) = mod_name, None in
      add_toc_entry (Toc_library (m,sub));
      if ln = ""  then
        printf "<h1 class=\"libtitle\">%s</h1>\n\n" mod_name
      else
        printf "<h1 class=\"libtitle\">%s %s</h1>\n\n" ln mod_name
    end

  let hard_verbatim_char = output_char

  let indentation n = for _i = 1 to n do printf "&nbsp;" done

  let line_break () = printf "<br/>\n"

  let empty_line_of_code () =
    printf "\n<br/>\n"

  let nbsp () = printf "&nbsp;"

  let char = function
    | '<' -> printf "&lt;"
    | '>' -> printf "&gt;"
    | '&' -> printf "&amp;"
    | c -> output_char c

  let verbatim_char inline = char

  let latex_char _ = ()
  let latex_string _ = ()

  let html_char = output_char
  let html_string = output_string

  let start_latex_math () = ()
  let stop_latex_math () = ()

  let start_quote () = char '"'
  let stop_quote () = start_quote ()

  let start_verbatim inline = 
    if inline then printf "<tt>"
    else printf "<pre>"

  let stop_verbatim inline = 
    if inline then printf "</tt>" 
    else printf "</pre>\n"

  let url addr name = 
    printf "<a href=\"%s\">%s</a>" addr 
      (match name with
       | Some n -> n
       | None -> addr)

  let sublexer c loc =
    let tag =
      try Some (Index.find !cur_mod loc) with Not_found -> None
    in
    Tokens.output_tagged_symbol_char tag c

  let sublexer_in_doc c =
    Tokens.output_tagged_symbol_char None c

  let translate s =
    match Tokens.translate s with Some s -> s | None -> escaped s

  let keyword s loc = 
    printf "<span class=\"id\" title=\"keyword\">%s</span>" (translate s)

  let ident s loc =
    if is_keyword s then begin
      printf "<span class=\"id\" title=\"keyword\">%s</span>" (translate s)
    end else begin
      try
        match loc with
        | None -> raise Not_found
        | Some loc ->
            reference (translate s) (Index.find !cur_mod loc)
      with Not_found ->
	if is_tactic s then
	  printf "<span class=\"id\" title=\"tactic\">%s</span>" (translate s)
	else
	  if !Cdglobals.interpolate && !in_doc (* always a var otherwise *)
	  then
	    try reference (translate s) (Index.find_string !cur_mod s)
	    with _ -> Tokens.output_tagged_ident_string s
	  else
	    Tokens.output_tagged_ident_string s
    end

  let proofbox () = printf "<font size=-2>&#9744;</font>"

  let rec reach_item_level n =
    if !item_level < n then begin
      printf "<ul class=\"doclist\">\n<li>"; incr item_level;
      reach_item_level n
    end else if !item_level > n then begin
      printf "\n</li>\n</ul>\n"; decr item_level;
      reach_item_level n
    end

  let item n =
    let old_level = !item_level in
    reach_item_level n;
    if n <= old_level then printf "\n</li>\n<li>"

  let stop_item () = reach_item_level 0

  let start_coq () = if not !raw_comments then printf "<div class=\"code\">\n"

  let end_coq () = if not !raw_comments then printf "</div>\n"

  let start_doc () = in_doc := true;
    if not !raw_comments then
      printf "\n<div class=\"doc\">\n"

  let end_doc () = in_doc := false;
    stop_item ();
    if not !raw_comments then printf "\n</div>\n"

  let start_emph () = printf "<i>"

  let stop_emph () = printf "</i>"

  let start_comment () = printf "<span class=\"comment\">(*"

  let end_comment () = printf "*)</span>"

  let start_inline_coq () = 
    if !inline_notmono then printf "<span class=\"inlinecodenm\">"
                       else printf "<span class=\"inlinecode\">"

  let end_inline_coq () = printf "</span>"

  let start_inline_coq_block () = line_break (); start_inline_coq ()

  let end_inline_coq_block () = end_inline_coq ()

  let paragraph () = printf "\n<div class=\"paragraph\"> </div>\n\n" 

  (* inference rules *)
  let inf_rule assumptions (_,_,midnm) conclusions =
    (* this first function replaces any occurance of 3 or more spaces
       in a row with "&nbsp;"s.  We do this to the assumptions so that
       people can put multiple rules on a line with nice formatting *)
    let replace_spaces str =
      let rec copy a n = match n with 0 -> [] | n -> (a :: copy a (n - 1)) in 
      let results = Str.full_split (Str.regexp "[' '][' '][' ']+") str in
      let strs = List.map (fun r -> match r with
                                    | Str.Text s  -> [s]
                                    | Str.Delim s -> 
                                        copy "&nbsp;" (String.length s))  
                          results
      in
        String.concat "" (List.concat strs)
    in
    let start_assumption line =
          (printf "<tr class=\"infruleassumption\">\n";
           printf "  <td class=\"infrule\">%s</td>\n" (replace_spaces line)) in
    let end_assumption () =
          (printf "  <td></td>\n";
           printf "</td>\n") in
    let rec print_assumptions hyps = 
          match hyps with
          | []                 -> start_assumption "&nbsp;&nbsp;"
          | [(_,hyp)]          -> start_assumption hyp
          | ((_,hyp) :: hyps') -> (start_assumption hyp;
                                   end_assumption ();
                                   print_assumptions hyps') in
    printf "<center><table class=\"infrule\">\n";
    print_assumptions assumptions;
    printf "  <td class=\"infrulenamecol\" rowspan=\"3\">\n";
    (match midnm with
     | None   -> printf "    &nbsp;\n  </td>" 
     | Some s -> printf "    %s &nbsp;\n  </td>" s);
    printf "</tr>\n";
    printf "<tr class=\"infrulemiddle\">\n";
    printf "  <td class=\"infrule\"><hr /></td>\n";
    printf "</tr>\n";
    print_assumptions conclusions;
    end_assumption ();
    printf "</table></center>"

  let section lev f =
    let lab = new_label () in
    let r = sprintf "%s.html#%s" !cur_mod lab in
    (match !toc_depth with
     | None -> add_toc_entry (Toc_section (lev, f, r))
     | Some n -> if lev <= n then add_toc_entry (Toc_section (lev, f, r))
                   else ());
    stop_item ();
    printf "<a name=\"%s\"></a><h%d class=\"section\">" lab lev;
    f ();
    printf "</h%d>\n" lev

  let rule () = printf "<hr/>\n"

  module Appendix = struct

    let make_toc out =
      oc := out;
      let ln = !lib_name in
      let make_toc_entry = function
        | Toc_library (m,sub) ->
 	  stop_item ();
	  let ms = match sub with | None -> m | Some s -> m ^ ": " ^ s in
          if ln = "" then
 	    printf "<a href=\"%s.html\"><h2>%s</h2></a>\n" m ms
          else
 	    printf "<a href=\"%s.html\"><h2>%s %s</h2></a>\n" m ln ms
        | Toc_section (n, f, r) ->
  	  item n;
  	  printf "<a href=\"%s\">" r; f (); printf "</a>\n"
      in
      page_title := (if !title <> "" then !title else "Table of contents");
      header ();
      if !title <> "" then printf "<h1>%s</h1>\n" !title;
      printf "<div id=\"toc\">\n";
      Queue.iter make_toc_entry toc_q;
      stop_item ();
      printf "</div>\n";
      trailer ()

    (* make a HTML index from a list of triples (name,text,link) *)
    let index_ref i c =
      let idxc = sprintf "%s_%c" i.idx_name c in
      !index_name ^ (if !multi_index then "_" ^ idxc ^ ".html" else ".html#" ^ idxc)

    let letter_index category idx (c,l) =
      if l <> [] then begin
        let cat = if category && idx <> "global" then "(" ^ idx ^ ")" else "" in
        printf "<a name=\"%s_%c\"></a><h2>%s %s</h2>\n" idx c (display_letter c) cat;
        List.iter
	  (fun (id,(text,link,t)) ->
	     let id' = prepare_entry id t in
	     printf "<a href=\"%s\">%s</a> %s<br/>\n" link id' text) l;
        printf "<br/><br/>"
      end

    let all_letters i = List.iter (letter_index false i.idx_name) i.idx_entries

    (* Construction d'une liste des index (1 index global, puis 1
       index par catégorie) *)
    let format_global_index =
      Index.map
        (fun s (m,t) ->
	   if t = Library then
             let ln = !lib_name in
             if ln <> "" then
	       "[" ^ String.lowercase ln ^ "]", m ^ ".html", t
             else
	       "[library]", m ^ ".html", t
	   else
	     sprintf "[%s, in <a href=\"%s.html\">%s</a>]" (type_name t) m m ,
	     sprintf "%s.html#%s" m (sanitize_name s), t)

    let format_bytype_index = function
      | Library, idx ->
	Index.map (fun id m -> "", m ^ ".html", Library) idx
      | (t,idx) ->
	Index.map
	  (fun s m ->
	     let text = sprintf "[in <a href=\"%s.html\">%s</a>]" m m in
	     (text, sprintf "%s.html#%s" m (sanitize_name s), t)) idx

    (* Impression de la table d'index *)
    let print_index_table_item i =
      printf "<tr>\n<td>%s Index</td>\n" (String.capitalize i.idx_name);
      List.iter
        (fun (c,l) ->
	   if l <> [] then
	     printf "<td><a href=\"%s\">%s</a></td>\n" (index_ref i c)
	       (display_letter c)
	   else
	     printf "<td>%s</td>\n" (display_letter c))
        i.idx_entries;
      let n = i.idx_size in
      printf "<td>(%d %s)</td>\n" n (if n > 1 then "entries" else "entry");
      printf "</tr>\n"

    let print_index_table idxl =
      printf "<table>\n";
      List.iter print_index_table_item idxl;
      printf "</table>\n"

    let make_one_multi_index prt_tbl i =
      (* Attn: make_one_multi_index crée un nouveau fichier... *)
      let idx = i.idx_name in
      let one_letter ((c,l) as cl) =
        with_outfile (sprintf "%s_%s_%c.html" !index_name idx c) (fun fmt ->
            oc := fmt;
            if (!header_trailer) then header ();
            prt_tbl (); printf "<hr/>";
            letter_index true idx cl;
            if List.length l > 30 then begin printf "<hr/>"; prt_tbl () end;
            if (!header_trailer) then trailer ()
          )
      in
      List.iter one_letter i.idx_entries

    let make_multi_index () =
      let all_index =
        let glob,bt = Index.all_entries () in
	(format_global_index glob) ::
	(List.map format_bytype_index bt) in
      let print_table () = print_index_table all_index in
      List.iter (make_one_multi_index print_table) all_index

    let make_index out =
      oc := out;
      let all_index =
        let glob,bt = Index.all_entries () in
	(format_global_index glob) ::
	(List.map format_bytype_index bt) in
      let print_table () = print_index_table all_index in
      let print_one_index i =
        if i.idx_size > 0 then begin
	  printf "<hr/>\n<h1>%s Index</h1>\n" (String.capitalize i.idx_name);
	  all_letters i
        end
      in
      cur_mod := "Index";
      if !title <> "" then printf "<h1>%s</h1>\n" !title;
      print_table ();
      if not (!multi_index) then
        begin
	  List.iter print_one_index all_index;
          printf "<hr/>"; print_table ()
        end
  end

  let appendix ~toc ~index ~split_index ~standalone =
    if toc         then with_outfile "toc.html"            Appendix.make_toc;
    if index       then with_outfile (!index_name^".html") Appendix.make_index;
    if split_index then Appendix.make_multi_index ()
end

(*s TeXmacs-aware output *)

module TeXmacs : S = struct

  (* Private methods and values *)
  let oc = ref stdout
  let output_char   c = Pervasives.output_char   !oc c
  let output_string s = Pervasives.output_string !oc s

  let printf s = Printf.fprintf !oc s

  (*s Latex preamble *)

  let (preamble : string Queue.t) =
    in_doc := false; Queue.create ()

  let appendix ~toc ~index ~split_index ~standalone = ()

  let start_file out ~toc ~index ~split_index ~standalone =
    ()

  let end_file () = ()

  let header () =
    output_string
      "(*i This file has been automatically generated with the command  \n";
    output_string
      "    "; Array.iter (fun s -> printf "%s " s) Sys.argv; printf " *)\n"

  let trailer () = ()

  let push_in_preamble _ = ()

  let support_files = []

  let nbsp () = output_char ' '

  let char_true c = match c with
    | '\\' -> printf "\\\\"
    | '<' -> printf "\\<"
    | '|' -> printf "\\|"
    | '>' -> printf "\\>"
    | _ -> output_char c

  let char c = if !in_doc then char_true c else output_char c
  let verbatim_char inline = char
  let hard_verbatim_char = output_char

  let latex_char = char_true
  let latex_string = String.iter latex_char

  let html_char _ = ()
  let html_string _ = ()

  let raw_ident s =
    for i = 0 to String.length s - 1 do char s.[i] done

  let start_module _coq_mod = ()

  let start_latex_math () = printf "<with|mode|math|"

  let stop_latex_math () = output_char '>'

  let start_verbatim inline = in_doc := true; printf "<\\verbatim>"
  let stop_verbatim inline = in_doc := false; printf "</verbatim>"

  let url addr name = 
    printf "%s<\\footnote><\\url>%s</url></footnote>" addr
      (match name with
       | None -> ""
       | Some n -> n)

  let start_quote () = output_char '`'; output_char '`'
  let stop_quote () = output_char '\''; output_char '\''

  let indentation n = ()

  let keyword s =
    printf "<kw|"; raw_ident s; printf ">"

  let ident_true s =
    if is_keyword s then keyword s
    else raw_ident s

  let keyword s loc = keyword s
  let ident s _ = if !in_doc then ident_true s else raw_ident s

  let output_sublexer_string doescape issymbchar tag s =
    if doescape then raw_ident s else output_string s

  let sublexer c l =
    if !in_doc then Tokens.output_tagged_symbol_char None c else char c

  let sublexer_in_doc c =
    char c

  let initialize () =
    Tokens.token_tree := token_tree_texmacs;
    Tokens.outfun := output_sublexer_string

  let proofbox () = printf "QED"

  let rec reach_item_level n =
    if !item_level < n then begin
      printf "\n<\\itemize>\n<item>"; incr item_level;
      reach_item_level n
    end else if !item_level > n then begin
      printf "\n</itemize>"; decr item_level;
      reach_item_level n
    end

  let item n =
    let old_level = !item_level in
    reach_item_level n;
    if n <= old_level then printf "\n\n<item>"

  let stop_item () = reach_item_level 0

  let start_doc () = in_doc := true; printf "(** texmacs: "

  let end_doc () = stop_item (); in_doc := false; printf " *)"

  let start_coq () = ()

  let end_coq () = ()

  let start_emph () = printf "<with|font shape|italic|"
  let stop_emph () = printf ">"

  let start_comment () = ()
  let end_comment () = ()

  let section_kind = function
    | 1 -> "section"
    | 2 -> "subsection"
    | 3 -> "subsubsection"
    | 4 -> "paragraph"
    | _ -> assert false

  let section lev f =
    stop_item ();
    printf "<"; output_string (section_kind lev); printf "|";
    f (); printf ">\n\n"

  let rule () =
    printf "\n<hrule>\n"

  let paragraph () = printf "\n\n"

  let line_break () = printf "\n"

  let empty_line_of_code () = printf "\n"

  let start_inline_coq () = printf "<verbatim|["

  let end_inline_coq () = printf "]>"

  let start_inline_coq_block () = line_break (); start_inline_coq ()

  let end_inline_coq_block () = end_inline_coq ()

  let inf_rule = inf_rule_dumb start_verbatim stop_verbatim char

end


(*s Raw output *)

module Raw : S = struct

  (* Private methods and values *)
  let oc = ref stdout
  let output_char   c = Pervasives.output_char   !oc c
  let output_string s = Pervasives.output_string !oc s
  let printf        s = Printf.fprintf !oc s

  let appendix ~toc ~index ~split_index ~standalone = ()

  let start_file out ~toc ~index ~split_index ~standalone =
    ()

  let end_file () = ()

  let start_module _coq_mod = ()

  let header  () = ()
  let trailer () = ()

  let push_in_preamble _ = ()

  let support_files = []

  let nbsp () = output_char ' '

  let char = output_char
  let verbatim_char inline = char
  let hard_verbatim_char = output_char

  let latex_char = output_char
  let latex_string = output_string

  let html_char _ = ()
  let html_string _ = ()

  let raw_ident s =
    for i = 0 to String.length s - 1 do char s.[i] done

  let end_module () = ()

  let start_latex_math () = ()
  let stop_latex_math () = ()

  let start_verbatim inline = ()
  let stop_verbatim inline = ()

  let inf_rule = inf_rule_dumb start_verbatim stop_verbatim char

  let url addr name = 
    match name with
    | Some n -> printf "%s (%s)" n addr
    | None -> printf "%s" addr

  let start_quote () = printf "\""
  let stop_quote () = printf "\""

  let indentation n =
      for _i = 1 to n do printf " " done

  let keyword s loc = raw_ident s
  let ident s loc = raw_ident s

  let sublexer c l = char c
  let sublexer_in_doc c = char c

  let initialize () =
    Tokens.token_tree := ref Tokens.empty_ttree;
    Tokens.outfun := (fun _ _ _ _ -> failwith "Useless")

  let proofbox () = printf "[]"

  let item n = printf "- "
  let stop_item () = ()
  let reach_item_level _ = ()

  let start_doc () = printf "(** "
  let end_doc () = printf " *)\n"

  let start_emph () = printf "_"
  let stop_emph () = printf "_"

  let start_comment () = printf "(*"
  let end_comment () = printf "*)"

  let start_coq () = ()
  let end_coq () = ()

  let section_kind =
    function
      | 1 -> "* "
      | 2 -> "** "
      | 3 -> "*** "
      | 4 -> "**** "
      | _ -> assert false

  let section lev f =
    output_string (section_kind lev);
    f ()

  let rule () = ()

  let paragraph () = printf "\n\n"

  let line_break () = printf "\n"

  let empty_line_of_code () = printf "\n"

  let start_inline_coq () = ()
  let end_inline_coq () = ()

  let start_inline_coq_block () = line_break (); start_inline_coq ()
  let end_inline_coq_block () = end_inline_coq ()

end
