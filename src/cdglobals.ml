(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)


let version      = "u1"
let compile_date = ""
let wwwstdlib    = "@inria"
let wwwcoq       = "who knows"
let coqlib       = None

(*s Output options *)

type target_language = HTML | JsCoq | Debug

let target_language = ref HTML

type output_t =
  | StdOut
  | MultFiles
  | File of string

let output_dir = ref ""
let out_to     = ref MultFiles

let ( / ) = Filename.concat

let coqdoc_out f =
  if !output_dir <> "" && Filename.is_relative f then
    if not (Sys.file_exists !output_dir) then
      (Printf.eprintf "No such directory: %s\n" !output_dir; exit 1)
    else
      !output_dir / f
  else
    f

let with_outfile file f =
  try
    let out = open_out (coqdoc_out file) in
    f (Format.formatter_of_out_channel out);
    close_out out
  with | Sys_error s -> Printf.eprintf "%s\n" s; exit 1

type glob_source_t =
    | NoGlob
    | DotGlob
    | GlobFile of string

let glob_source = ref DotGlob

(*s Manipulations of paths and path aliases *)

let normalize_path p =
  (* We use the Unix subsystem to normalize a physical path (relative
     or absolute) and get rid of symbolic links, relative links (like
     ./ or ../ in the middle of the path; it's tricky but it
     works... *)
  (* Rq: Sys.getcwd () returns paths without '/' at the end *)
  let orig = Sys.getcwd () in
  Sys.chdir p;
  let res = Sys.getcwd () in
  Sys.chdir orig;
  res

let normalize_filename f =
  let basename = Filename.basename f in
  let dirname = Filename.dirname f in
  normalize_path dirname, basename



(** A weaker analog of the function in Envars *)

let udoc_dft_path () =
  normalize_path (Filename.dirname Sys.executable_name)

let header_trailer = ref true
let header_file = ref ""
let header_file_spec = ref false
let footer_file = ref ""
let footer_file_spec = ref false
let quiet = ref true
let light = ref false
let gallina = ref false
let short = ref false
let index = ref true
let multi_index = ref false
let index_name = ref "index"
let toc = ref false
let title = ref ""
let externals = ref true
let coqlib_url  = ref wwwstdlib
let udoc_path  = ref (udoc_dft_path ())
let raw_comments = ref false
let parse_comments = ref false
let plain_comments = ref false
let toc_depth = (ref None : int option ref)
let lib_name = ref "Library"
let interpolate = ref false
let inline_notmono = ref false

let charset = ref "iso-8859-1"
let inputenc = ref ""
let latin1 = ref false
let utf8 = ref false

let set_latin1 () =
  charset := "iso-8859-1";
  inputenc := "latin1";
  latin1 := true

let set_utf8 () =
  charset := "utf-8";
  inputenc := "utf8x";
  utf8 := true

(* Parsing options *)

type coq_module = string

type file =
  | Vernac_file of string * coq_module
