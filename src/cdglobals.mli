(* Glocal uDoc variables *)

val version      : string
val compile_date : string
val wwwstdlib    : string
val wwwcoq       : string

(* uDoc Command line options: *)

type output_t = StdOut | MultFiles | File of string
type glob_source_t = NoGlob | DotGlob | GlobFile of string

type coq_module = string
type file = Vernac_file of string * coq_module

(* Path handling functions *)
val normalize_path     : string -> string
val normalize_filename : string -> string * string

(** [coqdoc_out f] locates output file *)
val coqdoc_out  : string -> string

(** [with_outfile s f] opens a file named [s] and calls [f out] where
    [out] is the file descriptor *)
val with_outfile : string -> (Format.formatter -> unit) -> unit

(* Global options *)
val out_to      : output_t ref
val output_dir  : string   ref
val glob_source : glob_source_t ref

type uoptions = {
  (* Title of the document *)
  title : string;

  (* Index/Toc options *)
  index       : bool;
  index_name  : string;
  multi_index : bool;
  toc         : bool;
  toc_depth   : int option;

  (* Header/Footer *)
  header_trailer   : bool;

  header_file      : string;
  header_file_spec : bool;
  footer_file      : string;
  footer_file_spec : bool;
}

val opts : uoptions ref

(* Stdlib url/path *)
val coqlib_url : string ref
val udoc_path  : string ref
