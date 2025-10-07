@@ portable

open! Import

type t =
  | Parsing_toplevel_whitespace
  | Parsing_nested_whitespace
  | Parsing_atom
  | Parsing_list
  | Parsing_sexp_comment
  | Parsing_block_comment
[@@deriving sexp_of]

val to_string : t -> string
