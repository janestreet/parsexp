open! Import

type t =
  | Parsing_toplevel_whitespace
  | Parsing_nested_whitespace
  | Parsing_atom
  | Parsing_list
  | Parsing_sexp_comment
  | Parsing_block_comment
[@@deriving sexp_of]

let to_string t =
  match sexp_of_t t with
  | Atom s -> s
  | List _ -> failwith "BUG: [sexp_of_t] returned a [List _]"
;;
