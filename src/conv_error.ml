open! Import

type t =
  | Parse_error of Parse_error.t
  | Of_sexp_error of Of_sexp_error.t
[@@deriving sexp_of]

let report ppf ~filename t =
  match t with
  | Parse_error e -> Parse_error.report ppf ~filename e
  | Of_sexp_error e -> Of_sexp_error.report ppf ~filename e
;;
