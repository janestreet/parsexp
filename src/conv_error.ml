open! Import

type t =
  | Parse_error of Parse_error.t
  | Of_sexp_error of Of_sexp_error.t
[@@deriving_inline sexp_of]

let sexp_of_t =
  (function
    | Parse_error v0 ->
      let v0 = Parse_error.sexp_of_t v0 in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Parse_error"; v0 ]
    | Of_sexp_error v0 ->
      let v0 = Of_sexp_error.sexp_of_t v0 in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Of_sexp_error"; v0 ]
      : t -> Sexplib0.Sexp.t)
;;

[@@@end]

let report ppf ~filename t =
  match t with
  | Parse_error e -> Parse_error.report ppf ~filename e
  | Of_sexp_error e -> Of_sexp_error.report ppf ~filename e
;;
