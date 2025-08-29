open! Import

type t =
  | Parse_error of Parse_error.t
  | Of_sexp_error of Of_sexp_error.t
[@@deriving sexp_of]

(** Similar to [Parse_error.report] *)
val report : Format.formatter -> filename:string -> t -> unit
