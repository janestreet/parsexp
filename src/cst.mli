(** Concrete syntax tree of s-expressions *)

(** This module exposes a type that describe the full contents of a source file containing
    s-expressions.

    One can use this type to do low-level rewriting of s-expression files.
*)

open! Base

type t =
  | Atom of
      { loc       : Positions.range
      ; atom      : string
      ; (** Original unescaped atom, in case it is a double-quoted atom. The string
            includes the enclosing double quotes. *)
        unescaped : string option
      }
  | List of
      { loc      : Positions.range
      ; elements : t_or_comment list
      }

and t_or_comment =
  | Sexp    of t
  | Comment of comment

and comment =
  | Plain_comment of
      { loc     : Positions.range
      ; comment : string
      } (** Line or block comment *)
  | Sexp_comment  of
      { hash_semi_pos : Positions.pos
      ; comments      : comment list
      ; sexp          : t
      }
[@@deriving compare, sexp_of]

module Forget : sig
  val t             : t                 -> Sexp.t
  val t_or_comment  : t_or_comment      -> Sexp.t option
  val t_or_comments : t_or_comment list -> Sexp.t list
end
