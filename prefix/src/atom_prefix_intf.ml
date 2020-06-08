(** Preview incomplete atoms when the parser is in one

    This interface borrows the jargon of semiotics to distinguish between

    1. the signifier: the string we write when serializing a sexp
    2. the signified: the atom we mean to exist in the parsed OCaml value
*)

open! Import

module Signified = struct
  type t =
    | Complete of { prefix : string }
    (** [Complete] means [prefix] is all the signified from the start of the atom to
        the end of the input seen so far. *)
    | Incomplete of { prefix_of_prefix : string }
    (** [Incomplete] means the parser is uncertain how to handle some suffix of the input
        it has seen so far. E.g. midway through a backslash-escape sequence *)
  [@@deriving sexp_of]
end

module type Atom_prefix = sig
  module Signified = Signified

  type t [@@deriving sexp_of]

  (** [create state] returns [Some t] if the parser is known to be in an atom.

      [create] returns [None] when the parser is uncertain whether or not it is in an
      atom. For example, [#] not in double-quotes can be either an atom or a block
      comment. The parser must consume the next character to determine which. *)
  val create : (Positions.Builder.t, _) Automaton.t -> t option

  val get_signified : t -> Signified.t

  (** [get_signifier t ~parser_input] returns the substring of [parser_input]
      this [t] corresponds to, starting at the beginning of the partial atom,
      ending at the position the parser was at when [create] was called.

      [parser_input] should contain the entire document being
      parsed (or at least the prefix that was fed to the parser so far).

      [get_signifier] does not necessarily return a valid sexp: consider when the parser
      is partway through a quoted string. *)
  val get_signifier : t -> parser_input:string -> string
end
