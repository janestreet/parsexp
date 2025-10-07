open! Import

type t =
  | Atom of
      { loc : Positions.range
      ; atom : string
      ; unescaped : string option
      }
  | List of
      { loc : Positions.range
      ; elements : t_or_comment list
      }

and t_or_comment =
  | Sexp of t
  | Comment of comment

and comment =
  | Plain_comment of
      { loc : Positions.range
      ; comment : string
      }
  | Sexp_comment of
      { hash_semi_pos : Positions.pos
      ; comments : comment list
      ; sexp : t
      }
[@@deriving sexp_of]

let compare = Stdlib.compare
let compare_t_or_comment = Stdlib.compare
let compare_comment = Stdlib.compare

module Forget = struct
  (* In cps to prevent non-tail recursion.
     The polymorphism in the signature ensures that each function returns
     only through the continuation. *)
  module Cps : sig @@ portable
    val forget_t : t -> (Sexp.t -> 'r) -> 'r
    val forget_toc : t_or_comment -> (Sexp.t option -> 'r) -> 'r
    val forget_tocs : t_or_comment list -> (Sexp.t list -> 'r) -> 'r
  end = struct
    let rec forget_t t k =
      match t with
      | Atom { atom; _ } -> k (Sexp.Atom atom)
      | List { elements; _ } -> forget_tocs elements (fun xs -> k (Sexp.List xs))

    and forget_tocs tocs k =
      match tocs with
      | [] -> k []
      | toc :: tocs ->
        forget_toc toc (function
          | None -> forget_tocs tocs k
          | Some x -> forget_tocs tocs (fun xs -> k (x :: xs)))

    and forget_toc toc k =
      match toc with
      | Comment _ -> k None
      | Sexp t -> forget_t t (fun x -> k (Some x))
    ;;
  end

  let t x = Cps.forget_t x (fun y -> y)
  let t_or_comment x = Cps.forget_toc x (fun y -> y)
  let t_or_comments x = Cps.forget_tocs x (fun y -> y)
end
