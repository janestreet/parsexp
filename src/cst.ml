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
[@@deriving_inline sexp_of]

let rec sexp_of_t =
  (function
    | Atom { loc = v_loc; atom = v_atom; unescaped = v_unescaped } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_option sexp_of_string v_unescaped in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "unescaped"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_string v_atom in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "atom"; arg ] :: bnds
      in
      let bnds =
        let arg = Positions.sexp_of_range v_loc in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg ] :: bnds
      in
      Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Atom" :: bnds)
    | List { loc = v_loc; elements = v_elements } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_list sexp_of_t_or_comment v_elements in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "elements"; arg ] :: bnds
      in
      let bnds =
        let arg = Positions.sexp_of_range v_loc in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg ] :: bnds
      in
      Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "List" :: bnds)
      : t -> Sexplib0.Sexp.t)

and sexp_of_t_or_comment =
  (function
    | Sexp arg0__001_ ->
      let res0__002_ = sexp_of_t arg0__001_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Sexp"; res0__002_ ]
    | Comment arg0__003_ ->
      let res0__004_ = sexp_of_comment arg0__003_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Comment"; res0__004_ ]
      : t_or_comment -> Sexplib0.Sexp.t)

and sexp_of_comment =
  (function
    | Plain_comment { loc = v_loc; comment = v_comment } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_string v_comment in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "comment"; arg ] :: bnds
      in
      let bnds =
        let arg = Positions.sexp_of_range v_loc in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg ] :: bnds
      in
      Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Plain_comment" :: bnds)
    | Sexp_comment
        { hash_semi_pos = v_hash_semi_pos; comments = v_comments; sexp = v_sexp } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_t v_sexp in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "sexp"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_list sexp_of_comment v_comments in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "comments"; arg ] :: bnds
      in
      let bnds =
        let arg = Positions.sexp_of_pos v_hash_semi_pos in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "hash_semi_pos"; arg ] :: bnds
      in
      Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Sexp_comment" :: bnds)
      : comment -> Sexplib0.Sexp.t)
;;

[@@@end]

let compare = Caml.compare
let compare_t_or_comment = Caml.compare
let compare_comment = Caml.compare

module Forget = struct
  (* In cps to prevent non-tail recursion.
     The polymorphism in the signature ensures that each function returns
     only through the continuation. *)
  module Cps : sig
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
