open! Import

module type Conv = Conv.S
module type Parser = Parser.S
module type Eager_parser = Parser.S_eager

module Conv_error = Conv_error
module Of_sexp_error = Of_sexp_error
module Old_parser_cont_state = Old_parser_cont_state
module Parse_error = Parse_error
module Positions = Positions
module Cst = Cst
module A = Automaton

exception Parse_error = Parse_error.Parse_error
exception Of_sexp_error = Of_sexp_error.Of_sexp_error

module Single =
  (val Parser.make Sexp (fun () -> Single) (fun _ -> Automaton_stack.get_single))

module Many = (val Parser.make Sexp (fun () -> Many) (fun _ -> Automaton_stack.get_many))
module Eager = (val Parser.make_eager Sexp (fun _ -> Automaton_stack.get_single))

let and_get_positions get_sexp state stack = get_sexp stack, A.positions state

let and_positions mode get_sexp =
  Parser.make Sexp_with_positions mode (and_get_positions get_sexp)
;;

module Single_and_positions =
  (val and_positions (fun () -> Single) Automaton_stack.get_single)

module Many_and_positions = (val and_positions (fun () -> Many) Automaton_stack.get_many)

module Eager_and_positions =
  (val Parser.make_eager
         Sexp_with_positions
         (Automaton_stack.get_single |> and_get_positions))

let just_get_positions state () = A.positions state
let just_positions mode = Parser.make Positions mode just_get_positions

module Single_just_positions = (val just_positions (fun () -> Single))
module Many_just_positions = (val just_positions (fun () -> Many))
module Eager_just_positions = (val Parser.make_eager Positions just_get_positions)

let cst mode f = Parser.make Cst mode (fun _ -> f)

module Many_cst = (val cst (fun () -> Many) Automaton_stack.For_cst.get_many)

module Eager_cst =
  (val Parser.make_eager Cst (fun _ stack ->
         match Automaton_stack.For_cst.get_many stack with
         | [ sexp ] -> sexp
         | _ -> assert false))

type 'a id = 'a
type sexp_list = Sexp.t list

module Conv_single =
  Conv.Make
    (struct
      type 'a res = 'a
      type parsed_sexp = Sexp.t
      type chunk_to_conv = Sexp.t

      let apply_f x ~f = f x
      let find = Positions.find_sub_sexp_phys
    end)
    (Single)
    (Single_just_positions)

module Conv_many =
  Conv.Make
    (struct
      type 'a res = 'a list
      type parsed_sexp = Sexp.t list
      type chunk_to_conv = Sexp.t

      let apply_f x ~f = List.rev (List.rev_map x ~f)
      let find = Positions.find_sub_sexp_in_list_phys
    end)
    (Many)
    (Many_just_positions)

module Conv_many_and_locations =
  Conv.Make
    (struct
      type 'a res = 'a list
      type parsed_sexp = Sexp.t list * Positions.t
      type chunk_to_conv = Sexp.t * Positions.range

      let find positions (s, _) ~sub =
        Positions.find_sub_sexp_in_list_phys positions s ~sub
      ;;

      let apply_f (sexps, positions) ~f =
        let iter = Positions.Iterator.create positions in
        List.rev
          (List.rev_map sexps ~f:(fun sexp ->
             let location = Positions.Iterator.advance_sexp_exn iter sexp in
             f (sexp, location)))
      ;;
    end)
    (Many_and_positions)
    (Many_just_positions)

module Conv_many_at_once =
  Conv.Make
    (struct
      type 'a res = 'a
      type parsed_sexp = Sexp.t list
      type chunk_to_conv = Sexp.t list

      let apply_f x ~f = f x
      let find = Positions.find_sub_sexp_in_list_phys
    end)
    (Many)
    (Many_just_positions)

module Private = struct
  module Automaton = Automaton
  module Automaton_stack = Automaton_stack
  module Automaton_state = Automaton_state
  module Positions = Positions
end
