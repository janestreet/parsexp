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
module A = Parser_automaton

exception Parse_error = Parse_error.Parse_error
exception Of_sexp_error = Of_sexp_error.Of_sexp_error

module Single = Parser.Make (struct
    type parsed_value = Sexp.t
    type stack = A.stack
    type state = unit

    let kind = A.Sexp
    let mode = A.Single
    let empty = A.empty_stack
    let make_value _ stack = A.sexp_of_stack stack
  end)

module Many = Parser.Make (struct
    type parsed_value = Sexp.t list
    type stack = A.stack
    type state = unit

    let kind = A.Sexp
    let mode = A.Many
    let empty = A.empty_stack
    let make_value _ stack = A.sexps_of_stack stack
  end)

module Eager = Parser.Make_eager (struct
    type parsed_value = Sexp.t
    type stack = A.stack
    type state = unit

    let kind = A.Sexp
    let empty = A.empty_stack
    let make_value _ stack = A.sexp_of_stack stack
  end)

module Single_and_positions = Parser.Make (struct
    type parsed_value = Sexp.t * Positions.t
    type stack = A.stack
    type state = Positions.Builder.t

    let kind = A.Sexp_with_positions
    let mode = A.Single
    let empty = A.empty_stack
    let make_value state stack = A.sexp_of_stack stack, A.positions state
  end)

module Many_and_positions = Parser.Make (struct
    type parsed_value = Sexp.t list * Positions.t
    type stack = A.stack
    type state = Positions.Builder.t

    let kind = A.Sexp_with_positions
    let mode = A.Many
    let empty = A.empty_stack
    let make_value state stack = A.sexps_of_stack stack, A.positions state
  end)

module Eager_and_positions = Parser.Make_eager (struct
    type parsed_value = Sexp.t * Positions.t
    type stack = A.stack
    type state = Positions.Builder.t

    let kind = A.Sexp_with_positions
    let empty = A.empty_stack
    let make_value state stack = A.sexp_of_stack stack, A.positions state
  end)

module Single_just_positions = Parser.Make (struct
    type parsed_value = Positions.t
    type stack = unit
    type state = Positions.Builder.t

    let kind = A.Positions
    let mode = A.Single
    let empty = ()
    let make_value state () = A.positions state
  end)

module Many_just_positions = Parser.Make (struct
    type parsed_value = Positions.t
    type stack = unit
    type state = Positions.Builder.t

    let kind = A.Positions
    let mode = A.Many
    let empty = ()
    let make_value state () = A.positions state
  end)

module Eager_just_positions = Parser.Make_eager (struct
    type parsed_value = Positions.t
    type stack = unit
    type state = Positions.Builder.t

    let kind = A.Positions
    let empty = ()
    let make_value state () = A.positions state
  end)

module Many_cst = Parser.Make (struct
    type parsed_value = Cst.t_or_comment list
    type stack = A.stack_cst
    type state = A.state_cst

    let kind = A.Cst
    let mode = A.Many
    let empty = A.empty_stack_cst
    let make_value _ stack = A.sexps_cst_of_stack stack
  end)

module Eager_cst = Parser.Make_eager (struct
    type parsed_value = Cst.t_or_comment
    type stack = A.stack_cst
    type state = A.state_cst

    let kind = A.Cst
    let empty = A.empty_stack_cst

    let make_value _ stack =
      match A.sexps_cst_of_stack stack with
      | [ sexp ] -> sexp
      | _ -> assert false
    ;;
  end)

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
  module Parser_automaton = Parser_automaton
end
