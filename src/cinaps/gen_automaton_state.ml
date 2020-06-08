open! Base
open! Import

let print_constants () =
  let initial_int = Parsexp_symbolic_automaton.State.(to_int initial) in
  let error_int = Parsexp_symbolic_automaton.State.to_int Error in
  Stdio.print_endline
    [%string
      {|
let initial_state = %{initial_int#Int}
let error_state = %{error_int#Int}
|}]
;;
