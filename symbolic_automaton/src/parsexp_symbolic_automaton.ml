(** Abstract version of the parsing automaton.

    It is used in two places:

    - to define the automaton and generate parser code.
    - for tests

    At parser runtime, we instead use an integer for states and a table of
    functions for transitions
 **)

open! Base
module Parse_error_reason = Parse_error_reason
module State = State

