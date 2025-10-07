@@ portable

(** Parser automaton *)

type u'
type s'
type 'a iarray := 'a Basement.Stdlib_iarray_labels.t

val transitions : Automaton_action.Poly.t iarray
val transitions_eoi : Automaton_action.Epsilon.Poly.t iarray
val old_parser_approx_cont_states : Old_parser_cont_state.t iarray
