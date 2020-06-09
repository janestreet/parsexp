open! Core_kernel
open! Import

let round_trip_prefix prefix ~len ~parser_input ~verbose =
  let prefix_signifier = Sexp_prefix.get_a_signifier prefix ~parser_input in
  let suffix = String.drop_prefix parser_input len in
  if verbose
  then
    print_s
      [%message (prefix : Sexp_prefix.t) (prefix_signifier : string) (suffix : string)];
  prefix_signifier ^ suffix
;;

module Sexp_signifier = struct
  type t = string [@@deriving sexp_of]

  let does_parse s = Parsexp.Many.parse_string s |> Result.is_ok

  let quickcheck_generator =
    Quickcheck.Generator.recursive_union
      [ [%quickcheck.generator: Atom_signifier.t] ]
      ~f:(fun self ->
        [ (match%bind.Quickcheck.Generator [%quickcheck.generator: bool] with
            | true -> self
            | false ->
              let%map.Quickcheck.Generator subsexps = Quickcheck.Generator.list self in
              String.concat ~sep:" " (List.concat [ [ "(" ]; subsexps; [ ")" ] ]))
        ])
    |> Quickcheck.Generator.filter ~f:does_parse
  ;;

  let quickcheck_shrinker =
    Quickcheck.Shrinker.filter [%quickcheck.shrinker: string] ~f:does_parse
  ;;

  module Compare_sexps = struct
    type nonrec t = t

    let compare =
      Comparable.lift [%compare: Sexp.t list] ~f:Parsexp.Many.parse_string_exn
    ;;
  end
end

let test_prefix state stack ~len ~parser_input ~verbose =
  Option.iter (Sexp_prefix.create state stack) ~f:(fun prefix ->
    let observed = round_trip_prefix prefix ~len ~parser_input ~verbose in
    if not ([%compare.equal: Sexp_signifier.Compare_sexps.t] parser_input observed)
    then raise_s [%message (len : int) (observed : string) (parser_input : string)])
;;

let test_input (saw_state : Coverage.Saw_state.t) parser_input =
  List.iter (state_after_every_prefix Many parser_input) ~f:(fun (len, state, stack) ->
    saw_state.f state;
    test_prefix state stack ~len ~parser_input ~verbose:false)
;;

(* Check that a value is unchanged after we round-trip some prefix of its string
   representation through [Prefix]. *)
let%expect_test _ =
  require_does_not_raise [%here] (fun () ->
    Coverage.with_state_coverage ~f:(fun saw_state ->
      Base_quickcheck.Test.run_exn (module Sexp_signifier) ~f:(test_input saw_state)));
  [%expect {| |}]
;;
