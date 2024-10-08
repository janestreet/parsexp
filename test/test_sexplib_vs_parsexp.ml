open! Import

(* Compare parsexp to the default cpp based sexplib parser *)

let test s =
  let sexplib = Sexplib.Sexp.of_string (String.strip s) in
  let parsexp = Single.parse_string_exn s in
  if Sexp.equal sexplib parsexp
  then (
    Stdlib.print_string "same\n";
    print_s [%sexp (parsexp : Sexp.t)])
  else (
    Stdlib.print_string "FAILURE\n";
    print_s [%sexp ~~(sexplib : Sexp.t)];
    print_s [%sexp ~~(parsexp : Sexp.t)]);
  let sexplib_lexer = Sexplib.Sexp.scan_sexp (Lexing.from_string s) in
  if not (Sexp.equal sexplib sexplib_lexer)
  then (
    Stdlib.print_string
      "\nNote: the various sexplib parsers disagree between themselves\n";
    print_s [%sexp ~~(sexplib_lexer : Sexp.t)])
;;

let%expect_test _ =
  test {| "a\q" |};
  [%expect
    {|
    same
    "a\\q"
    |}]
;;

let%expect_test _ =
  test {| "a\ b" |};
  [%expect
    {|
    same
    "a\\ b"

    Note: the various sexplib parsers disagree between themselves
    (sexplib_lexer "a b")
    |}]
;;

let test_cont_state input =
  let sexplib : Sexp.t =
    try
      match Sexplib.Sexp.parse input with
      | Done (sexp, _) -> [%sexp Done (sexp : Sexp.t)]
      | Cont (st, _) -> [%sexp Cont (Sexplib.Sexp.Cont_state.to_string st : string)]
    with
    | _ -> [%sexp Raised]
  in
  let parsexp =
    let state = Private.Automaton.create Single Sexp in
    ignore
      (String.fold input ~init:Private.Automaton.Stack.empty ~f:(fun stack ch ->
         Private.Automaton.feed state ch stack)
       : Private.Automaton.Stack.t);
    let old_state = Private.Automaton.old_parser_cont_state state in
    [%sexp Cont (old_state : Old_parser_cont_state.t)]
  in
  if Sexp.equal sexplib parsexp
  then (
    Stdlib.print_string "same\n";
    print_s [%sexp (parsexp : Sexp.t)])
  else (
    Stdlib.print_string "FAILURE\n";
    print_s [%sexp ~~(sexplib : Sexp.t)];
    print_s [%sexp ~~(parsexp : Sexp.t)])
;;

let%expect_test _ =
  test_cont_state " ";
  [%expect
    {|
    same
    (Cont Parsing_toplevel_whitespace)
    |}];
  test_cont_state "\r";
  [%expect
    {|
    same
    (Cont Parsing_nested_whitespace)
    |}];
  test_cont_state "\"toto";
  [%expect
    {|
    same
    (Cont Parsing_atom)
    |}];
  test_cont_state "#";
  [%expect
    {|
    same
    (Cont Parsing_atom)
    |}];
  test_cont_state "#| toto";
  [%expect
    {|
    same
    (Cont Parsing_block_comment)
    |}];
  test_cont_state "#| \"bla";
  [%expect
    {|
    same
    (Cont Parsing_block_comment)
    |}];
  test_cont_state "#; toto";
  [%expect
    {|
    same
    (Cont Parsing_sexp_comment)
    |}];
  test_cont_state "; toto";
  [%expect
    {|
    same
    (Cont Parsing_toplevel_whitespace)
    |}];
  test_cont_state "(";
  [%expect
    {|
    same
    (Cont Parsing_list)
    |}];
  test_cont_state "#; (";
  [%expect
    {|
    same
    (Cont Parsing_sexp_comment)
    |}];
  test_cont_state "#; #|";
  [%expect
    {|
    same
    (Cont Parsing_sexp_comment)
    |}];
  test_cont_state "#;\r";
  [%expect
    {|
    same
    (Cont Parsing_sexp_comment)
    |}]
;;
