open Import
open Parsexp

let test s =
  Single.parse_string s
  |> [%sexp_of: (Sexp.t, Parse_error.t) Result.t]
  |> print_s
;;

let%expect_test "unterminated sexp" =
  test "(abc";
  [%expect {|
    (Error (
      (position (
        (line   1)
        (col    4)
        (offset 4)))
      (message "unclosed parentheses at end of input")))
  |}];
  test "  ";
  [%expect {|
    (Error (
      (position (
        (line   1)
        (col    2)
        (offset 2)))
      (message "no s-expression found in input")))
  |}]
;;

let%expect_test "parsexp bug: it accepts invalid syntax" =
  test "(#;)";
  [%expect {|
    (Error (
      (position (
        (line   1)
        (col    3)
        (offset 3)))
      (message "unterminated sexp comment"))) |}];
  test "#;(#;) a";
  [%expect {| (Ok a) |}];
;;

let%expect_test "regression test (used to raise s-expression followed by data)" =
  test "a #;0";
  [%expect {|
    (Ok a) |}];
;;

let parse_eager_cst ~no_sexp_is_error str =
  let r = ref [] in
  let state =
    Parsexp.Eager_cst.State.create ~no_sexp_is_error
      (fun _ v -> r := v :: !r)
  in
  Result.try_with (fun () ->
    let stack = Parsexp.Eager_cst.Stack.empty in
    let stack = Parsexp.Eager_cst.feed_string state str stack in
    Parsexp.Eager_cst.feed_eoi state stack;
    List.rev !r)
;;

let parse_eager_cst_no_sexp_is_error =
  parse_eager_cst ~no_sexp_is_error:true

let parse_eager_cst =
  parse_eager_cst  ~no_sexp_is_error:false

let%expect_test "regression test (we didn't run the callback on comments, resulting in \
                 missing comments or assertion failures)" =
  let test s =
    parse_eager_cst s
    |> [%sexp_of: (Cst.t_or_comment list, exn) Result.t]
    |> print_s
  in
  test ";";
  [%expect {|
    (Ok ((
      Comment (
        Plain_comment
        (loc (
          (start_pos (
            (line   1)
            (col    0)
            (offset 0)))
          (end_pos (
            (line   1)
            (col    1)
            (offset 1)))))
        (comment ";"))))) |}];
  test "#||#";
  [%expect {|
    (Ok ((
      Comment (
        Plain_comment
        (loc (
          (start_pos (
            (line   1)
            (col    0)
            (offset 0)))
          (end_pos (
            (line   1)
            (col    4)
            (offset 4)))))
        (comment "#||#"))))) |}];
  test "#;#;a b";
  [%expect {|
    (Ok ((
      Comment (
        Sexp_comment
        (hash_semi_pos (
          (line   1)
          (col    0)
          (offset 0)))
        (comments ((
          Sexp_comment
          (hash_semi_pos (
            (line   1)
            (col    2)
            (offset 2)))
          (comments ())
          (sexp (
            Atom
            (loc (
              (start_pos (
                (line   1)
                (col    4)
                (offset 4)))
              (end_pos (
                (line   1)
                (col    5)
                (offset 5)))))
            (atom a)
            (unescaped ()))))))
        (sexp (
          Atom
          (loc (
            (start_pos (
              (line   1)
              (col    6)
              (offset 6)))
            (end_pos (
              (line   1)
              (col    7)
              (offset 7)))))
          (atom b)
          (unescaped ()))))))) |}];
  test "a;\nb";
  [%expect {|
    (Ok (
      (Sexp (
        Atom
        (loc (
          (start_pos (
            (line   1)
            (col    0)
            (offset 0)))
          (end_pos (
            (line   1)
            (col    1)
            (offset 1)))))
        (atom a)
        (unescaped ())))
      (Comment (
        Plain_comment
        (loc (
          (start_pos (
            (line   1)
            (col    1)
            (offset 1)))
          (end_pos (
            (line   1)
            (col    2)
            (offset 2)))))
        (comment ";")))
      (Sexp (
        Atom
        (loc (
          (start_pos (
            (line   2)
            (col    0)
            (offset 3)))
          (end_pos (
            (line   2)
            (col    1)
            (offset 4)))))
        (atom b)
        (unescaped ()))))) |}];
;;

let%expect_test
  "regression test (we counted comments as sexps for the purpose of ~no_sexp_is_error" =
  let test s =
    parse_eager_cst_no_sexp_is_error s
    |> [%sexp_of: (Cst.t_or_comment list, exn) Result.t]
    |> print_s
  in
  test ";";
  [%expect {|
    (Error (
      parser_automaton_internal.ml.Public.Parse_error
      ((position (
         (line   1)
         (col    1)
         (offset 1)))
       (message "no s-expression found in input")))) |}];
  test "#||#";
  [%expect {|
    (Error (
      parser_automaton_internal.ml.Public.Parse_error
      ((position (
         (line   1)
         (col    4)
         (offset 4)))
       (message "no s-expression found in input")))) |}];
  test "#;#;a b";
  [%expect {|
    (Error (
      parser_automaton_internal.ml.Public.Parse_error
      ((position (
         (line   1)
         (col    7)
         (offset 7)))
       (message "no s-expression found in input")))) |}]
;;

module Stream = Caml.Stream

module P = Eager

let rec hot_loop state stream stack =
  match Stream.peek stream with
  | None -> P.feed_eoi state stack
  | Some char ->
    let stack = P.feed state char stack in
    Stream.junk stream;
    hot_loop state stream stack

exception Got_sexp of Sexp.t

let fetch_sexp (stream : char Stream.t) =
  let got_sexp _state sexp =
    Exn.raise_without_backtrace (Got_sexp sexp)
  in
  let count = Stream.count stream in
  let state = P.State.create got_sexp in
  match hot_loop state stream P.Stack.empty with
  | () -> None
  | exception (Got_sexp sexp) ->
    (* This test is true if the s-expression includes the last character passed to
       the parser *)
    if P.State.offset state > Stream.count stream - count then Stream.junk stream;
    Some sexp

let iter_sexps (stream : char Stream.t) ~f =
  let got_sexp _state sexp = f sexp in
  let state = P.State.create got_sexp in
  hot_loop state stream P.Stack.empty

let input = {|
(Hello World)
(a b c)
"Hello world"
(1 (2 3))
|}

let%expect_test "eager parser raise" =
  let stream = Stream.of_string input in
  let rec loop stream =
    match fetch_sexp stream with
    | None -> assert (Option.is_none (Stream.peek stream))
    | Some sexp -> Caml.Format.printf "got: %a@." Sexp.pp_hum sexp; loop stream
  in
  loop stream;
  [%expect {|
    got: (Hello World)
    got: (a b c)
    got: "Hello world"
    got: (1 (2 3)) |}]
;;

let%expect_test "eager parser continue" =
  let stream = Stream.of_string input in
  iter_sexps stream ~f:(Caml.Format.printf "got: %a@." Sexp.pp_hum);
  [%expect{|
    got: (Hello World)
    got: (a b c)
    got: "Hello world"
    got: (1 (2 3))
  |}]
;;

let%expect_test "eager parser incorrect mutation" =
  let stream = Stream.of_string input in
  let state = ref (P.State.create (fun _ _ -> assert false)) in
  let got_sexp _state _sexp = P.State.reset !state in
  state := P.State.create got_sexp;
  show_raise (fun () ->
    hot_loop !state stream P.Stack.empty);
  [%expect {|
    (raised "Assert_failure parser_automaton_internal.ml:*:*") (glob)
  |}]
;;

let%expect_test "eager parser feed after raise without reset" =
  let stream = Stream.of_string input in
  let got_sexp _state _sexp = raise Caml.Exit in
  let state = P.State.create got_sexp in
  (try
     hot_loop state stream P.Stack.empty
   with Caml.Exit ->
     ());
  show_raise (fun () ->
    hot_loop state stream P.Stack.empty);
  [%expect {|
    (raised (Failure "Parsexp.Parser_automaton: parser is dead"))
  |}]
;;
