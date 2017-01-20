open Import
open Parsexp

let%expect_test "unterminated sexp" =
  let test s =
    Single.parse_string s
    |> [%sexp_of: (Sexp.t, Parse_error.t) Result.t]
    |> print_s
  in
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
