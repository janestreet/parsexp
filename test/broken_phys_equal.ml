open! Core

type t =
  { name : string
  ; numbers : int list
  ; opt : string option
  }
[@@deriving sexp, sexp_grammar]

let%expect_test "Returns the incorrect sexp" =
  let fill char length = String.init length ~f:(const char) in
  let string = sexp_of_t { name = ""; numbers = []; opt = None } |> Sexp.to_string_mach in
  let sexps, positions = Parsexp.Many_and_positions.parse_string_exn string in
  let sub =
    match sexps with
    | [ List [ _; _; List [ Atom "opt"; opt ] ] ] -> opt
    | _ -> assert false
  in
  let range =
    Parsexp.Positions.find_sub_sexp_in_list_phys positions sexps ~sub |> Option.value_exn
  in
  let underline =
    String.concat
      [ fill ' ' range.start_pos.col; fill '^' (range.end_pos.col - range.start_pos.col) ]
  in
  print_endline ("| " ^ string);
  print_endline ("| " ^ underline);
  [%expect {|
    | ((name"")(numbers())(opt()))
    |                         ^^
    |}];
  print_s [%sexp (range : Parsexp.Positions.range)];
  [%expect
    {|
    ((start_pos ((line 1) (col 24) (offset 24)))
     (end_pos ((line 1) (col 26) (offset 26))))
    |}]
;;
