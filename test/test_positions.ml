open! Import

let cases =
  (* Cases of inputs, [.] represent a saved position, [:] represent a position saved
     twice. *)
  [ ""
  ; "."
  ; "1."
  ; "12."
  ; "123."
  ; "1234."
  ; "12345."
  ; "123456."
  ; ".\n"
  ; "\n."
  ; "\n\n\n"
  ; ". . . xxxxx\nxxxx . xxx . xxx\n"
  ; "....."
  ; ":"
  ; "..:.."
  ; Printf.sprintf "%*s." 40 ""
  ; Printf.sprintf "%*s." 300 ""
  ; String.concat (List.init (62 * 4) ~f:(fun _i -> ".."))
  ]
;;

(* Extract the positions of the '.' in the input *)
let build_positions s =
  let builder = Positions.Builder.create () in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\n' -> Positions.Builder.add_newline builder ~offset:i
    | '.' -> Positions.Builder.add builder ~offset:i
    | ':' -> Positions.Builder.add_twice builder ~offset:i
    | _ -> ()
  done;
  Positions.Builder.contents builder
;;

(* Same but with a trivial implementation (i.e. without using [Positions]). *)
let build_positions_simple s =
  let rec loop i (pos : Positions.pos) =
    if i = String.length s
    then []
    else (
      let offset = pos.offset + 1 in
      match s.[i] with
      | '\n' -> loop (i + 1) { line = pos.line + 1; col = 0; offset }
      | '.' -> pos :: loop (i + 1) { pos with offset; col = pos.col + 1 }
      | ':' -> pos :: pos :: loop (i + 1) { pos with offset; col = pos.col + 1 }
      | _ -> loop (i + 1) { pos with offset; col = pos.col + 1 })
  in
  loop 0 Positions.beginning_of_file
;;

let%expect_test "build_positions_simple" =
  let f s = print_s [%sexp (build_positions_simple s : Positions.pos list)] in
  f "";
  [%expect {| () |}];
  f ".";
  [%expect
    {|
    ((
      (line   1)
      (col    0)
      (offset 0)))
    |}];
  f ". .";
  [%expect
    {|
    (((line 1) (col 0) (offset 0))
     ((line 1) (col 2) (offset 2)))
    |}];
  f ". xxx \n xxx .";
  [%expect
    {|
    (((line 1) (col 0) (offset 0))
     ((line 2) (col 5) (offset 12)))
    |}]
;;

let check_all_subsexps_map_to_their_position s sexps positions =
  let check_subsexp subsexp =
    match Positions.find_sub_sexp_in_list_phys positions sexps ~sub:subsexp with
    | None -> failwith "not found"
    | Some range ->
      assert (
        Sexp.( = )
          (Single.parse_string_exn
             (String.sub
                s
                ~pos:range.start_pos.offset
                ~len:(range.end_pos.offset - range.start_pos.offset)))
          subsexp)
  in
  let rec iter sexps =
    List.iter sexps ~f:(fun sexp ->
      check_subsexp sexp;
      match sexp with
      | Atom _ -> ()
      | List l -> iter l)
  in
  iter sexps
;;

let%expect_test "build_positions_ignore_commented_expr" =
  let f s =
    let sexps, positions = Many_and_positions.parse_string_exn s in
    print_s [%sexp (Positions.to_list positions : Positions.pos list)];
    check_all_subsexps_map_to_their_position s sexps positions
  in
  f "a #;((b)) c";
  [%expect
    {|
    (((line 1) (col 0)  (offset 0))
     ((line 1) (col 0)  (offset 0))
     ((line 1) (col 10) (offset 10))
     ((line 1) (col 10) (offset 10)))
    |}]
;;

let%expect_test "all" =
  List.iter cases ~f:(fun input ->
    let expected = build_positions_simple input in
    let got = build_positions input |> Positions.to_list in
    require
      ([%compare.equal: Positions.pos list] got expected)
      ~if_false_then_print_s:
        (lazy
          [%sexp
            { input : string; expected : Positions.pos list; got : Positions.pos list }]))
;;

let%expect_test "find" =
  List.iter cases ~f:(fun input ->
    let positions = build_positions_simple input |> Array.of_list in
    let from_parsexp = build_positions input in
    let count = Array.length positions in
    for i = 0 to count - 1 do
      for j = i + 1 to count - 1 do
        let expected =
          Positions.make_range_incl ~start_pos:positions.(i) ~last_pos:positions.(j)
        in
        let got = Result.try_with (fun () -> Positions.find from_parsexp i j) in
        require
          (match got with
           | Ok got -> [%compare.equal: Positions.range] got expected
           | Error _ -> false)
          ~if_false_then_print_s:
            (lazy
              [%sexp
                { input : string
                ; i : int
                ; j : int
                ; expected : (Positions.range, exn) Result.t = Ok expected
                ; got : (Positions.range, exn) Result.t
                }])
      done
    done);
  [%expect {| |}]
;;

let cases_for_find_sub_sexp = [ "( ( ( abc ) (+ 1 2) ) )" ]

module Annotated = struct
  type t =
    | Atom of Positions.range * Sexp.t
    | List of Positions.range * Sexp.t * t list

  let of_sexp_and_positions =
    let rec loop (sexp : Sexp.t) (positions : Positions.pos list) =
      match sexp, positions with
      | Atom _, start_pos :: last_pos :: rest ->
        Atom (Positions.make_range_incl ~start_pos ~last_pos, sexp), rest
      | List l, start_pos :: rest ->
        let annots_rev, rest =
          List.fold_left l ~init:([], rest) ~f:(fun (acc, positions) sexp ->
            let annot, rest = loop sexp positions in
            annot :: acc, rest)
        in
        (match rest with
         | [] -> assert false
         | last_pos :: rest ->
           ( List
               (Positions.make_range_incl ~start_pos ~last_pos, sexp, List.rev annots_rev)
           , rest ))
      | _ -> assert false
    in
    fun sexp positions ->
      let t, rest = loop sexp positions in
      assert (List.is_empty rest);
      t
  ;;

  let rec iter t ~f =
    match t with
    | Atom (range, sexp) -> f range sexp
    | List (range, sexp, children) ->
      f range sexp;
      List.iter children ~f:(iter ~f)
  ;;
end

let%expect_test "find_sub_sexp_phys" =
  List.iter cases_for_find_sub_sexp ~f:(fun input ->
    let sexp, positions = Single_and_positions.parse_string_exn input in
    let annot = Annotated.of_sexp_and_positions sexp (Positions.to_list positions) in
    Annotated.iter annot ~f:(fun expected sub ->
      let got = Positions.find_sub_sexp_phys positions sexp ~sub in
      require
        (Option.value_map
           got
           ~default:false
           ~f:([%compare.equal: Positions.range] expected))
        ~if_false_then_print_s:
          (lazy
            [%sexp
              { input : string
              ; sexp : Sexp.t
              ; sub : Sexp.t
              ; expected : Positions.range option = Some expected
              ; got : Positions.range option
              }])));
  [%expect {| |}]
;;

let%expect_test "advance_sexp_exn" =
  let input =
    {|() (abc) (1 (2 (3)))
  123 (+ x y)
|}
  in
  let sexps, positions = Many_and_positions.parse_string_exn input in
  let iterator = Positions.Iterator.create positions in
  List.iter sexps ~f:(fun sexp ->
    let ({ start_pos; end_pos } : Positions.range) =
      Positions.Iterator.advance_sexp_exn iterator sexp
    in
    let pos = start_pos.offset in
    let len = end_pos.offset - start_pos.offset in
    let sub = String.sub input ~pos ~len in
    let sexp_from_loc = Parsexp.Single.parse_string_exn sub in
    Expect_test_helpers_core.require_equal (module Sexp) sexp sexp_from_loc;
    print_s [%sexp (sexp : Sexp.t)]);
  [%expect
    {|
    ()
    (abc)
    (1 (2 (3)))
    123
    (+ x y)
    |}]
;;
