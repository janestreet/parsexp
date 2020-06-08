open! Core_kernel
open! Import

type t = string [@@deriving sexp_of]

let does_parse signifier =
  match Parsexp.Single.parse_string_exn signifier with
  | exception _ -> false
  | List _ -> false
  | Atom _ -> true
;;

let any_char = Char.gen_uniform_inclusive Char.min_value Char.max_value

let very_short_string char_generator =
  let%bind.Quickcheck.Generator len =
    Base_quickcheck.Generator.int_log_uniform_inclusive 1 5
  in
  String.gen_with_length len char_generator
;;

let with_affix infix_generator =
  let%map.Quickcheck.Generator affix = very_short_string Char.gen_alpha
  and infix = infix_generator
  and affix_is_prefix = [%quickcheck.generator: bool] in
  let prefix, suffix =
    match affix_is_prefix with
    | true -> affix, ""
    | false -> "", affix
  in
  [%string {|%{prefix}%{infix}%{suffix}|}]
;;

let with_delimiters ~left ~right generator =
  let%map.Quickcheck.Generator middle = generator in
  [%string {|%{left}%{middle}%{right}|}]
;;

let in_quoted_string generators =
  Quickcheck.Generator.union generators
  |> with_affix
  |> with_delimiters ~left:{|"|} ~right:{|"|}
  |> Quickcheck.Generator.filter ~f:does_parse
;;

let line_break = Quickcheck.Generator.of_list [ "\n"; "\r\n" ]

let backslash_newline =
  let%map.Quickcheck.Generator line_break = line_break
  and whitespace = very_short_string Char.gen_whitespace in
  [%string {|\%{line_break}%{whitespace}|}]
;;

let backslash_numeric prefix int_to_string =
  let%map.Quickcheck.Generator signified = very_short_string any_char in
  String.concat_map signified ~f:(fun char ->
    let escaped = int_to_string (Char.to_int char) in
    [%string {|\%{prefix}%{escaped}|}])
;;

let backslash_decimal = backslash_numeric "" Int.to_string
let backslash_hex = backslash_numeric "x" (sprintf "%02x")

let quoted_strings =
  in_quoted_string [ backslash_newline; backslash_decimal; backslash_hex ]
;;

let lorem_ipsum =
  (* backslash plus a random character is likely to form an invalid escape sequence. *)
  Char.gen_print |> Quickcheck.Generator.filter ~f:(Char.( <> ) '\\') |> String.gen'
;;

let line_comments =
  with_affix
    (let%map.Quickcheck.Generator comment = lorem_ipsum
     and num_semicolons = Base_quickcheck.Generator.int_uniform_inclusive 1 3
     and line_break = line_break in
     let semicolons = String.make num_semicolons ';' in
     [%string {|%{semicolons}%{comment}%{line_break}|}])
  |> Quickcheck.Generator.filter ~f:does_parse
;;

let sexp_comments =
  Base_quickcheck.Generator.sexp_of [%quickcheck.generator: string]
  |> Quickcheck.Generator.map ~f:Sexp.to_string
  |> with_delimiters ~left:"#;" ~right:""
  |> with_affix
  |> Quickcheck.Generator.filter ~f:does_parse
;;

let get_signifier signified = Sexp.to_string_hum (Atom signified)

let regular_strings =
  let%map.Quickcheck.Generator signified = String.gen' any_char in
  get_signifier signified
;;

let all_but_block_comments =
  [ line_comments; quoted_strings; regular_strings; sexp_comments ]
;;

let block_comments =
  Quickcheck.Generator.recursive_union
    all_but_block_comments
    ~f:(with_delimiters ~left:"#|" ~right:"|#" >> with_affix >> List.return)
  |> Quickcheck.Generator.filter ~f:does_parse
;;

let quickcheck_generator =
  Quickcheck.Generator.union (block_comments :: all_but_block_comments)
  |> Quickcheck.Generator.filter ~f:does_parse
;;

let quickcheck_shrinker =
  Quickcheck.Shrinker.filter [%quickcheck.shrinker: string] ~f:does_parse
;;

let quickcheck_observer = Quickcheck.Observer.of_hash (module String)
