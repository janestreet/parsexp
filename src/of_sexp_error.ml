open! Import

type t =
  { user_exn : exn
  ; sub_sexp : Sexp.t
  ; location : Positions.range option
  }
[@@deriving sexp_of]

let user_exn t = t.user_exn
let sub_sexp t = t.sub_sexp
let location t = t.location

let report ppf ~filename t =
  let line, start, stop =
    match t.location with
    | None -> 1, 0, 0
    | Some { start_pos; end_pos } ->
      start_pos.line, start_pos.col, start_pos.col + end_pos.offset - start_pos.offset
  in
  Format.fprintf
    ppf
    "File \"%s\", line %d, characters %d-%d:\n\
     Error: s-expression conversion error;\n\
     exception %s\n"
    filename
    line
    start
    stop
    (Sexplib0.Sexp_conv.printexc_prefer_sexp t.user_exn)
;;

exception Of_sexp_error of t [@@deriving sexp_of]

let raise ~user_exn ~sub_sexp ~location =
  raise (Of_sexp_error { user_exn; sub_sexp; location })
;;
