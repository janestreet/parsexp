open! Core_kernel
open! Import

type t = string [@@deriving quickcheck, sexp_of]
