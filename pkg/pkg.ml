#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "aperf" @@ fun c->
  Ok [ Pkg.bin "aperf/aperf"
     ; Pkg.lib "pkg/META"
     ; Pkg.mllib ~api:["Aperf"] "aperf/aperf.mllib" ]

