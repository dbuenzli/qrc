#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "qrc" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/qrc.mllib";
       Pkg.bin ~cond:cmdliner "test/qrtrip";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.test "test/test_props";
       Pkg.test "test/test_perf";
       Pkg.test ~run:false "test/self_test"; ]
