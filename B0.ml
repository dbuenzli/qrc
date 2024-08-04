open B0_kit.V000

(* Library names *)

let qrc = B0_ocaml.libname "qrc"

let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let qrc_lib =
  let srcs = [ `Dir ~/"src" ] in
  B0_ocaml.lib qrc ~srcs

(* Tools *)

let qrtrip_tool =
  let srcs = [ `File ~/"test/qrtrip.ml"] in
  let requires = [qrc; unix; cmdliner] in
  B0_ocaml.exe "qrtrip" ~srcs ~requires ~doc:"The qrtrip tool"

(* Tests *)

let test ?doc ?run:(r = true) ?(requires = []) ?(srcs = []) src =
  let srcs = (`File src) :: srcs in
  let requires = qrc :: requires in
  let meta = B0_meta.(empty |> tag test |> ~~ run r) in
  let name = Fpath.basename ~strip_ext:true src in
  B0_ocaml.exe name ~srcs ~requires ~meta ?doc

let test_perf = test ~/"test/test_perf.ml"
let test_props = test ~/"test/test_props.ml"
let test_self_props =
  test ~run:false ~/"test/self_test.ml" ~srcs:[`File ~/"test/self_vecs.ml"]


(* Packs *)

let default =
   let meta =
   B0_meta.empty
   |> ~~ B0_meta.authors ["The qrc programmers"]
   |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
   |> ~~ B0_meta.homepage "https://erratique.ch/software/qrc"
   |> ~~ B0_meta.online_doc "https://erratique.ch/software/qrc/doc"
   |> ~~ B0_meta.licenses ["ISC"]
   |> ~~ B0_meta.repo "git+https://erratique.ch/repos/qrc.git"
   |> ~~ B0_meta.issues "https://github.com/dbuenzli/qrc/issues"
   |> ~~ B0_meta.description_tags ["qr-code"; "codec"; "org:erratique"; ]
   |> ~~ B0_opam.build
     {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
                  "--with-cmdliner" "%{cmdliner:installed}%" ]]|}
   |> ~~ B0_opam.depopts ["cmdliner", "";]
   |> ~~ B0_opam.conflicts [ "cmdliner", {|< "1.3.0"|}; ]
   |> ~~ B0_opam.depends
     [ "ocaml", {|>= "4.14.0"|};
       "ocamlfind", {|build|};
       "ocamlbuild", {|build|};
       "topkg", {|build & >= "1.0.3"|};
       "b0", {|dev & with-test|};
     ]
   |> B0_meta.tag B0_opam.tag
 in
 B0_pack.make "default" ~doc:"The qrc package" ~meta ~locked:true @@
 B0_unit.list ()
