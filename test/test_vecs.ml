(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

(* One matrix is generated per QR code version/ec level. The standard
   has no test vectors and most QR code encoders out there seem to
   disagree on their outputs, so this only tests the implementation
   against new versions of itself. *)

let random_data seed =
  Random.init seed;
  let max = Qrc.Prop.mode_capacity (`V 40) `L `Byte in
  let b = Bytes.create max in
  for i = 0 to max - 1 do Bytes.set b i (Char.chr (Random.int 256)) done;
  Bytes.unsafe_to_string b

let matrix_to_string m =
  let bytes = Qrc.Matrix.bits m in
  let b = Bytes.create (Bigarray.Array1.dim bytes) in
  let get bits i = Char.chr (Bigarray.Array1.get bits i)in
  for i = 0 to Bytes.length b - 1 do Bytes.set b i (get bytes i) done;
  Bytes.unsafe_to_string b

let fold_all_qrs f acc data =
  let acc = ref acc in
  for v = 1 to 40 do
    let gen version ec_level =
      let mode = `Byte in
      let max_len = Qrc.Prop.mode_capacity version ec_level mode in
      let data = String.sub data 0 max_len in
      let m = Option.get @@ Qrc.encode ~version ~ec_level ~mode data in
      acc := f !acc ~version ~ec_level m
    in
    List.iter (gen (`V v)) [ `L; `M; `Q; `H ]
  done;
  !acc

let gen data file =
  try
    B0_std.Fmt.epr "Writing vectors in %a@." B0_std.Fmt.code file;
    Out_channel.with_open_bin file @@ fun oc ->
    let pr fmt = Printf.fprintf oc fmt in
    let f acc ~version ~ec_level:e m =
      let m = matrix_to_string m in
      if e = `L then pr "[|\n"; pr "%S;\n" m; if e = `H then pr "|];\n"
    in
    pr "(* Self test vectors, generated by self_test.ml *)\n";
    pr "let version = [|\n"; fold_all_qrs f () data; pr "|]\n";
    0
  with Sys_error e -> Printf.eprintf "%s" e; 1

let test data =
  Test.test "vectors" @@ fun () ->
  let f () ~version:(`V version) ~ec_level:ec m =
    let ec_idx = match ec with `L -> 0 | `M -> 1 | `Q -> 2 | `H -> 3 in
    let test = Vecs.version.(version - 1).(ec_idx) in
    if test = matrix_to_string m then () else
    Test.fail "Mismatch error: version %d ec_level: %d" version ec_idx
  in
  fold_all_qrs f () data

let run action =
  let data = random_data 1031 in
  match action with
  | `Gen -> gen data "test/vecs.ml"
  | `Test ->
      Test.main @@ fun () ->
      if Array.length Vecs.version = 40 then test data else
      Test.log "@[<v>%a, generate them BEFORE making changes with:@,%a@]"
        (B0_std.Fmt.st [`Fg (`Yellow)]) "No test vectors"
        B0_std.Fmt.code "b0 -- test_vecs --gen"

let main () =
  let usage = "Usage: test_vecs [--gen]" in
  let action = ref `Test in
  let args =
    [ "--gen", Arg.Unit (fun () -> action := `Gen),
      "Generate vectors in test/vecs.ml"; ]
  in
  let strf = Printf.sprintf in
  let fail_pos s = raise (Arg.Bad (strf "Don't know what to do with %S" s)) in
  Arg.parse args fail_pos usage;
  run !action

let () = if !Sys.interactive then () else exit (main ())
