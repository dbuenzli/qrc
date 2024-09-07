(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Generates one QR per version/ec_level with random data. *)

open B0_testing

let strf = Printf.sprintf
let pr = Printf.printf

let random_init seed =
  let seed = match seed with
  | Some seed -> seed | None -> Random.self_init (); Random.int 10000
  in
  Random.init seed; seed

let random_data () =
  let max = Qrc.Prop.mode_capacity (`V 40) `L `Byte in
  let b = Bytes.create max in
  for i = 0 to max - 1 do Bytes.set b i (Char.chr (Random.int 256)) done;
  Bytes.unsafe_to_string b

let gen data =
  Test.test "generating QR codes for random data." @@ fun () ->
  for v = 1 to 40 do
    let gen version ec_level =
      let mode = `Byte in
      let max_len = Qrc.Prop.mode_capacity version ec_level mode in
      let data = String.sub data 0 max_len in
      let m = Option.get @@ Qrc.encode ~version ~ec_level ~mode data in
      Sys.opaque_identity @@ ignore (m)
    in
    List.iter (gen (`V v)) [ `L; `M; `Q; `H ]
  done

let test seed =
  Test.main @@ fun () ->
  let seed = random_init seed in
  let data = random_data () in
  Test.log "Using random seed %d" seed;
  gen data

let main () =
  let usage = "Usage: test_perf [--seed SEED]" in
  let seed = ref None  in
  let args =
    [ "--seed", Arg.Int (fun i -> seed := Some i), "Seed for random data." ]
  in
  let fail_pos s = raise (Arg.Bad (strf "Don't know what to do with %S" s)) in
  Arg.parse args fail_pos usage;
  test !seed

let () = if !Sys.interactive then () else exit (main ())
