(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Generates one QR per version/ec_level with random data. *)

open B0_testing

let random_data st =
  let max = Qrc.Prop.mode_capacity (`V 40) `L `Byte in
  let b = Bytes.create max in
  for i = 0 to max - 1 do
    Bytes.set b i (Char.chr (Random.State.int st 256))
  done;
  Bytes.unsafe_to_string b

let test_gen =
  Test.test "generating QR codes for random data." @@ fun () ->
  let data = random_data (Test.Rand.state ()) in
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

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
