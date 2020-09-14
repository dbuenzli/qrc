(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Generates one QR per version/ec_level with random data. *)

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
  let seed = random_init seed in
  let data = random_data () in
  pr "Using random seed %d\n%!" seed; gen data; pr "Done!\n%!"

let main () =
  let usage = "Usage: test_perf [--seed SEED]" in
  let seed = ref None  in
  let args =
    [ "--seed", Arg.Int (fun i -> seed := Some i), "Seed for random data." ]
  in
  let fail_pos s = raise (Arg.Bad (strf "Don't know what to do with %S" s)) in
  Arg.parse args fail_pos usage;
  test !seed

let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
