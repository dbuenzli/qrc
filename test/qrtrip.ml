(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf
let tty_cap_of_fd fd =
  let rec isatty fd = try Unix.isatty fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> isatty fd
  | Unix.Unix_error (e, _, _) -> false
  in
  if not (isatty fd) then `None else
  match Unix.getenv "TERM" with
  | exception Not_found -> `None | "" -> `None | "dumb" -> `None
  | v -> `Ansi

let stdin_to_string () =
  let rec loop acc = match input_line stdin with
  | l -> loop (l :: acc)
  | exception End_of_file -> Ok (String.concat "" (List.rev acc))
  | exception Sys_error e -> Error e
  in
  loop []

let string_to_file file s =
  try
    let oc = open_out_bin file in
    let finally () = close_out_noerr oc in
    Ok (Fun.protect ~finally @@ fun () -> output_string oc s)
  with Sys_error e -> Error e

(* Encoding *)

let get_format outf = function
| Some f -> f
| None ->
    match outf with
    | Some f when not (String.equal "-" f) -> `Unicode_full
    | _ ->
        match tty_cap_of_fd Unix.stdout with
        | `None -> `Unicode_full | `Ansi -> `Ansi

let output_matrix format outf ~invert ~quiet_zone m =
  let pp_svg ?invert ?quiet_zone ppf m =
    Format.pp_print_string ppf (Qrc.Matrix.to_svg ?invert ?quiet_zone m)
  in
  let pp = match get_format outf format with
  | `Ascii -> Qrc_fmt.pp_ascii
  | `Ansi -> Qrc_fmt.pp_ansi
  | `Svg -> pp_svg
  | `Unicode_half -> Qrc_fmt.pp_utf_8_half
  | `Unicode_full -> Qrc_fmt.pp_utf_8_full
  in
  let qr = strf "@[%a@]" (pp ~invert ~quiet_zone) m in
  match outf with
  | None -> print_string qr; Ok ()
  | Some outf -> string_to_file outf qr

let encode format outf invert no_quiet_zone version ec_level mask data =
  let get_data = function None -> stdin_to_string () | Some d -> Ok d in
  Result.bind (get_data data) @@ fun data ->
  let quiet_zone = not no_quiet_zone in
  let version = Option.map (fun v -> `V v) version in
  match Qrc.encode ?mask ?version ?ec_level data with
  | Some m ->
      Result.map Result.ok (output_matrix format outf ~invert ~quiet_zone m)
  | None ->
      let ec_level = match ec_level with None -> `M | Some l -> l in
      let version = match version with None -> `V 40 | Some v -> v in
      let err = strf
          "QR code capacity of %d bytes exceeded: %d data bytes provided\n\
           Try to increase the version (-v) or reduce the error correction \
           level (-c)"
          (Qrc.Prop.mode_capacity version ec_level `Byte)
          (String.length data)
      in
      Ok (Error err)

(* Decoding *)

let decode () = Error "Sorry, unimplemented yet."

(* Tripping *)

let qrtrip enc format outf invert no_quiet_zone version ec_level mask data  =
  let log_err e = Format.eprintf "@[%s: @[%s@]@]@." Sys.executable_name e in
  let r = match enc with
  | `Decode -> decode ()
  | `Encode ->
      encode format outf invert no_quiet_zone version ec_level mask data
  in
  match r with
  | Error e -> log_err e; 2
  | Ok r -> match r with Ok () -> 0 | Error e -> log_err e; 1

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let int_range ~lo ~hi kind =
  let err fmt = Format.ksprintf (fun e -> Error (`Msg e)) fmt in
  let parse_range v = match int_of_string v with
  | exception Failure _ -> err "%s: not an integer in %d-%d" v lo hi
  | v when lo <= v && v <= hi -> Ok v
  | v -> err "%d: invalid %s, must be in %d-%d" v kind lo hi
  in
  Arg.conv (parse_range, Format.pp_print_int)

let format =
  let formats =
    [ "ascii", `Ascii; "ansi", `Ansi; "svg", `Svg; "half", `Unicode_half;
      "text", `Unicode_full ]
  in
  let doc =
    "QR matrix output format. \
     $(b,ascii) outputs US-ASCII characters, \
     $(b,ansi) outputs ANSI terminal escape sequences, \
     $(b,svg) outputs an SVG image,
     $(b,text) outputs UTF-8 encoded Unicode full blocks, \
     $(b,half) outputs UTF-8 encoded Unicode half blocks."
  in
  let env = Cmd.Env.info "QRTRIP_FORMAT" and docv = "FORMAT" in
  let fconv = Arg.(some ~none:"ansi or text" (Arg.enum formats)) in
  Arg.(value & opt fconv None & info ~doc ~docv ["f"; "format"] ~env)

let invert =
  let doc = "Invert QR matrix, black modules are white and vice-versa." in
  Arg.(value & flag & info ["i"; "invert"] ~doc)

let no_quiet_zone =
  let doc =
    "Do not output the quiet zone. Not recommended, leave scanners in peace."
  in
  Arg.(value & flag & info ["no-quiet"] ~doc)

let outf =
  let doc = "Output to $(docv)." and docv = "FILE" in
  Arg.(value & opt (some ~none:"stdout" string) None & info ["o"] ~doc ~docv)

let version =
  let doc =
    "Use QR code $(docv) (1-40) for encoding. This defines the matrix width \
     (21-177). Not recommended, let the minimal needed version be selected."
  in
  let vconv = int_range ~lo:1 ~hi:40 "version" and none = "minimal needed" in
  Arg.(value & opt (some ~none vconv) None &
       info ~doc ~docv:"VERSION" ["v"; "qr-version"])

let mask =
  let doc =
    "Force QR mask to $(docv) (0-7) for encoding. This option should \
     not be used, it is used for debugging."
  in
  let mconv = int_range ~lo:0 ~hi:7 "mask" and none = "standard compliant" in
  Arg.(value & opt (some ~none mconv) None & info ~doc ~docv:"MASK" ["mask"])

let data =
  let doc = "Data to encode." and none = "read from stdin" in
  Arg.(value & pos 0 (some ~none string) None & info ~doc ~docv:"DATA" [])

let ec_level =
  let levels = [ "L", `L; "M", `M; "Q", `Q; "H", `H ] in
  let doc = Printf.sprintf
      "Use error correction $(docv) for encoding. Must be %s. Respectively \
       corresponds to 7%%, 15%%, 25%% and 30%% damage correction. A higher \
       level may end up being used if that does not change the selected QR \
       code version (its width)."
      (Arg.doc_alts_enum levels)
  in
  let lconv = Arg.some ~none:"M" (Arg.enum levels) in
  Arg.(value & opt lconv None & info ~doc ~docv:"LEVEL" ["c"; "ec-level"])

let encode =
  let enc = Arg.info ~doc:"Encode a QR code (default)." ["e"; "encode"] in
  let dec = Arg.info ~doc:"Decode a QR code (unimplemented)." ["d"; "decode"] in
  Arg.(value & vflag `Encode  [`Encode, enc; `Decode, dec])

let qrtrip =
  let doc = "QR encode data" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) encodes data into QR codes.";
    `S Manpage.s_bugs;
    `P "This program is distributed with the Qrc OCaml library.
        See https://erratique.ch/software/qrc for contact information."; ]
  in
  let exits =
    Cmd.Exit.info ~doc:"on QR code capacity exceeded." 1 ::
    Cmd.Exit.info ~doc:"on indiscriminate error reported on stderr." 2 ::
    Cmd.Exit.defaults
  in
  Cmd.v (Cmd.info "qrtrip" ~version:"%%VERSION%%" ~doc ~man ~exits) @@
  Term.(const qrtrip $ encode $ format $ outf $ invert $ no_quiet_zone $
        version $ ec_level $ mask $ data)

let main () = Cmd.eval' qrtrip
let () = if !Sys.interactive then () else exit (main ())
