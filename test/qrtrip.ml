(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind
let exec = Filename.basename Sys.executable_name

let tty_bold = "\027[01m"
let tty_red_bold = "\027[31;01m"
let tty_reset = "\027[m"
let pp_code ppf s = Format.fprintf ppf "@<0>%s%s@<0>%s" tty_bold s tty_reset
let log_err fmt =
  Format.eprintf ("@[%s: @<0>%s%s@<0>%s: " ^^ fmt ^^ "@]@.")
    exec tty_red_bold "Error" tty_reset

let log_if_error ~use = function Ok v -> v | Error e -> log_err "%s" e; use

let tty_cap_of_fd fd =
  let rec isatty fd = try Unix.isatty fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> isatty fd
  | Unix.Unix_error (e, _, _) -> false
  in
  if not (isatty fd) then `None else
  match Unix.getenv "TERM" with
  | exception Not_found -> `None | "" -> `None | "dumb" -> `None
  | v -> `Ansi

let read_file file =
  let read file ic = try Ok (In_channel.input_all ic) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdin () = In_channel.set_binary_mode In_channel.stdin true in
  try match file with
  | "-" -> binary_stdin (); read file In_channel.stdin
  | file -> In_channel.with_open_bin file (read file)
  with Sys_error e -> Error e

let write_file file s =
  let write file s oc = try Ok (Out_channel.output_string oc s) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdout () = Out_channel.(set_binary_mode stdout true) in
  try match file with
  | "-" -> binary_stdout (); write file s Out_channel.stdout
  | file -> Out_channel.with_open_bin file (write file s)
  with Sys_error e -> Error e

(* Encoding *)

let err_capacity = 1

let format_for_stdout () = match tty_cap_of_fd Unix.stdout with
| `None -> `Unicode_full | `Ansi -> `Ansi

let get_format outf = function
| Some format -> format
| None ->
    match outf with
    | "-" -> format_for_stdout ()
    | file when String.ends_with ~suffix:".svg" file -> `Svg
    | _ -> `Unicode_full

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
  let qr = Format.asprintf "@[%a@]" (pp ~invert ~quiet_zone) m in
  write_file outf qr

let encode
    ~format ~outf ~invert ~no_quiet_zone ~version ~ec_level ~mask ~is_msg
    ~file_or_msg
  =
  let* data = if is_msg then Ok file_or_msg else read_file file_or_msg in
  let quiet_zone = not no_quiet_zone in
  let version = Option.map (fun v -> `V v) version in
  match Qrc.encode ?mask ?version ?ec_level data with
  | Some m ->
      let* () = output_matrix format outf ~invert ~quiet_zone m in
      Ok Cmdliner.Cmd.Exit.ok
  | None ->
      let ec_level = match ec_level with None -> `M | Some l -> l in
      let version = match version with None -> `V 40 | Some v -> v in
      log_err
        "@[<v>QR capacity of %a bytes exhausted: data has %a bytes@,\
         Reduce error correction with %a or icrease the version with %a"
        pp_code (Int.to_string (Qrc.Prop.mode_capacity version ec_level `Byte))
        pp_code (Int.to_string (String.length data))
        pp_code "-c" pp_code "-v";
      Ok err_capacity

(* Decoding *)

let decode () = Error "Sorry, unimplemented yet."

(* Tripping *)

let qrtrip
    ~encode:enc ~format ~outf ~invert ~no_quiet_zone ~version ~ec_level ~mask
    ~is_msg ~file_or_msg
  =
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  match enc with
  | `Decode -> decode ()
  | `Encode ->
      encode ~format ~outf ~invert ~no_quiet_zone ~version ~ec_level ~mask
        ~is_msg ~file_or_msg

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
     $(b,half) outputs UTF-8 encoded Unicode half blocks. If absent \
     $(b,svg) is used if the file of $(b,-o) ends with $(b,.svg), $(b,ansi)
     or $(b,full) is used on $(b,stdout) and $(b,full) otherwise.";
  in
  let env = Cmd.Env.info "QRTRIP_FORMAT" and docv = "FORMAT" in
  let absent = "guess" in
  let fconv = Arg.some (Arg.enum formats) in
  Arg.(value & opt fconv None & info ~absent ~doc ~docv ["f"; "format"] ~env)

let invert =
  let doc = "Invert QR matrix, black modules are white and vice-versa." in
  Arg.(value & flag & info ["i"; "invert"] ~doc)

let no_quiet_zone =
  let doc =
    "Do not output the quiet zone. Not recommended, leave scanners in peace."
  in
  Arg.(value & flag & info ["no-quiet"] ~doc)

let outf =
  let doc = "Output to $(docv). Standard output if unspecified." in
  let docv = "FILE" in
  Arg.(value & opt string "-" & info ["o"] ~doc ~docv)

let version =
  let doc =
    "Use QR code $(docv) (1-40) for encoding. This defines the matrix width \
     (21-177). Not recommended, let the minimal needed version be selected."
  in
  let vconv = int_range ~lo:1 ~hi:40 "version" and absent = "minimal needed" in
  Arg.(value & opt (some vconv) None &
       info ["v"; "qr-version"] ~absent ~doc ~docv:"VERSION")

let mask =
  let doc =
    "Force QR mask to $(docv) (0-7) for encoding. This option should \
     not be used, it is used for debugging."
  in
  let mconv = int_range ~lo:0 ~hi:7 "mask" and absent = "standard compliant" in
  Arg.(value & opt (some mconv) None & info ["mask"] ~absent ~doc ~docv:"MASK")

let is_msg =
  let doc = "Positional argument is the message to encode, not a file path." in
  Arg.(value & flag & info ["m"; "message"] ~doc)

let file_or_msg =
  let doc = "File or message (with option $(b,-m)) to encode. \
             Standard input if unspecified." in
  Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"FILE")

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
  Arg.(value & opt lconv None & info ["c"; "ec-level"] ~doc ~docv:"LEVEL")

let encode =
  let enc = Arg.info ["e"; "encode"] ~doc:"Encode a QR code (default)." in
  let dec = Arg.info ["d"; "decode"] ~doc:"Decode a QR code (unimplemented)." in
  Arg.(value & vflag `Encode  [`Encode, enc; `Decode, dec])

let err_capacity = 1

let qrtrip =
  let doc = "QR encode data" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) encodes data into QR codes. Examples:";
    `Pre "$(iname) $(b,/path/to/file)            # QR code for file contents";
    `Noblank;
    `Pre "$(iname) $(b,-m https://example.org)   \
          # QR code for https://example.org";
    `Noblank;
    `Pre "$(iname) $(b,-m https://example.org -o example.svg)"; `Noblank;
    `Pre "$(iname) $(b,-f svg -m https://example.org > example.svg)"; `Noblank;
    `Pre
      "$(iname) $(b,-f svg -m https://example.org | show-url -t example.svg)";
    `S Manpage.s_bugs;
    `P "This program is distributed with the Qrc OCaml library.
        See https://erratique.ch/software/qrc for contact information."; ]
  in
  let exits =
    Cmd.Exit.info ~doc:"on QR code capacity exceeded." err_capacity ::
    Cmd.Exit.defaults
  in
  Cmd.v (Cmd.info "qrtrip" ~version:"%%VERSION%%" ~doc ~man ~exits) @@
  let+ encode and+ format and+ outf and+ invert and+ no_quiet_zone
  and+ version and+ ec_level and+ mask and+ is_msg and+ file_or_msg in
  qrtrip ~encode ~format ~outf ~invert ~no_quiet_zone
    ~version ~ec_level ~mask ~is_msg ~file_or_msg

let main () = Cmd.eval' qrtrip
let () = if !Sys.interactive then () else exit (main ())
