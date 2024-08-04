(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  ?invert:bool -> ?quiet_zone:bool -> Format.formatter -> Qrc.Matrix.t -> unit

let pp_cut = Format.pp_print_cut
let pp_char c ppf () = Format.pp_print_char ppf c
let pp_str_char s ppf () = Format.pp_print_as ppf 1 s
let pp_twice pp ppf () = for i = 1 to 2 do pp ppf () done
let pp_quiet_cols ~pp_white ppf () = for i = 0 to 3 do pp_white ppf () done
let pp_quiet_line ~pp_white ppf m =
  Format.pp_open_hbox ppf (); pp_quiet_cols ~pp_white ppf ();
  for i = 0 to (Qrc.Matrix.w m) - 1 do pp_white ppf () done;
  pp_quiet_cols ~pp_white ppf (); Format.pp_close_box ppf ()

let pp_one_to_one
    ~pp_white ~pp_black ?(invert = false) ?(quiet_zone = true) ppf m
  =
  let pp_white = if invert then pp_black else pp_white
  and pp_black = if invert then pp_white else pp_black in
  let pp_quiet_cols = pp_quiet_cols ~pp_white in
  let pp_quiet_lines ppf m =
    for i = 0 to 3 do pp_quiet_line ~pp_white ppf m; pp_cut ppf (); done
  in
  let pp_start_line ppf () =
    Format.pp_open_hbox ppf (); if quiet_zone then pp_quiet_cols ppf ();
  in
  let pp_end_line ppf () =
    if quiet_zone then pp_quiet_cols ppf (); Format.pp_close_box ppf ();
    pp_cut ppf ();
  in
  Format.pp_open_vbox ppf 0; if quiet_zone then pp_quiet_lines ppf m;
  for y = 0 to (Qrc.Matrix.w m) - 1 do
    pp_start_line ppf ();
    for x = 0 to (Qrc.Matrix.w m) - 1 do
      if Qrc.Matrix.get m ~x ~y then pp_black ppf () else pp_white ppf ()
    done;
    pp_end_line ppf ();
  done;
  if quiet_zone then pp_quiet_lines ppf m; Format.pp_close_box ppf ()

let pp_two_v_to_one
  ~pp_00 ~pp_10 ~pp_01 ~pp_11 ?(invert = false) ?(quiet_zone = true) ppf m
  =
  let pp_00 = if invert then pp_11 else pp_00
  and pp_01 = if invert then pp_10 else pp_01
  and pp_10 = if invert then pp_01 else pp_10
  and pp_11 = if invert then pp_00 else pp_11 in
  let pp_quiet_cols = pp_quiet_cols ~pp_white:pp_00 in
  let pp_quiet_lines ppf m =
    for i = 0 to 1 do pp_quiet_line ~pp_white:pp_00 ppf m; pp_cut ppf (); done
  in
  let pp_start_line ppf () =
    Format.pp_open_hbox ppf (); if quiet_zone then pp_quiet_cols ppf ();
  in
  let pp_end_line ppf () =
    if quiet_zone then pp_quiet_cols ppf (); Format.pp_close_box ppf ();
    pp_cut ppf ();
  in
  Format.pp_open_vbox ppf 0; if quiet_zone then pp_quiet_lines ppf m;
  for y = 0 to (Qrc.Matrix.w m / 2) - 1 do
    pp_start_line ppf ();
    let y0 = y * 2 and y1 = y * 2 + 1 in
    for x = 0 to (Qrc.Matrix.w m) - 1 do
      let v0 = Qrc.Matrix.get m ~x ~y:y0 in
      let v1 = Qrc.Matrix.get m ~x ~y:y1 in
      if v0
      then (if v1 then pp_11 ppf () else pp_10 ppf ())
      else (if v1 then pp_01 ppf () else pp_00 ppf ())
    done;
    pp_end_line ppf ()
  done;
  if (Qrc.Matrix.w m) mod 2 = 1 then begin (* last line if odd w *)
    pp_start_line ppf ();
    let y = Qrc.Matrix.w m - 1 in
    for x = 0 to Qrc.Matrix.w m - 1 do
      if Qrc.Matrix.get m ~x ~y then pp_10 ppf () else pp_00 ppf ()
    done;
    pp_end_line ppf ()
  end;
  if quiet_zone then pp_quiet_lines ppf m; Format.pp_close_box ppf ()

let pp_ascii =
  let pp_white = pp_twice (pp_char ' ') and pp_black = pp_twice (pp_char '#') in
  pp_one_to_one ~pp_white ~pp_black

let pp_ansi =
  let pp_white = pp_str_char "\027[47m  \027[m" in
  let pp_black = pp_str_char "\027[40m  \027[m" in
  pp_one_to_one ~pp_white ~pp_black

let pp_utf_8_full =
  let pp_white = pp_twice (pp_char ' ') in
  let pp_black = pp_twice (pp_str_char "\u{2588}") in
  pp_one_to_one ~pp_white ~pp_black

let pp_utf_8_half =
  let pp_00 = pp_char ' ' in
  let pp_10 = pp_str_char "\u{2580}" in
  let pp_01 = pp_str_char "\u{2584}" in
  let pp_11 = pp_str_char "\u{2588}" in
  pp_two_v_to_one ~pp_00 ~pp_10 ~pp_01 ~pp_11
