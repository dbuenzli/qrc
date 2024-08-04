(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** QR matrix text formatters. *)

type t =
  ?invert:bool -> ?quiet_zone:bool -> Format.formatter -> Qrc.Matrix.t -> unit
(** The type for QR matrix formatters. If [invert] is [true] (defaults
    to [false]) black modules are white and vice-versa. If
    [quiet_zone] is [true] (default) the surrounding frame of four zero
    modules is added. *)

val pp_ascii : t
(** [pp_ascii] uses two U+0020 ([' ']) for white modules and two U+0023
    (['#']) for black ones. *)

val pp_ansi : t
(** [pp_ansi] uses two ANSI white U+0020 ([' ']) for white modules and two
    ANSI black U+0020 ([' ']) for black ones. *)

val pp_utf_8_full : t
(** [pp_utf_8_full] uses two U+0020 ([' ']) for white modules and two
    UTF-8 encoded U+2588 (FULL BLOCK) for black ones. *)

val pp_utf_8_half : t
(** [pp_utf_8_half] fits two vertical modules per character. It uses
    U+0020 ([' ']) and UTF-8 encoded U+2588 (FULL BLOCK), U+2584 (LOWER HALF
    BLOCK) and U+2580 (UPPER HALF BLOCK). *)
