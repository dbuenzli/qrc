(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
