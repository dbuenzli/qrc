(*---------------------------------------------------------------------------
   Copyright (c) 2024 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let output_svg_qr data = match Qrc.encode data with
| None -> prerr_endline "Data capacity exceeded!"
| Some m -> print_endline (Qrc.Matrix.to_svg m)
