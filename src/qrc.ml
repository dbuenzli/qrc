(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Any mention of section, figure or table refers to the ISO/IEC 18004:2015(E)
   standard. *)

let div_round_up x y = (x + y - 1) / y

(* Sequences of bytes. We use bigarrays to get typed array in js_of_ocaml *)

module Bytes = struct
  type t = (int,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t
  let _create len = Bigarray.(Array1.create int8_unsigned c_layout len)
  let create ~len v = let a = _create len in Bigarray.Array1.fill a v; a
  let length b = Bigarray.Array1.dim b
  let get = (Bigarray.Array1.get : t -> int -> int)
  let set = (Bigarray.Array1.set : t -> int -> int -> unit)
  let copy b = let c = _create (length b) in Bigarray.Array1.blit b c; c
  let blit ~src si ~dst di ~len =
    let src = Bigarray.Array1.sub src si len in
    let dst = Bigarray.Array1.sub dst di len in
    Bigarray.Array1.blit src dst
end

(* Sequences of bits. *)

module Bits = struct
  type t = Bytes.t
  let create len v =
    Bytes.create ~len:(div_round_up len 8) (if v then 255 else 0)

  let length b = (Bytes.length b) * 8
  let copy = Bytes.copy
  let[@inline] get bits i =
    let i_byte = i / 8 in
    let bit_mask = 1 lsl (7 - i mod 8) in
    (Bytes.get bits i_byte) land bit_mask > 0

  let[@inline] set bits i v =
    let i_byte = i / 8 in
    let byte = Bytes.get bits i_byte in
    let bit_mask = 1 lsl (7 - i mod 8) in
    let byte' = if v then byte lor bit_mask else byte land (lnot bit_mask) in
    Bytes.set bits i_byte byte'
end

(* Galois fields and Reed-Solomon encoding

   Follows the treatement found at https://research.swtch.com/field, see also
   https://en.wikipedia.org/wiki/Finite_field_arithmetic#Implementation_tricks*)
module Gf_256 = struct
  type poly = int
  type byte = int
  type t =
    { log : byte array; (* 256 els *)
      exp : byte array; (* 510 els, twice the same data to avoid mod in mul *) }

  let mul x y ~mod' = (* slow, only used to generate the exp table *)
    let z = ref 0 and x = ref x and y = ref y in
    while (!x > 0) do
      if !x land 1 = 1 then z := !z lxor !y; (* add *)
      x := !x lsr 1;
      y := !y lsl 1;
      if !y land 0x100 <> 0 then (* exceeds deg 7 *) y := !y lxor mod' (* sub *)
    done;
    !z

  let create ~r ~g =
    let f = { log = Array.make 256 0; exp = Array.make 510 0 } in
    let x = ref 1 (* g ^ 0 *) in
    for i = 0 to 254 do
      (* The exp table is doubled to avoid a mod 255 in mul below *)
      f.exp.(i) <- !x; f.exp.(i + 255) <- !x;
      f.log.(!x) <- i;
      x := mul !x g ~mod':r
    done;
    f.log.(0) <- 255;
    f

  let[@inline] add x y = x lxor y
  let[@inline] sub x y = x lxor y
  let[@inline] exp f x = f.exp.(x mod 255)
  let[@inline] log f x = f.log.(x)
  let[@inline] inv f x = if x = 0 then 0 else f.exp.(255 - f.log.(x))
  let[@inline] mul f x y =
    if x = 0 || y = 0 then 0 else f.exp.(f.log.(x) + f.log.(y))
end

module Rs = struct
  type gen = int array (* Generator polynomial coefficients (hi to lo). *)

  (* Generates a polynomial for [ec] error correction bytes. Given [g] the
     generator of [f] this computes the coefficients of the polynomial:
     gen(x) = (x - g^0)(x - g^1)(x - g^ec) *)
  let gen f ~ec : gen =
    let gen = Array.make (ec + 1) 0 in
    gen.(ec) <- 1; (* gen := 1 *)
    for i = 0 to ec - 1 do (* do gen := gen * (x - g^i) *)
      let gi = Gf_256.exp f i in
      for j = 0 to ec - 1 do
        gen.(j) <- Gf_256.sub gen.(j + 1) (Gf_256.mul f gen.(j) gi)
      done;
      gen.(ec) <- Gf_256.mul f gen.(ec) gi (* g^0 * g^1 * ... * g^ec *)
    done;
    gen

  (* The following function computes the remainder of [p], the message
     padded with [ec] zero bytes, divided by [gen] by repeatedly
     subtracting multiples of [gen].

    let encode f ~ec ~gen p =
      for i = 0 to Bytes.length p - ec - 1 (* iterate on message bytes *) do
        (* do p := p - (p.(i) / gen.(0)) * gen  *)
        let k = Gf_256.mul f (Bytes.get p i) (Gf_256.inv f gen.(0)) in
        for j = 0 to Array.length gen - 1 do
          let ci = i + j in
          let c = Bytes.get p ci in
          Bytes.set p ci (Gf_256.sub c (Gf_256.mul f k gen.(j)))
        done
      done

     If we shortcut when [pi = 0] and [gen.(j) = 0], note that gen.(0) is
     always 1, inline definitions and simplify we get to the different
     more efficient function below, which works directly on the log
     of [gen]'s coefficients. *)

  let log_gen f ~ec =
    let gen = gen f ~ec in
    for i = 0 to ec do gen.(i) <- Gf_256.log f gen.(i) done;
    gen

  let encode (f : Gf_256.t) ~ec ~log_gen p =
    for i = 0 to Bytes.length p - ec - 1 do
      let pi = Bytes.get p i in
      if pi = 0 then () else
      let log_k = f.log.(pi) in
      for j = 0 to Array.length log_gen - 1 do
        let log_genj = log_gen.(j) in
        if log_genj = 255 (* => gen.(j) = 0 *) then () else
        let ci = i + j in
        let c = Bytes.get p ci in
        Bytes.set p ci (Gf_256.sub c (f.exp.(log_k + log_genj)))
      done
    done
end

(* QR matrices *)

module Matrix = struct
  type bits = Bits.t
  type t = { w : int; bits : bits; }
  let zero ~w = let bits = Bits.create (w * w) false in { w; bits }
  let copy m = { m with bits = Bits.copy m.bits }
  let of_bits ~w bits =
    let len = div_round_up (w * w) 8 in
    if Bits.length bits >= len then { w; bits } else
    invalid_arg "Not enough bits provided"

  let w m = m.w
  let bits m = m.bits
  let[@inline] get m ~x ~y = Bits.get m.bits ((y * m.w) + x)
  let[@inline] _set m ~x ~y v = Bits.set m.bits ((y * m.w) + x) v
  let[@inline] set m ~x ~y = _set m ~x ~y true
  let[@inline] clear m ~x ~y = _set m ~x ~y false
  let fold f m acc =
    let acc = ref acc in
    for y = 0 to m.w - 1 do
      for x = 0 to m.w - 1 do acc := f x y (get m ~x ~y) !acc done
    done;
    !acc

  let set_square_frame m ~x ~y (* top-left corner *) ~w =
    let xmin = x and ymin = y and xmax = x + w - 1 and ymax = y + w - 1 in
    for x = xmin to xmax do set m ~x ~y:ymin done; (* top seg *)
    for x = xmin to xmax do set m ~x ~y:ymax done; (* bot seg *)
    for y = ymin + 1 to ymax - 1 do set m ~x:xmin ~y done; (* left seg *)
    for y = ymin + 1 to ymax - 1 do set m ~x:xmax ~y done (* right seg *)

  let to_svg ?(w_mm = 50) ?(invert = false) ?(quiet_zone = true) m =
    let w_mm = string_of_int w_mm in
    let w = string_of_int @@ if quiet_zone then m.w + 8 else m.w in
    let on, off = if invert then "white", "black" else "black", "white" in
    let ( ++ ) acc d = d :: acc in
    let acc =
      [] ++ "<svg xmlns='http://www.w3.org/2000/svg' version='1.1' " ++
      "width='" ++ w_mm ++ "mm' " ++ "height='" ++ w_mm ++ "mm' " ++
      "viewBox='0 0 " ++ w ++ " " ++ w ++ "'>\n" ++
      " <rect width='" ++ w ++ "' height='" ++ w ++ "' fill='" ++ off ++
      "'/>" ++ " <path fill='" ++ on ++ "' d='"
    in
    let on ~x ~y acc =
      acc ++ "\n M " ++ string_of_int x ++ "," ++
      string_of_int y ++ " l 1,0 0,1 -1,0 z"
    in
    let acc = ref acc in
    let pad = if quiet_zone then 4 else 0 in
    for y = 0 to m.w - 1 do
      for x = 0 to m.w - 1 do
        if (get m ~x ~y) then (acc := on ~x:(pad + x) ~y:(pad + y) !acc)
      done;
    done;
    String.concat "" (List.rev (!acc ++ "\n' /></svg>"))
end

(* QR code properties *)

type version = [ `V of int ]
type mode = [ `Byte ]
type ec_level = [ `L | `M | `Q | `H ]

module Prop = struct
  let version_min = 1
  let version_max = 40
  let version_to_w = function `V v -> 21 + 4 * (v - 1)
  let version_of_w w =
    let err w = Error ("QR code width cannot be " ^ string_of_int w) in
    match w with
    | w when 21 <= w && w <= 177 ->
        let base = w - 21 in
        if base mod 4 <> 0 then err w else Ok (`V (base / 4 + 1))
    | w -> err w

  (* Alignement patterns, functions allow to recover the data of table E.1 in
     N.B. this assumes version > 1 *)

  let align_pat_count (`V v) = (* along one dimension *) (v / 7) + 2
  let align_pat_first = 6
  let align_pat_last v = version_to_w v - 7
  let align_pat_delta (`V version as v) =
    (* center to center distance from right to left or bottom up. The last
       hop to column/row 6 absorbs the unevenness. *)
    if version = 32 then 26 (* odd exception: we compute 28 *) else
    let s = (align_pat_last v - align_pat_first) in
    let d = div_round_up s (align_pat_count v - 1) in
    if d mod 2 = 1 then d + 1 else d

  let align_pat_center ~pat_count ~pat_last ~pat_delta i =
    (* ith pattern from left to right or top to botom  *)
    if i = 0 then 6 else pat_last - pat_delta * ((pat_count - 1) - i)

  (* Capacity *)

  let total_modules (`V version as v) =
    (* 'Data modules except (C)' in table 1 *)
    let version_w = version_to_w v in
    let all_mods = version_w * version_w in
    let finder_mods = 3 * 64 in
    let timing_mods = 2 * (version_w - 16) in
    let align_pats_mods =
      if version = 1 then 0 else
      let align_pat_count = align_pat_count v in
      let align_pats = max 1 ((align_pat_count * align_pat_count) - 3) in
      let align_pats_and_timing_overlap = 2 * (5 * (align_pat_count - 2)) in
      align_pats * 25 - align_pats_and_timing_overlap
    in
    let version_mods = if version >= 7 then 2 * 18 else 0 in
    let format_info_mods = 2 * 15 in
    let lone_dark_mod = 1 in (* see figure 25 *)
    all_mods - finder_mods - timing_mods - align_pats_mods - version_mods -
    format_info_mods - lone_dark_mod

  let total_bytes v =
    (* 'Total number of codewords' in table 9  *)
    (total_modules v) / 8

  (* Error correction block specification, per ec level.

     Given the number of ec codeword per level and the number of blocks.
     We can recover the number of data code words per block and the block
     layout (first group and potentially second group with one more byte). *)

  let ec_level_idx = function `L -> 0 | `M -> 1 | `Q -> 2 | `H -> 3
  let ec_block_spec =
    (* Table 9, 'Number of error correction codewords' and
       'Number of error correction blocks' by increasing version and ec_level.
       This is the only property we did not manage to derive from structural
       principles of QR codes. *)
    [| 7;1; 10;1; 13;1; 17;1;
       10;1; 16;1; 22;1; 28;1;
       15;1; 26;1; 36;2; 44;2;
       20;1; 36;2; 52;2; 64;4;
       26;1; 48;2; 72;4; 88;4;
       36;2; 64;4; 96;4; 112;4;
       40;2; 72;4; 108;6; 130;5;
       48;2; 88;4; 132;6; 156;6;
       60;2; 110;5; 160;8; 192;8;
       72;4; 130;5; 192;8; 224;8;
       (* *)
       80;4; 150;5; 224;8; 264;11;
       96;4; 176;8; 260;10; 308;11;
       104;4; 198;9; 288;12; 352;16;
       120;4; 216;9; 320;16; 384;16;
       132;6; 240;10; 360;12; 432;18;
       144;6; 280;10; 408;17; 480;16;
       168;6; 308;11; 448;16; 532;19;
       180;6; 338;13; 504;18; 588;21;
       196;7; 364;14; 546;21; 650;25;
       224;8; 416;16; 600;20; 700;25;
       (* *)
       224;8; 442;17; 644;23; 750;25;
       252;9; 476;17; 690;23; 816;34;
       270;9; 504;18; 750;25; 900;30;
       300;10; 560;20; 810;27; 960;32;
       312;12; 588;21; 870;29; 1050;35;
       336;12; 644;23; 952;34; 1110;37;
       360;12; 700;25; 1020;34; 1200;40;
       390;13; 728;26; 1050;35; 1260;42;
       420;14; 784;28; 1140;38; 1350;45;
       450;15; 812;29; 1200;40; 1440;48;
       (* *)
       480;16; 868;31; 1290;43; 1530;51;
       510;17; 924;33; 1350;45; 1620;54;
       540;18; 980;35; 1440;48; 1710;57;
       570;19; 1036;37; 1530;51; 1800;60;
       570;19; 1064;38; 1590;53; 1890;63;
       600;20; 1120;40; 1680;56; 1980;66;
       630;21; 1204;43; 1770;59; 2100;70;
       660;22; 1260;45; 1860;62; 2220;74;
       720;24; 1316;47; 1950;65; 2310;77;
       750;25; 1372;49; 2040;68; 2430;81; |]

  let ec_bytes (`V version) ec_level =
    (* 'Number of error correction codewords' in table 9 *)
    ec_block_spec.((version - 1) * 8 + 2 * (ec_level_idx ec_level))

  let ec_blocks (`V version) ec_level =
    (* 'Number of error correction blocks' in table 9 *)
    ec_block_spec.((version - 1) * 8 + 2 * (ec_level_idx ec_level) + 1)

  let data_bytes version ec_level =
    (* 'Number of data codewords' in table 7 *)
    (total_bytes version) - (ec_bytes version ec_level)

  let character_count_bits (`V version) = function
  | `Byte -> if version <= 9 then 8 else 16

  let mode_capacity version ec_idx mode =
    (* 'Data capacity' in table 7 *)
    let bits = 8 * data_bytes version ec_idx in
    let bits = bits - 4 (* mode indicator *) in
    let bits = bits - character_count_bits version mode  in
    match mode with
    | `Byte -> bits / 8

  let field = lazy (Gf_256.create ~r:0b100011101 ~g:2) (* see 7.5.2 *)
  let gen field ~ec = Rs.gen field ~ec (* exposed for testing *)
end

(* Encoding

   https://www.thonky.com/qr-code-tutorial/ may be useful to understand the
   encoding process. *)

let encode_padding b ~first ~last = (* see 7.4.10 *)
  (* Pads with alternating 236 and 17 from ~first to ~last. *)
  let rec set_236 i last =
    if i > last then () else (Bytes.set b i 236; set_17 (i + 1) last)
  and set_17 i last =
    if i > last then () else (Bytes.set b i 17; set_236 i last)
  in
  set_236 first last

let encode_with_byte_mode v mode ~enc_len s = (* see 7.4 and 7.4.5 *)
  (* N.B. we can work around working at the bit level. Half bytes do it. *)
  let len = String.length s in
  let b = Bytes.create ~len:enc_len 0 in
  let cc = Prop.character_count_bits v mode in
  let get = Bytes.get and set = Bytes.set in
  set b 0 ((0b0100_0000) (* mode indicator *)
           lor (len lsr cc - 4)) (* hi four bits of len *);
  let half_data_start = match cc with
  | 8 ->
      set b 0 (0b0100_0000 lor ((len lsr 4) land 0xF));
      set b 1 ((len land 0xF) lsl 4);
      1
  | 16 ->
      set b 0 (0b0100_0000 lor ((len lsr 12) land 0xF));
      set b 1 ((len lsr 4) land 0xFF);
      set b 2 ((len land 0xF) lsl 4);
      2
  | _ -> assert false
  in
  let half_data_end = half_data_start + len - 1 + 1 in
  for i = 0 to len - 1 do
    let v = Char.code (String.get s i) in
    let k = half_data_start + i in
    set b k (get b k lor ((v lsr 4) land 0xF));
    set b (k + 1) ((v land 0xF) lsl 4);
  done;
  (* We always end up on a half-byte, add 0b0000 terminator (see 7.4.9) *)
  set b half_data_end (get b half_data_end land 0xF0);
  encode_padding b ~first:(half_data_end + 1) ~last:(enc_len - 1);
  b

let encode_with_mode v ec_level mode s = (* see 7.4 *)
  let enc_len = Prop.data_bytes v ec_level in
  match mode with
  | `Byte -> encode_with_byte_mode v mode ~enc_len s

(* Function modules matrix setters *)

let set_finder_patterns m = (* see 6.6.3 *)
  let set_finder_pattern m ~x ~y (* top-left *) =
    Matrix.set_square_frame m ~x ~y ~w:7;
    Matrix.set_square_frame m ~x:(x + 2) ~y:(y + 2) ~w:3;
    Matrix.set m ~x:(x + 3) ~y:(y + 3)
  in
  set_finder_pattern m ~x:0 ~y:0;
  set_finder_pattern m ~x:0 ~y:(Matrix.w m - 7);
  set_finder_pattern m ~x:(Matrix.w m - 7) ~y:0

let set_timing_patterns m = (* see 6.3.5 *)
  let max = Matrix.w m - 9 in
  for x = 8 to max do if x mod 2 = 0 then Matrix.set m ~x ~y:6 done;
  for y = 8 to max do if y mod 2 = 0 then Matrix.set m ~x:6 ~y done

let set_alignment_patterns (`V version as v) m = (* see 6.3.6 and annex E *)
  if version = 1 then () else
  let alignment_pattern m ~x ~y (* top-left *) =
    Matrix.set_square_frame m ~x ~y ~w:5;
    Matrix.set m ~x:(x + 2) ~y:(y + 2)
  in
  let pat_count = Prop.align_pat_count v in
  let pat_last = Prop.align_pat_last v in
  let pat_delta = Prop.align_pat_delta v in
  let max = pat_count - 1 in
  for i = 0 to max do
    for j = 0 to max do
      (* skip if overlaps with finder patterns *)
      if (i = 0 && j = 0) || (i = 0 && j = max) || (i = max && j = 0) then ()
      else
      let x = (Prop.align_pat_center ~pat_count ~pat_last ~pat_delta) i - 2 in
      let y = (Prop.align_pat_center ~pat_count ~pat_last ~pat_delta) j - 2 in
      alignment_pattern m ~x ~y
    done
  done

(* Format information matrix setter *)

let encode_format_information ec_level mask = (* see 7.9 & C.2 *)
  let ec_bits = match ec_level with
  | `L -> 0b01 | `M -> 0b00 | `Q -> 0b11 | `H -> 0b10
  in
  let data = (ec_bits lsl 13) lor (mask lsl 10) in
  let g = 0b10100110111 in
  let rem = ref data in
  for i = 14 downto 10 do
    if (!rem land (1 lsl i)) <> 0 then rem := !rem lxor (g lsl (i - 10));
  done;
  (data lor !rem) lxor 0b101010000010010

let set_format_information ec_level mask m = (* see 7.9 *)
  let data = encode_format_information ec_level mask in
  for i = 0 to 14 do
    let set = (data lsr i) land 1 = 1 in
    if not set then () else begin
      (* set horizontally, see figure 25 *)
      (if i < 8 then Matrix.set m ~x:(Matrix.w m - 1 - i) ~y:8 else
       if i < 9 then Matrix.set m ~x:(15 - i) ~y:8 else
       Matrix.set m ~x:(15 - i - 1) ~y:8);
      (* set vertically, see figure 25 *)
      (if i < 6 then Matrix.set m ~x:8 ~y:i else
       if i < 8 then Matrix.set m ~x:8 ~y:(i + 1) else
       Matrix.set m ~x:8 ~y:(Matrix.w m - (15 - i)))
    end;
  done;
  Matrix.set m ~x:8 ~y:(Matrix.w m - 8) (* dark module, see figure 25 *)

(* Version information matrix setter *)

let encode_version (`V version) = (* see 7.10 & D.2 *)
  let data = version lsl 12 in
  let g = 0b1111100100101 in
  let rem = ref data in
  for i = 17 downto 12 do
    if (!rem land (1 lsl i)) <> 0 then rem := !rem lxor (g lsl (i - 12));
  done;
  data lor !rem

let set_version_information (`V version as v) m = (* see 7.10 *)
  if version < 7 then () else
  let data = encode_version v in
  let delta = Matrix.w m - 8 - 3 in
  for i = 0 to 17 do
    let set = (data lsr i) land 1 = 1 in
    if not set then () else begin
      let x = i mod 3 and y = i / 3 in
      Matrix.set m ~x:(delta + x) ~y;  (* top right *)
      Matrix.set m ~x:y ~y:(delta + x) (* bottom left *)
    end
  done

(* Data and error correction matrix setter *)

let encode_ec v ec_level data =
  let total_bytes = Prop.total_bytes v in
  let block_count = Prop.ec_blocks v ec_level in
  let longer_block_count = total_bytes mod block_count in
  let normal_block_count = block_count - longer_block_count in
  let[@inline] is_longer_block i = i >= normal_block_count  in
  let ec_len = Prop.ec_bytes v ec_level / block_count in
  let data_len = (total_bytes / block_count) - ec_len in
  let blocks = (* see 7.5.2 and table 9 *)
    let start = ref 0 in
    let create_block i =
      let data_len = data_len + if (is_longer_block i) then 1 else 0 in
      let block_len = data_len + ec_len in
      let block = Bytes.create ~len:block_len 0 in
      Bytes.blit ~src:data !start ~dst:block 0 ~len:data_len;
      start := !start + data_len;
      block
    in
    Array.init block_count create_block
  in
  (* Data in blocks will be zeroed by RS computation so we allocate
     the data in the final bit stream now. See 7.6 and figure 15. *)
  let bits = Bytes.create ~len:total_bytes 0 in
  let byte = ref 0 in
  for i = 0 to (data_len + 1) - 1 do
    for b = 0 to block_count - 1 do
      if i = data_len && not (is_longer_block b) then () else
      (Bytes.set bits !byte (Bytes.get blocks.(b) i); incr byte)
    done
  done;
  (* Compute the error correction bytes in the blocks. *)
  let f = Lazy.force Prop.field in
  let log_gen = Rs.log_gen f ~ec:ec_len in
  Array.iter (Rs.encode f ~ec:ec_len ~log_gen) blocks;
  (* Allocate the error correction data in the bit stream (figure 15) *)
  for i = 0 to ec_len - 1 do
    for b = 0 to block_count - 1 do
      let i = Bytes.length blocks.(b) - ec_len + i in
      (Bytes.set bits !byte (Bytes.get blocks.(b) i); incr byte)
    done
  done;
  assert (!byte = total_bytes); (* check we used up all QR bytes *)
  bits

let mask_fun = function (* see table 10 *)
| 0 -> fun ~x ~y -> (y + x) mod 2 = 0
| 1 -> fun ~x:_ ~y -> y mod 2 = 0
| 2 -> fun ~x ~y:_ -> x mod 3 = 0
| 3 -> fun ~x ~y -> (y + x) mod 3 = 0
| 4 -> fun ~x ~y -> ((y / 2) + (x / 3)) mod 2 = 0
| 5 -> fun ~x ~y -> let yx = (y * x) in (yx mod 2) + (yx mod 3) = 0
| 6 -> fun ~x ~y -> let yx = (y * x) in ((yx mod 2) + (yx mod 3)) mod 2 = 0
| 7 -> fun ~x ~y -> (((y + x) mod 2) + ((y * x) mod 3)) mod 2 = 0
| _ -> assert false

let set_data mask (`V version as v) data m = (* see 7.7.3 *)
  let mask_fun = mask_fun mask in
  let w = Matrix.w m in
  let skip_version_info = version >= 7 in
  let[@inline] skip_function_pats ~x ~y =
    (skip_version_info && (* skip version info if version >= 7 *)
     ((y <= 5 && x >= w - 11) ||
      (x <= 5 && y >= w - 11))) ||
    y = 6 (* skip horizontal timing pattern *) ||
    (y <= 8 && (x <= 8 || x >= w - 8)) (* skip top finders *) ||
    (x <= 8 && y >= w - 8) (* skip bottom left finder *)
  in
  let max = w - 1 in
  let x = ref max in
  let y = ref max in
  let move = ref (-1) in (* negative moves up, positive moves down *)
  (* We go over the code from right to left up and down by 2-module wide
     columns and allocate the data bits on data modules (remaining unused
     ones are set to false). *)
  let bit = ref 0 in
  let bit_max = Bits.length data - 1 in
  while (!x >= 0) do
    while (0 <= !y && !y <= max) do
      for i = 0 to 1 do (* right and left modules of column row *)
        let x = !x - i in
        if (skip_function_pats[@inlined]) ~x ~y:!y then () else
        let skip_align_pat =
          (* The left module of columns is always handled by jumps when
             i = 0. This relies on the fact that pattern centers are always
             even. So on the left module (i = 1) there's nothing to skip. *)
          if i = 1 then false else
          if not (Matrix.get ~x ~y:!y m) then false else
          (* We hit an alignment pattern. *)
          let left = Matrix.get ~x:(x - 1) ~y:!y m in
          (* If left is also [true] we are not on the left edge of the
             the pattern so we skip the pattern height for the column
             and proceed directly with the new column row (for that
             row skip_base_function_patterns is guaranteed to be [false]). *)
          if left then (y := !y + 5 * !move; false) else
          true (* just skip the edge for the right module of the column. *)
        in
        if skip_align_pat then () else
        let v = if !bit <= bit_max then Bits.get data !bit else false in
        let v = if mask_fun ~x ~y:!y then not v else v in
        if v then Matrix.set ~x ~y:!y m;
        incr bit;
      done;
      y := !y + !move;
    done;
    x := !x - 2; (* next 2-module wide column *)
    if !x = 6 then decr x; (* skip vertical timing pattern. *)
    move := !move * -1; y := !y + !move; (* switch direction *)
  done;
  assert (!bit = Prop.total_modules v); (* check we used up all QR modules *)
  ()

(* Matrix penalty computation, section 7.8.3.1. *)

let[@inline] n3_incr j i max m =
  (* n3 XXX this is slow and we could be smarter about the search here.
     Abstracting over dimension also makes us loose a bit of time. *)
  let acc = ref 0 in
  for k = 0 to 1 do (* Do the check in both dimensions. *)
    let swap = k = 1 in
    let i = if swap then j else i
    and j = if swap then i else j in
    let[@inline] get ~x ~y m =
      if swap
      then Matrix.get ~x:y ~y:x m
      else Matrix.get ~x ~y m
    in
    if j + 6 <= max then begin
      (* check for 1011101 at (j,i), (j,i) is already checked to be true) *)
      if not (get ~x:(j + 1) ~y:i m) &&
         get ~x:(j + 2) ~y:i m &&
         get ~x:(j + 3) ~y:i m &&
         get ~x:(j + 4) ~y:i m &&
         not (get ~x:(j + 5) ~y:i m) &&
         get ~x:(j + 6) ~y:i m
      then begin
        if j - 4 >= 0 then begin (* 0000 before *)
          let count = ref 0 in
          while (!count < 4 && not (get ~x:(j - !count - 1) ~y:i m))
          do incr count done; if !count = 4 then acc := !acc + 40
        end;
        if j + 6 + 4 <= max then begin (* 0000 after *)
          let count = ref 0 in
          while (!count < 4 && not (get ~x:(j + 6 + !count + 1) ~y:i m))
          do incr count done; if !count = 4 then acc := !acc + 40
        end
      end
    end
  done;
  !acc

let matrix_penalty m = (* see table 11 *)
  let max = Matrix.w m - 1 in
  let n1 = ref 0 in
  let n1_last_h = ref false and n1_acc_h = ref 0 in
  let n1_last_v = ref false and n1_acc_v = ref 0 in
  let n2 = ref 0 in
  let n3 = ref 0 in
  let n4_dark = ref 0 in
  for i = 0 to max do
    n1_last_h := Matrix.get ~x:0 ~y:i m; n1_acc_h := 0;
    n1_last_v := Matrix.get ~x:i ~y:0 m; n1_acc_v := 0;
    for j = 0 to max do
      (* n1: consecutive colors horizontaly or vertically *)
      let curr_h = Matrix.get ~x:j ~y:i m in
      if !n1_last_h = curr_h then incr n1_acc_h else begin
        if !n1_acc_h >= 5 then (n1 := !n1 + (!n1_acc_h - 2));
        n1_last_h := curr_h; n1_acc_h := 1
      end;
      let curr_v = Matrix.get ~x:i ~y:j m in
      if !n1_last_v = curr_v then incr n1_acc_v else begin
        if !n1_acc_v >= 5 then (n1 := !n1 + (!n1_acc_v - 2));
        n1_last_v := curr_v; n1_acc_v := 1
      end;
      (* n2: block of 2x2 of the same color *)
      if j < max && i < max then begin
        let v0 = curr_h in
        let j' = j + 1 in
        let v1 = Matrix.get ~x:j' ~y:i m in
        if v1 <> v0 then () else
        let i' = i + 1 in
        let v2 = Matrix.get ~x:j ~y:i' m in
        if v2 <> v1 then () else
        let v3 = Matrix.get ~x:j' ~y:i' m in
        if v3 <> v2 then () else (n2 := !n2 + 3)
      end;
      (* n3: find [0000]1011101[0000] *)
      if curr_h then (n3 := !n3 + n3_incr j i max m);
      (* n4: collect dark modules *)
      if curr_h then incr n4_dark;
    done;
  done;
  let n4 =
    let r = (float !n4_dark) /. float (Matrix.w m * Matrix.w m) in
    let dev = abs_float (50. -. r *. 100.) in
    truncate (dev /. 5.) * 10
  in
  !n1 + !n2 + !n3 + n4

(* Matrix encoding *)

let layout_function_modules v m =
  set_finder_patterns m;
  set_timing_patterns m;
  set_alignment_patterns v m;
  set_version_information v m

let layout_data version ec_level mask data m =
  set_format_information ec_level mask m;
  set_data mask version data m;
  ()

let base_matrix v =
  let m = Matrix.zero ~w:(Prop.version_to_w v) in
  layout_function_modules v m; m

let encode_matrix ?mask ~version:v ~ec_level ~mode s =
  let data = encode_with_mode v ec_level mode s in
  let data = encode_ec v ec_level data in
  let base_matrix = base_matrix v in
  let min_penalty = ref max_int in
  let min_matrix = ref base_matrix in
  let min_mask, max_mask = match mask with
  | Some mask -> mask, mask (* only try this mask *)
  | None -> 0, 7 (* all mask as per standard *)
  in
  for mask = min_mask to max_mask do
    let m = Matrix.copy base_matrix in
    layout_data v ec_level mask data m;
    let p = matrix_penalty m in
    if p < !min_penalty then (min_penalty := p; min_matrix := m)
  done;
  !min_matrix

(* Encoding *)

let find_mode ?mode _s = match mode with Some m -> m | None -> `Byte
let rec get_best_level_for_version v ~mode ~after ~need =
  (* Sometimes we can fit the same version with the next ec level *)
  let try_next next = match need <= Prop.mode_capacity v next mode with
  | true -> get_best_level_for_version v ~mode ~after:next ~need
  | false -> after
  in
  match after with
  | `L -> try_next `M | `M -> try_next `Q | `Q -> try_next `H | `H -> `H

let find_version ~version ~mode ~ec_level ~need = match version with
| Some v ->
    if need <= Prop.mode_capacity v ec_level mode
    then Some (v, get_best_level_for_version v ~mode ~after:ec_level ~need)
    else None
| None ->
    let rec loop (`V version as v) =
      if version > Prop.version_max then None else
      if need <= Prop.mode_capacity v ec_level mode
      then Some (v, get_best_level_for_version v ~mode ~after:ec_level ~need)
      else loop (`V (version + 1))
    in
    loop (`V Prop.version_min)

let encode ?mask ?version ?mode ?(ec_level = `M) s =
  let mode = find_mode ?mode s in
  match find_version ~version ~mode ~ec_level ~need:(String.length s) with
  | None -> None
  | Some (v, ec) -> Some (encode_matrix ?mask ~version:v ~ec_level:ec ~mode s)
