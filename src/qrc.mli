(*---------------------------------------------------------------------------
   Copyright (c) 2020 The qrc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** QR code encoder.

    Consult the {{!notes_limits}limitations} and an encoding
    {{!example}example}.

    {b References.}

    ISO/IEC 18004:2015. {e QR Code bar code symbology specification}. *)

(** {1:matrices Matrices} *)

(** QR 2D matrices. *)
module Matrix : sig

  type bits =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** The type for sequence of bits. *)

  type t
  (** The type for 2D square matrices of {e modules} (binary pixels).

      For a matrix of width [w], the module at position ([0,0]) is the
      top-left corner, and ([w-1,w-1)] is the bottom-right corner. The quiet
      zone (surrounding frame of four zero modules) is not included. We
      store the bits in row-major order. *)

  val zero : w:int -> t
  (** [zero ~w] is a matrix of zeros of size [w]. *)

  val of_bits : w:int -> bits -> t
  (** [of_bits ~w bits] is a matrix from the sequence of bits [bits]
      interpreted in row-major order. Exceeding final bits are ignored.

      @raise Invalid_argument if less than [w]{^2} bits are provided. *)

  val w : t -> int
  (** [w m] is the matrix width. *)

  val bits : t -> bits
  (** [bits m] are the matrix's bits (modules) in row-major order. If
      there are more bits than [(w m) * (w m)], exceeding final bits
      should be ignored. *)

  val copy : t -> t
  (** [copy m] is a copy of [m]. *)

  val get : t -> x:int -> y:int -> bool
  (** [get m ~x ~y] is the [(x,y)] module of [m]. *)

  val set : t -> x:int -> y:int -> unit
  (** [set m ~x ~y v] sets the [(x,y)] module of [m] to [true]. *)

  val clear : t -> x:int -> y:int -> unit
  (** [clear m ~x ~y] sets the [(x,y)] module of [m] to [false]. *)

  val fold : (int -> int -> bool -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f m acc] folds [f] over all the modules [(x,y)] of [m] as [f x
      y (get m ~x ~y) acc'] in row-major order and starting with [acc]. *)

  val to_svg : ?w_mm:int -> ?invert:bool -> ?quiet_zone:bool -> t -> string
  (** [to_svg ~w_mm ~invert ~quiet_zone m] is an SVG image for
      [m] in a coordinate system using one (slightly larger) unit
      black square per set module. The image, including the quiet zone, has a
      width and height of [w_mm] millimeters (defaults to [50]).
      If [invert] is [true] (defaults to [false]) black modules are
      white and vice-versa. If [quiet_zone] is [true] (default) the
      surrounding frame of four zero modules is included. *)
end

(** {1:props Properties} *)

type version = [ `V of int ]
(** The type for QR code versions, from 1 to 40. *)

type ec_level =
  [ `L (** 7% *) | `M (** 15% *) | `Q (** 25% *) | `H (** 30% *) ]
(** The type for QR code error correction levels. Four levels respectively
    allowing 7%, 15%, 25% and 30% recovery of the QR code. *)

type mode = [ `Byte (** byte data *) ]
(** The type for (supported) data encoding modes. *)

(** Arithmetic over galois field GF(2{^8}). *)
module Gf_256 : sig

  type poly = int
  (** The type for degree 8 polynomials, using 0x1FF bits.  The
      [i]th zero-based bit index represents the coefficient (0 or 1) of
      the [i]th degree variable. *)

  type byte = int
  (** The type for bytes, the elements of GF(2{^8}). *)

  type t
  (** A galois field instance for a given polynomial and generator. *)

  val create : r:poly -> g:byte -> t
  (** [create ~r ~g] is a field modulo polynomial [r] (must be
      irreducible) and generator [g]. [g] is the base for {!exp} and
      {!log}. *)

  val add : byte -> byte -> byte
  (** [add x y] is [y] added (xored) to [x]. *)

  val sub : byte -> byte -> byte
  (** [sub x y] is [y] subtracted (xored) to [x]. *)

  val exp : t -> byte -> byte
  (** [exp f x] is the base [g] exponential [g]{^[x]} in [f]. *)

  val log : t -> byte -> byte
  (** [log f x] is [y] the base [g] logarithm of [x], i.e.
      [g]{^[y]} = [x]. This is [255] if [x = 0]. *)

  val mul : t -> byte -> byte -> byte
  (** [mul f x y] multiples [x] by [y] modulo [f]'s [r] polynomial. *)

  val inv : t -> byte -> byte
  (** [inv f x] is [x]{^-1}, the multiplicative inverse of [x]. This is
        [0] if [x] is [0]. *)
end

(** QR code properties.

    Except for {!mode_capacity}, not for the casual user. *)
module Prop : sig

  (** {1:version Version} *)

  val version_of_w : int -> (version, string) result
  (** [version_of_w w] is the version associated to a matrix width of [w]
      (without the quiet zone). *)

  val version_to_w : version -> int
  (** [version_to_w v] is the matrix width (without the quiet zone)
      for a QR code of version [v]. The result is between 21 to 177,
      increasing by 4 with each version. *)

  (** {1:capacity Capacity} *)

  val total_bytes : version -> int
  (** [total_bytes v] is the number of bytes available to encode data
      in a QR code of version [v]. This includes the bytes used for
      error correction. For all [ec_level] this is equal to {!data_bytes}[
      v ec_level] + {!ec_bytes}[ v ec_level].

      This is ‘Total number of codewords’ in table 9 of
      ISO/IEC 18004:2015. *)

  val data_bytes : version -> ec_level -> int
  (** [data_bytes v ec_level] is the number of bytes that can be used
      for data in a QR code of version [v] with error correction level
      [ec_level]

      This is the ‘Number of data codewords’ in table 7 of
      ISO/IEC 18004:2015.

      {b Warning.} Encoding the data into the actual
      {!mode} uses a few additional bytes from these bytes. Use
      {!mode_capacity} to determine the actual number of letters of
      the given mode you can encode in a QR code. *)

  val ec_bytes : version -> ec_level -> int
  (** [ec_bytes v ec_level] is the number of bytes used for error correction
      in a QR code of version [v] with error correction level [ec_level].

      This is the ‘Number of error correction codewords’ in table 9 of
      ISO/IEC 18004:2015. *)

  val ec_blocks : version -> ec_level -> int
  (** [ec_blocks v ec_level] is the number of blocks by which the
      data bytes to encode have to be divided and on which Reed-Solomon
      error correction is performed.

      This is the ‘Number of error correction blocks’ in table 9 of
      ISO/IEC 18004:2015. *)

  val mode_capacity : version -> ec_level -> mode -> int
  (** [mode_capacity v ec_level mode] is the number of [mode]
      {e letters} (that is bytes for [`Byte]) that can be encoded in a QR code
      of version [v] and error correction [ec_level].

      This is the ‘Data capacity’ columns in table 7 of
      ISO/IEC 18004:2015. *)

  (** {1:align_pats Alignment patterns}

      {b Note.} The result of the functions below for [`V 1] is undefined. *)

  val align_pat_count : version -> int
  (** [align_pat_count v] is the (maximal) number of alignement
      patterns along one dimension; square that and retract 3 to get the
      total. *)

  val align_pat_last : version -> int
  (** [align_pat_last v] is the [x] coordinate of the center of the last
      (rightmost) alignement pattern in version [v]. *)

  val align_pat_delta : version -> int
  (** [align_pat_delta v] is the distance between the centers of alignement
      patterns from right to left, or bottom to top; except for the last
      hop to column 6 or row 6 which absorbs the unevenness. *)

  val align_pat_center :
    pat_count:int -> pat_last:int -> pat_delta:int -> int -> int
  (** [align_pat_center ~pat_count ~pat_last ~pat_delta i] is the
      center of the [i]th alignement pattern starting from [0] and
      counting from left to right or top to bottom.

      This computes the values ‘Row/Columns coordinates of center module’
      in table E.1 of ISO/IEC 18004:2015. *)

  (** {1:gf Galois field} *)

  val field : Gf_256.t Lazy.t
  (** [field] is the Galois field used for error correction in QR codes,
      the polynomial is 0b100011101 and the generator is [2]. *)

  val gen : Gf_256.t -> ec:int -> Gf_256.byte array
  (** [gen f ec] is are the coefficients of the generator
      polynomial (x - [g]{^0})·(x - [g]{^1})·…·(x - [g]{^[ec]})
      with [g] the generator of [f] and [ec] the number of error
      correcting bytes . *)
end

(** {1:encoding Encoding} *)

val encode :
  ?mask:int -> ?version:version -> ?mode:mode -> ?ec_level:ec_level ->
  string -> Matrix.t option
(** [encode ~version ~mode ~ec_level data] is a QR matrix encoding bytes
    [data] with:
    {ul
    {- [ec_level], the error correction level, defaults to [`M].}
    {- [version], the version of the QR code. If unspecified the minimal
       needed version for the given [ec_level] is used.}
    {- [mode] is the encoding mode. If unspecified it is guessed, but
        guess what, only [`Byte] mode is supported.}
    {- [mask] can be used to force the data mask. This should not
       be used, the best mask to please your scanner is automatically
       selected as mandated by the standard.}}

    [None] is returned if [data] is too large to be fit the specified
    QR code parameters. Use {!Prop.mode_capacity} to find data
    capacity for given QR code properties beforehand.

    {b Note.} Sometimes once the [version] and [ec_level] constraints
    are satisfied for [data] we can fit the data in a higher
    [ec_level] with the same [version] (i.e. same matrix width). In
    that case that higher error correction level is used instead of
    the given or default [ec_level]. *)

(** {1:notes_limits Notes and limitations}

    {2:encoding Encoding}

    {ul
    {- Only the (universal) byte mode data encoding is supported. This may
       mean larger QR codes if you are trying to encode only decimal
       digits, only decimal digits and uppercase US-ASCII letters
       or only Kanji characters.}
    {- For textual data, using UTF-8 with [`Bytes] should work reasonably
       well. The module does not implement the ECI scheme to specify the
       encoding. Signals from the interwebs seem to indicate it's better
       to let scanners auto-discover the encoding as some do not
       understand the ECI scheme.}
    {- Structured append, i.e. data represented by up to 16 linked QR codes,
       is not supported – who does ?}} *)

(** {1:example Example}

    The following generates a QR code matrix for the given [data] bytes
    and outputs it as an SVG image on [stdout].
{[
  let output_svg_qr data = match Qrc.encode data with
  | None -> prerr_endline "Data capacity exceeded!"
  | Some m -> print_endline (Qrc.Matrix.to_svg m)
]}
*)
