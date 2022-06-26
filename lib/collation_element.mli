(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

type weight = int

type t = private string

val is_variable : t -> bool
(** True iff the collation element is variable. *)

val ignorable_depth : t -> int
(** The level of the first non-ignorable weight. *)

val depth : t -> int
(** The total number of weights including initial ignorable weights. *)

val add_to_buffer : t -> int -> Buffer.t -> unit

val create : ?is_variable: bool -> weight array -> t

val [@inline always] decode_v1 : string -> t

val [@inline always] encode_v1 : t -> string
