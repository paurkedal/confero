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

type t = string

let is_variable ce = Char.code ce.[0] land 0x80 <> 0

let ignorable_depth ce = Char.code ce.[0] land 0x3f

let depth ce = ignorable_depth ce + (String.length ce - 1) / 2

let add_to_buffer ce level buf =
  let d = ignorable_depth ce in
  if level >= d then begin
    let offset = 2 * (level - d) + 1 in
    if offset + 1 < String.length ce then
      let ch0, ch1 = ce.[offset], ce.[offset + 1] in
      if ch0 <> '\x00' || ch1 <> '\x00' then begin
        Buffer.add_char buf ch0;
        Buffer.add_char buf ch1
      end
  end

let create ?(is_variable = false) arr =
  let n_arr = Array.length arr in
  let n_ign =
    let rec loop i = if i = n_arr || arr.(i) <> 0 then i else loop (i + 1) in
    loop 0
  in
  let elt0 =
    Char.chr (n_ign lor (if is_variable then 0x80 else 0))
  in
  let elt i =
    if i = 0 then elt0 else
    Char.chr (arr.(n_ign + (i - 1) / 2) lsr (8 * (i mod 2)) land 0xff)
  in
  String.init (1 + 2 * (n_arr - n_ign)) elt

let [@inline always] decode_v1 x = x
let [@inline always] encode_v1 x = x
