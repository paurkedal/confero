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

open Allkeys

let implicit wA wB =
  let elements = [|
    E.create [|wA; 0x20; 0x2|];
    E.create [|wB|];
  |] in
  M.Accept (elements, rej)

let fallback ch =
  let k = Uchar.to_int ch in
  if 0x17000 <= k && k <= 0x18AFF || 0x18D00 <= k && k <= 0x18D8F then
    implicit 0xFB00 ((k - 0x17000) lor 0x8000)
  else
  if 0x1B170 <= k && k <= 0x1B2FF then
    implicit 0xFB01 ((k - 0x1B170) lor 0x8000)
  else
  if 0x18B00 <= k && k <= 0x18CFF then
    implicit 0xFB02 ((k - 0x18B00) lor 0x8000)
  else

  let w0 =
    if Uucp.Cjk.is_unified_ideograph ch then
      if 0x4e00 <= k && k < 0x10000 then 0xfb40 else 0xfb80
    else
      0xFBC0
  in
  implicit (w0 + k lsr 15) (k land 0x7FFF lor 0x8000)

let mapping ch =
  (match Allkeys.mapping ch with
   | M.Accept _ as node -> node
   | M.Reject -> fallback ch)
