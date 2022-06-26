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

open Prereq

type t = string

let equal = String.equal
let compare = String.compare

let pp ppf key =
  for i = 0 to String.length key - 1 do
    if i > 0 && i mod 2 = 0 then Format.pp_print_space ppf ();
    Format.fprintf ppf "%02x" (Char.code key.[i])
  done

let decode_uutf ?(encoding = `UTF_8) source =
  let decoder = Uutf.decoder ~encoding source in
  let rec loop () =
    (match Uutf.decode decoder with
     | `Await -> failwith "Bad source"
     | `Uchar och -> Seq.Cons (och, loop)
     | `End -> Seq.Nil
     | `Malformed msg -> failwith msg)
  in
  loop

let map_uunf form =
  let uunf = Uunf.create form in
  let rec loop iseq = function
   | `Await ->
      (match iseq () with
       | Seq.Nil -> loop iseq (Uunf.add uunf `End)
       | Seq.Cons (ich, iseq') -> loop iseq' (Uunf.add uunf (`Uchar ich)))
   | `Uchar och -> Seq.Cons (och, fun () -> loop iseq (Uunf.add uunf `Await))
   | `End -> Seq.Nil
  in
  fun iseq () -> loop iseq (Uunf.add uunf `Await)

let of_collation_elements ces =
  let ces = Array.of_seq ces in
  let buf = Buffer.create 256 in
  let add_at_level l ce = Collation_element.add_to_buffer ce l buf in
  let add_sep () = Buffer.add_char buf '\x00'; Buffer.add_char buf '\x00' in
  let depth = Array.fold_right (max % Collation_element.depth) ces 0 in
  if depth = 0 then "" else begin
    Array.iter (add_at_level 0) ces;
    for l = 1 to depth - 1 do
      add_sep ();
      Array.iter (add_at_level l) ces
    done;
    Buffer.contents buf
  end

let of_uchar_seq ~mapping useq = useq
  |> map_uunf `NFD
  |> Collation_mapping.run mapping
  |> of_collation_elements

let of_string ?encoding ~mapping s =
  decode_uutf ?encoding (`String s) |> of_uchar_seq ~mapping
