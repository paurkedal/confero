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

let mapping = Confero_ducet.mapping

(* Using unsafe variant since Uchar.of_int fails on 0xd800 which is present in
 * the test data. *)
let uchar_of_hex s = Uchar.unsafe_of_int (int_of_string ("0x" ^ s))

module Data = struct

  type t = {
    ic: in_channel;
    fp: string;
    mutable pos: int;
  }

  let open_in fp =
    let ic = open_in fp in
    {ic; fp; pos = 0}

  let rec read data =
    data.pos <- data.pos + 1;
    (match input_line data.ic with
     | exception End_of_file -> None
     | line ->
        let line = String.trim line in
        if line = "" || line.[0] = '#' then read data else
        String.split_on_char ' ' line
          |> List.map uchar_of_hex
          |> Option.some)

  let pp_pos ppf data = Fmt.pf ppf "%s:%d" data.fp data.pos
end

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

let dump ustr key =
  let ustr_nf = List.to_seq ustr |> map_uunf `NFD |> List.of_seq in
  let pp_uchar = Fmt.(using Uchar.to_int (fmt "%x")) in
  let pp_ustr = Fmt.(list ~sep:sp pp_uchar) in
  Fmt.epr "@[<h>- %a => %a => %a@]@."
    pp_ustr ustr pp_ustr ustr_nf Confero.Sort_key.pp key

let () =
  let error_count = ref 0 in
  let data = Data.open_in Sys.argv.(1) in
  let rec loop ustr' key' =
    (match Data.read data with
     | None -> ()
     | Some ustr ->
        let key = Confero.Sort_key.of_uchar_seq ~mapping (List.to_seq ustr) in
        let c = Confero.Sort_key.compare key' key in
        if c > 0 then begin
          incr error_count;
          if !error_count < 200 then begin
            Fmt.epr "%a: Compares less than previous line:@." Data.pp_pos data;
            dump ustr' key';
            dump ustr key
          end
        end;
        loop ustr key)
  in
  (match Data.read data with
   | None -> failwith "Empty test data."
   | Some ustr ->
      let key = Confero.Sort_key.of_uchar_seq ~mapping (List.to_seq ustr) in
      loop ustr key);
  if !error_count <> 0 then begin
    Fmt.epr "%d errors" !error_count;
    exit 2
  end
