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

module Collation_element = Collation_element
module Collation_mapping = Collation_mapping
module Sort_key = Sort_key

let identity_mapping ch =
  let ce = Collation_element.create [|Uchar.to_int ch|] in
  Collation_mapping.(Accept ([|ce|], fun _ -> Reject))

let generic_mapping = ref identity_mapping
let registry : (Iso639.Lang.t, Collation_mapping.t) Hashtbl.t = Hashtbl.create 7

let register_collation_mapping ?lang mapping =
  (match lang with
   | None -> generic_mapping := mapping
   | Some lang -> Hashtbl.replace registry lang mapping)

let infer_collation_mapping ?lang ?mapping () =
  let from_lang =
    (match lang with
     | None -> None
     | Some lang -> Hashtbl.find_opt registry lang)
  in
  (match from_lang, mapping with
   | Some m, _ | None, Some m -> m
   | None, None -> !generic_mapping)

let collate ?encoding ?lang ?mapping s1 s2 =
  let mapping = infer_collation_mapping ?lang ?mapping () in
  let k1 = Sort_key.of_string ?encoding ~mapping s1 in
  let k2 = Sort_key.of_string ?encoding ~mapping s2 in
  Sort_key.compare k1 k2
