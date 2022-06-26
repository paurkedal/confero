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

let (%) f g x = f (g x)

let rec readlines ic acc =
  (match input_line ic with
   | exception End_of_file -> List.rev acc
   | line -> readlines ic (line :: acc))

let mapping = Confero_ducet.mapping

let keys () =
  readlines stdin [] |> List.iter begin fun word ->
    let key = (Confero.Sort_key.of_string ~mapping word :> string) in
    for i = 0 to String.length key - 1 do
      if i > 0 && i mod 2 = 0 then Printf.printf " ";
      Printf.printf "%02x" (Char.code key.[i])
    done;
    Printf.printf " # %s\n" word
  end

let keys_cmd =
  let open Cmdliner in
  let term = Term.(const keys $ const ()) in
  let info = Cmd.info "keys" in
  Cmd.v info term

let sort () =
  readlines stdin []
    |> List.sort (Confero.collate ~mapping)
    |> List.iter (Printf.printf "%s\n")

let sort_cmd =
  let open Cmdliner in
  let term = Term.(const sort $ const ()) in
  let info = Cmd.info "sort" in
  Cmd.v info term

let main_cmds = [
  keys_cmd;
  sort_cmd;
]

let () =
  let open Cmdliner in
  let main_info = Cmd.info "confero" in
  exit Cmdliner.Cmd.(eval (group main_info main_cmds))
