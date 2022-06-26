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

type t = Uchar.t -> node
and node = Reject | Accept of Collation_element.t array * t

let rec unstash xs node =
  (match xs with
   | [] -> node
   | x :: xs' -> unstash xs' (Seq.Cons (x, fun () -> node)))

let emit_array arr cont =
  let rec loop i () =
    if i = Array.length arr then cont () else
    Seq.Cons (arr.(i), loop (i + 1))
  in
  loop 0

let run mapping =
  let
    rec pluck ccc useq_stash useq_node candidate state =
      (match useq_node with
       | Seq.Nil ->
          emit_array candidate (start (unstash useq_stash useq_node)) ()
       | Seq.Cons (ch', useq') ->
          let ccc' = Uucp_ext.canonical_combining_class ch' in
          if ccc' = 0 then
            emit_array candidate (start (unstash useq_stash useq_node)) ()
          else
          if ccc' <= ccc then
            pluck ccc (ch' :: useq_stash) (useq' ()) candidate state
          else
          (match state ch' with
           | Accept (candidate', state') ->
              pluck ccc' useq_stash (useq' ()) candidate' state'
           | Reject ->
              pluck (max ccc ccc')
                (ch' :: useq_stash) (useq' ()) candidate state))
    and extend useq_node candidate state =
      (match useq_node with
       | Seq.Nil ->
          emit_array candidate (start Seq.Nil) ()
       | Seq.Cons (ch, useq') ->
          (match state ch with
           | Reject ->
              let ccc = Uucp_ext.canonical_combining_class ch in
              if ccc <> 0 then
                pluck ccc [ch] (useq' ()) candidate state
              else
                emit_array candidate (start useq_node) ()
           | Accept (candidate', state') ->
              extend (useq' ()) candidate' state'))
    and start useq_node () =
      (match useq_node with
       | Seq.Nil -> Seq.Nil
       | Seq.Cons (ch, useq') ->
          (match mapping ch with
           | Reject ->
              Printf.ksprintf failwith
                "Invalid collation mapping, got stuck on codepoint %#x."
                (Uchar.to_int ch)
           | Accept (candidate', state') ->
              extend (useq' ()) candidate' state'))
  in
  fun useq -> start (useq ())
