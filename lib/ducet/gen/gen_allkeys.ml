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

module List = struct
  include List
  let rec fold f = function [] -> Fun.id | x :: xs -> fold f xs % f x
end

module Parser = struct
  open Angstrom

  let int_of_hex s = int_of_string ("0x" ^ s)

  let is_inline = function '\n' | '\r' -> false | _ -> true
  let is_space = function ' ' -> true | _ -> false
  let is_hexdigit = function '0'..'9' | 'a'..'f' | 'A'..'F' -> true | _ -> false

  let space_opt = skip_while is_space
  let space = skip is_space *> space_opt
  let semicolon = space_opt *> char ';' *> space_opt
  let pair p1 p2 = lift2 (fun x y -> (x, y)) p1 p2
  let brackets p = char '[' *> p <* char ']'

  let uchar = take_while1 is_hexdigit >>| int_of_hex
  let uchar_range = pair uchar (string ".." *> uchar)
  let cweight = consumed (count 4 (satisfy is_hexdigit)) >>| int_of_hex
  let varmark = satisfy (function '*' | '.' -> true | _ -> false)
  let celement =
    brackets (pair varmark (sep_by1 (char '.') cweight)) >>| fun (mark, ws) ->
    Confero.Collation_element.create (Array.of_list ws)
      ~is_variable:(mark = '*')

  let comment_line = option () (char '#' *> skip_while is_inline) *> end_of_line

  let version = take_while1 (function '0'..'9' | '.' -> true | _ -> false)

  let directive =
    choice [
      string "@version" *> space *> version
        >>| (fun x -> `Version x);
      string "@implicitweights" *> space *>
        pair uchar_range (semicolon *> cweight)
        >>| (fun x -> `Implicit_weights x);
    ]
    <* space_opt <* comment_line

  let mapping =
    pair (sep_by1 space uchar) (semicolon *> many1 celement >>| Array.of_list)
    <* space_opt <* comment_line

  let file =
    pair (many (skip_many comment_line *> directive))
         (many (skip_many comment_line *> mapping))
    <* skip_many comment_line
    <* end_of_input
end

module Input_data = struct

  let empty = []

  let load path =
    let ic = open_in path in
    let _, res = Angstrom_unix.parse Parser.file ic in
    close_in ic;
    (match res with
     | Ok (_preamble, data) -> data
     | Error err -> Printf.eprintf "Failed to parse %s: %s" path err; exit 2)

  let merge = (@)

end

module Int_map = struct
  include Map.Make (Int)

  let cut k m =
    (match split k m with
     | mL, None, mR -> (mL, mR)
     | mL, Some x, mR -> (mL, add k x mR))
end

module Int_forest = struct
  type 'a t = 'a tree Int_map.t
  and 'a tree = Tree of 'a * 'a t

  let empty = Int_map.empty

  let rec tree_singleton = function
   | [] -> fun v -> Tree (Some v, Int_map.empty)
   | k :: ks -> fun v -> Tree (None, Int_map.singleton k (tree_singleton ks v))

  let rec tree_add ks v (Tree (v', forest')) =
    (match ks with
     | [] -> assert (v' = None); Tree (Some v, forest')
     | k :: ks -> Tree (v', forest_add k ks v forest'))
  and forest_add k ks v =
    Int_map.update k @@ function
     | None -> Some (tree_singleton ks v)
     | Some tree' -> Some (tree_add ks v tree')

  let add ks v =
    (match ks with
     | [] -> fun _ -> failwith "bad key"
     | k :: ks -> forest_add k ks v)

  let well_formed forest_top =
    try
      let rec complete (Tree (ces, forest)) =
        Tree (Option.get ces, Int_map.map complete forest)
      in
      Int_map.map complete forest_top
    with Invalid_argument _ ->
      let rec recurse ks k (Tree (ces, forest)) =
        let ks' = k :: ks in
        if ces = None then
          Fmt.epr "Not well-formed, @[%a@] is missing.\n"
            Fmt.(list ~sep:sp (fmt "U+%04x")) (List.rev ks');
        Int_map.iter (recurse ks') forest
      in
      Int_map.iter (recurse []) forest_top;
      failwith "Input is not well formed."
end

let pp_escaped_string ppf s =
  for i = 0 to String.length s - 1 do
    Fmt.pf ppf "\\x%02x" (Char.code s.[i])
  done

let pp_celement =
  Fmt.(const string "E.decode_v1 \"" ++
       using Confero.Collation_element.encode_v1 pp_escaped_string ++
       const char '"')

let pp_array pp_elt =
  let open Fmt in
  const string "[|" ++
  array ~sep:(const string "; ") pp_elt ++
  const string "|]"

let rec emit_step name forest =
  let subname k = Fmt.str "%s_%x" name k in
  let emit_deps k (Int_forest.Tree (_, forest')) =
    if not (Int_map.is_empty forest') then
      emit_step (subname k) forest'
  in
  let bind_sparse k (Int_forest.Tree (ces, forest')) =
    Fmt.pr "@[<h> | %#x -> M.Accept (%a, %s)@]@." k
      (pp_array pp_celement) ces
      (if Int_map.is_empty forest' then "rej" else subname k)
  in
  let bind_dense k (Int_forest.Tree (ces, forest')) =
    Fmt.pr "@[<h>    M.Accept (%a, %s);@]@."
      (pp_array pp_celement) ces
      (if Int_map.is_empty forest' then "rej" else subname k)
  in
  Int_map.iter emit_deps forest;
  let k_min = fst (Int_map.min_binding forest) in
  let k_max = fst (Int_map.max_binding forest) in
  if 4 * Int_map.cardinal forest < k_max - k_min + 1 then begin
    Fmt.pr "let %s ch = match Uchar.to_int ch with@." name;
    Int_map.iter bind_sparse forest;
    Fmt.pr " | _ -> M.Reject@."
  end else begin
    Fmt.pr "let %s =@." name;
    Fmt.pr "  let a = [|@.";
    for k = k_min to k_max do
      (match Int_map.find_opt k forest with
       | None -> Fmt.pr "@[<h>    M.Reject;@]@."
       | Some tree -> bind_dense k tree)
    done;
    Fmt.pr "  |] in@.";
    Fmt.pr "  fun ch ->@.";
    Fmt.pr "    let k = Uchar.to_int ch in@.";
    Fmt.pr "    if k < %#x || k > %#x then M.Reject else@." k_min k_max;
    Fmt.pr "    a.(k - %#x)@." k_min;
  end

let emit_top forest =
  let block_size = 0x1000 in
  let rec loop blocks k forest =
    if Int_map.is_empty forest then List.rev blocks else
    let k' = k + block_size in
    let subforest, forest' = Int_map.cut k' forest in
    begin
      let block =
        if Int_map.is_empty subforest then "rej" else
        let block = Fmt.str "block_%x" (k / block_size) in
        begin
          emit_step block subforest;
          block
        end
      in
      loop (block :: blocks) k' forest'
    end
  in
  let blocks = loop [] 0 forest in
  Fmt.pr "let blocks = [|@.";
  Fmt.pr "  @[<hov>%a@]@.|]@." Fmt.(list ~sep:semi string) blocks;
  Fmt.pr "let mapping ch =@.";
  Fmt.pr "  let k = Uchar.to_int ch in@.";
  Fmt.pr "  if k >= %#x then M.Reject else@." (List.length blocks * block_size);
  Fmt.pr "  blocks.(k / %#x) ch@." block_size

let header = {code|
module E = Confero.Collation_element
module M = Confero.Collation_mapping
let rej _ = M.Reject
|code}

let () =
  let inputs = List.map Input_data.load (List.tl (Array.to_list Sys.argv)) in
  let input = Input_data.empty |> List.fold Input_data.merge inputs in
  let forest =
    Int_forest.empty |> List.fold (fun (k, v) -> Int_forest.add k v) input
  in
  let forest = Int_forest.well_formed forest in
  Fmt.pr "%s@." header;
  emit_top forest
