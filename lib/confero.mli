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

(** {1 Collation Element Mappings} *)

module Collation_mapping = Collation_mapping

val register_collation_mapping :
  ?lang: Iso639.Lang.t ->
  Collation_mapping.t -> unit
(** [register_collation_mapping ~lang m] registers [m] to be used for collating
    string representing text in the language [lang].

    [register_collation_mapping m] registers [m] to be used as a fall-back
    mapping when [collate] is called without an explicit mapping and either the
    language is unspecified or not mapping has been registered for the specified
    language.  A good fall-back is provided by linking against the
    [confero.ducet] sublibrary.

    This function updates global state and should only be used during program
    initialization by libraries providing collation mappings or by the main
    entry point of the application. *)

val infer_collation_mapping :
  ?lang: Iso639.Lang.t ->
  ?mapping: Collation_mapping.t ->
  unit -> Collation_mapping.t
(** [infer_collation_mapping ?lang ?mapping ()] returns the mapping which would
    have been used by {!collate} given the same named arguments. *)

(** {1 Collation Algorithm} *)

module Collation_element = Collation_element
(** Vectors of weights derived from an input string and a collation element
    mapping. *)

module Sort_key = Sort_key
(** A compact representations of a collation elements as private strings whose
    lexicographic byte order represent the collation order. *)

(** {1 Collation} *)

val collate :
  ?encoding: Uutf.encoding ->
  ?lang: Iso639.Lang.t ->
  ?mapping: Collation_mapping.t ->
  ?total: bool ->
  string -> string -> int
(** [collate ?encoding ?lang ?mapping s1 s2] returns a negative integer, zero,
    or a positive integer when [s1] is ordered before [s2], [s1] and [s2] are
    unordered, or [s1] is ordered after [s2], respectively, according to a
    collation mapping inferred from [mapping] and [lang] as follows:

      - If [lang] is specified and a mapping has been registered for [lang] with
        {!register_collation_mapping} then it is used, else
      - if [mapping] is specified then it is used, else
      - if a fall-back mapping has been registered with
        {!register_collation_mapping} then it is used, else
      - a mapping using Unicode code point ordering is used.

    Pass [~total:true] when a total order is required, e.g. when using it as the
    implementation of {!Map.OrderedType.compare} or {!Set.OrderedType.compare}.

    @param lang
      The language used to infer the collation mappping.
    @param mapping
      The default mapping if not inferred from [lang].
    @param encoding
      The encoding of the strings to compare.
    @param total
      Ensure that the implied order is total.  This is done by falling back to
      {!String.compare} if the result from the collation algorithm is zero.  The
      default is [false], meaning that zero may be returned for two different
      input strings due to Unicode normalization or due to a non-injective
      collation element mapping. *)
