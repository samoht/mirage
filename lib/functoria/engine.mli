(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013-2020 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2015-2020 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Functoria engine. *)

type t = Device_graph.t
(** The type for key graphs. *)

val if_keys : t -> Key.Set.t
(** [if_keys t] is the set of [if] keys in the graph [t]. *)

val all_keys : t -> Key.Set.t
(** [all_keys t] is the set of keys in the graph [t]. *)

val packages : t -> Package.t list Key.value
(** [packages t] is the set of packages in the graph [t]. *)

(** {2 Triggering Hooks} *)

val connect : ?init:'a Impl.t list -> Info.t -> t -> unit Action.t
(** [connect ?init i t] generates the [connect] functions in [main.ml], for each
    of the implementations appearing [t], in topological order. Use build
    information [i]. *)

val configure : Info.t -> t -> Device.effect
(** [configure i t] calls all the configuration hooks for each of the
    implementations appearing in [t], in topological order. Use the build
    information [i]. *)

val build : Info.t -> t -> Device.effect
(** [build i t] calls the build hooks for each of the implementations appearing
    in [t], in topological order. Use the build information [i]. *)
