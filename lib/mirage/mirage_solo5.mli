open Functoria

val dune : Info.t -> Dune.stanza list

val configure : Info.t -> Fpath.t list * unit Action.t

val package : Mirage_key.mode_solo5 -> string

val workspace : Info.t -> Dune.stanza list
