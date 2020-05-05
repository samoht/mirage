open Functoria

val dune : Info.t -> Dune.stanza list

val configure : Info.t -> Fpath.t list * unit Action.t

val workspace : Info.t -> Dune.stanza list
