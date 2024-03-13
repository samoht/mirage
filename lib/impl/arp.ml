module Key = Mirage_key
open Functoria
open Ethernet
open Time
open Misc

type arpv4 = Arpv4

let arpv4 = Type.v Arpv4

let arp_conf =
  let packages =
    [ package ~min:"3.0.0" ~max:"4.0.0" ~sublibs:[ "mirage" ] "arp" ]
  in
  let connect _ modname = function
    | [ eth; _time ] -> code ~pos:__POS__ "%s.connect %s" modname eth
    | _ -> connect_err "arp" 2
  in
  impl ~packages ~connect "Arp.Make" (ethernet @-> time @-> arpv4)

let arp ?(time = default_time) (eth : ethernet impl) = arp_conf $ eth $ time
