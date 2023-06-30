open Functoria
module Key = Mirage_key

type network = NETWORK

let network = Type.v NETWORK
let all_networks = ref []

let add_new_network () =
  let device = Fmt.str "net%d" (List.length !all_networks) in
  all_networks := device :: !all_networks

let network_conf (intf : string Key.runtime_key) =
  let keys = [ (intf :> Key.t) ] in
  let packages_v =
    Key.match_ Key.(value target) @@ function
    | `Unix -> [ package ~min:"3.0.0" ~max:"4.0.0" "mirage-net-unix" ]
    | `MacOSX -> [ package ~min:"1.8.0" ~max:"2.0.0" "mirage-net-macosx" ]
    | `Xen -> [ package ~min:"2.1.0" ~max:"3.0.0" "mirage-net-xen" ]
    | `Qubes ->
        [
          package ~min:"2.1.0" ~max:"3.0.0" "mirage-net-xen";
          Mirage_impl_qubesdb.pkg;
        ]
    | #Mirage_key.mode_solo5 ->
        [ package ~min:"0.8.0" ~max:"0.9.0" "mirage-net-solo5" ]
  in
  let connect _ modname _ =
    Fmt.str "%s.connect %a" modname Key.serialize_call intf
  in
  let configure _ =
    add_new_network ();
    Action.ok ()
  in
  impl ~keys ~packages_v ~connect ~configure "Netif" network

let netif ?group dev = network_conf @@ Key.interface ?group dev

let default_network =
  match_impl
    Key.(value target)
    [
      (`Unix, netif "tap0");
      (`MacOSX, netif "tap0");
      (* On Solo5 targets, a single default network is customarily
       * named just 'service' *)
      (`Hvt, netif "service");
      (`Spt, netif "service");
      (`Virtio, netif "service");
      (`Muen, netif "service");
      (`Genode, netif "service");
    ]
    ~default:(netif "0")
