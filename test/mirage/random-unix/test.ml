open Mirage

let opt = Key.runtime "Key.opt"
let opt_all = Key.runtime "Key.opt_all"
let flag = Key.runtime "Key.flag"
let required = Key.runtime "Key.required"

let test () =
  let context = Key.add_to_context Key.target `Unix Context.empty in
  let sigs = conduit @-> random @-> job in
  let network = default_network in
  let etif = etif network in
  let arp = arp etif in
  let ipv4 = create_ipv4 etif arp in
  let ipv6 = create_ipv6 network etif in
  let stackv4v6 =
    direct_stackv4v6 ~ipv4_only:(Key.ipv4_only ()) ~ipv6_only:(Key.ipv6_only ())
      network etif arp ipv4 ipv6
  in
  let init = Functoria.(keys sys_argv) in
  let job =
    main "App.Make" sigs $ conduit_direct ~tls:true stackv4v6 $ default_random
  in

  let job =
    let connect _ _ _ = "return ()" in
    Functoria.impl
      ~keys:[ opt; opt_all; flag; required ]
      ~extra_deps:[ dep job; dep init ]
      "Functoria_runtime" ~connect Functoria.job
  in
  Functoria_test.run context job

let () =
  match Functoria.Action.run (test ()) with
  | Ok () -> ()
  | Error (`Msg e) -> failwith e
