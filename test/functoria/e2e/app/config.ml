open Functoria
open E2e

let opt = Key.runtime "Key.opt"
let opt_all = Key.runtime "Key.opt_all"
let flag = Key.runtime "Key.flag"
let required = Key.runtime "Key.required"

let keys =
  let connect _ _ _ =
    Fmt.str
      {|
      let _ : string = %a
      and _ : string list = %a
      and _ : bool = %a
      and _ : string = %a
      in
      return ()|}
      Key.serialize_call opt Key.serialize_call opt_all Key.serialize_call flag
      Key.serialize_call required
  in
  let keys = [ opt; opt_all; flag; required ] in
  impl ~connect ~keys "Unit" job

let main = main "App.Make" (job @-> job)
let () = register "noop" [ main $ keys ]
