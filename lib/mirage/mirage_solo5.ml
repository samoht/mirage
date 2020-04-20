open Functoria
open Action.Infix
open Astring
module Key = Mirage_key

let package = function
  | `Virtio -> "solo5-bindings-virtio"
  | `Muen -> "solo5-bindings-muen"
  | `Hvt -> "solo5-bindings-hvt"
  | `Genode -> "solo5-bindings-genode"
  | `Spt -> "solo5-bindings-spt"
  | _ -> invalid_arg "solo5 bindings only defined for solo5 targets"

let ext = function
  | `Virtio -> ".virtio"
  | `Muen -> ".muen"
  | `Hvt -> ".hvt"
  | `Genode -> ".genode"
  | `Spt -> ".spt"
  | _ -> invalid_arg "solo5 bindings only defined for solo5 targets"

let generate_manifest () =
  let networks =
    List.map (fun n -> (n, `Network)) !Mirage_impl_network.all_networks
  in
  let blocks =
    Hashtbl.fold
      (fun k _v acc -> (k, `Block) :: acc)
      Mirage_impl_block.all_blocks []
  in
  let pp_device ppf (name, typ) =
    Fmt.pf ppf {|{ "name": %S, "type": %S }|} name
      (match typ with `Network -> "NET_BASIC" | `Block -> "BLOCK_BASIC")
  in
  let devices = networks @ blocks in
  let contents =
    Fmt.str
      {|{
  "type": "solo5.manifest",
  "version": 1,
  "devices": [ %a ]
}
|}
      Fmt.(list ~sep:(unit ", ") pp_device)
      devices
  in
  Action.write_file (Fpath.v "manifest.json") contents >>= fun () ->
  Action.write_file (Fpath.v "manifest.ml") ""

let build _ = generate_manifest ()

let files _ = List.map Fpath.v [ "manifest.json"; "manifest.ml" ]

let main i = Fpath.(base (rem_ext (Info.main i)))

let pp_list = Fmt.(list ~sep:(unit " ") string)

let out i =
  let target = Info.get i Key.target in
  let public_name =
    match Info.output i with None -> Info.name i | Some o -> o
  in
  public_name ^ ext target

let link i =
  let target = Info.get i Key.target in
  let main = Fpath.to_string (main i) in
  let pkg = package target in
  let out = out i in
  Dune.stanzaf
    {|
(rule
  (copy %%{lib:%s:ldflags} .))

(rule
  (target %s)
  (mode (promote (until-clean)))
  (deps main.exe.o manifest.o)
  (action
    (run %%{ld} (:include ldflags) %s.exe.o manifest.o -o %%{target})))
|}
    pkg out main

let manifest () =
  Dune.stanzaf
    {|
(rule
  (targets manifest.c)
  (deps manifest.json)
  (action (run solo5-elftool gen-manifest manifest.json manifest.c)))

(library
  (name manifest)
  (modules manifest)
  (foreign_stubs
    (language c)
    (names manifest)))
|}

let install i =
  let out = out i in
  let package = Info.name i in
  Dune.stanzaf {|
(install
  (files %s)
  (section bin)
  (package %s))
|} out
    package

let main i =
  let libraries = Info.libraries i in
  let flags = Mirage_dune.flags i in
  let main = Fpath.to_string (main i) in
  Dune.stanzaf
    {|
(rule (copy %%{lib:mirage-solo5:cflags} .))

(executable
  (name %s)
  (modes (native object))
  (libraries ocaml-freestanding %a)
  (link_flags %a)
  (modules (:standard \ config manifest))
  (forbidden_libraries unix)
  (flags (:include cflags)))
|}
    main pp_list libraries pp_list flags

let dune i = [ main i; manifest (); link i; install i ]

let workspace _ =
  let dune target =
    Dune.stanzaf
      {|
(context (default
  (name mirage-%a)
  (host default)
  (env (_ (c_flags (:include cflags-%a))))))
|}
      Key.pp_target target Key.pp_target target
  in
  List.map dune [ `Hvt ]
