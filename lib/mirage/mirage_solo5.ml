open Functoria
open Action.Infix
open Astring
module Key = Mirage_key

let package = function
  | `Virtio -> "solo5-virtio"
  | `Muen -> "solo5-muen"
  | `Hvt -> "solo5-hvt"
  | `Genode -> "solo5--genode"
  | `Spt -> "solo5-spt"
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
(rule (copy %%{lib:%s:ldflags} solo5-ldflags))

(rule (copy %%{lib:ocaml-freestanding:ldflags} freestanding-ldflags))

(rule
  (target %s)
  (enabled_if (= %%{context_name} "mirage-%a"))
  (mode (promote (until-clean)))
  (deps main.exe.o manifest.o solo5-ldflags freestanding-ldflags)
  (action
    (run
      ld %%{read:solo5-ldflags} %%{read:freestanding-ldflags}
         %s.exe.o manifest.o -o %%{target})))
|}
    pkg out Key.pp_target target main

let manifest _i =
  Dune.stanzaf
    {|
(rule
  (targets manifest.c)
  (deps manifest.json (package solo5))
  (action (run solo5-elftool gen-manifest manifest.json manifest.c)))
|}

let install i =
  let out = out i in
  let target = Info.get i Key.target in
  let package = Info.name i in
  Dune.stanzaf
    {|
;(install
;  (files %s)
;  need ocaml/dune#3354
;  (enabled_if (= %%{context_name} "mirage-%a"))
;  (section bin)
;  (package %s))
|}
    out Key.pp_target target package

(* FIXME: should be generated in the root dune only *)
let workspace_flags i =
  let target = Info.get i Key.target in
  let pkg = package target in
  Dune.stanzaf
    {|
(rule
  (target cflags-%a)
  (action
    (with-stdout-to %%{target} (progn
      (echo "(")
      (cat %%{lib:%s:cflags})
      (echo " ")
      (cat %%{lib:ocaml-freestanding:cflags})
      (echo ")")))))
    |}
    Key.pp_target target pkg

let main i =
  let libraries = Info.libraries i in
  let flags = Mirage_dune.flags i in
  let target = Info.get i Key.target in
  let main = Fpath.to_string (main i) in
  Dune.stanzaf
    {|
(executable
  (enabled_if (= %%{context_name} "mirage-%a"))
  (name %s)
  (modes (native object))
  (libraries ocaml-freestanding %a)
  (link_flags %a)
  (modules (:standard \ config manifest))
  (foreign_stubs  (language c) (names manifest))
  (forbidden_libraries unix))
|}
    Key.pp_target target main pp_list libraries pp_list flags

let dune i = [ main i; manifest i; link i; workspace_flags i; install i ]

let workspace _ =
  let dune target =
    Dune.stanzaf
      {|
(context (default
  (name mirage-%a)
  (host default)
  (disable_dynamically_linked_foreign_archives true)
  (env (_ (c_flags (:include cflags-%a))))))
|}
      Key.pp_target target Key.pp_target target
  in
  Dune.stanza "(profile release)" :: List.map dune [ `Hvt ]
