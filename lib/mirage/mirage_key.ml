(*
 * Copyright (c) 2015 Gabriel Radanne <drupyog@zoho.com>
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

open Cmdliner
open Functoria
module Key = Key
open Astring

(** {2 Documentation helper} *)

let mirage_section = "MIRAGE PARAMETERS"
let unikernel_section = "UNIKERNEL PARAMETERS"
let pp_group = Fmt.(option ~none:(any "the unikernel") @@ fmt "the %s group")

(** {2 Special keys} *)

(** {3 Mode} *)

type mode_unix = [ `Unix | `MacOSX ]
type mode_xen = [ `Xen | `Qubes ]
type mode_solo5 = [ `Hvt | `Spt | `Virtio | `Muen | `Genode ]
type mode = [ mode_unix | mode_xen | mode_solo5 ]

let target_conv : mode Cmdliner.Arg.conv =
  let parser, printer =
    Cmdliner.Arg.enum
      [
        ("unix", `Unix);
        ("macosx", `MacOSX);
        ("xen", `Xen);
        ("virtio", `Virtio);
        ("hvt", `Hvt);
        ("muen", `Muen);
        ("qubes", `Qubes);
        ("genode", `Genode);
        ("spt", `Spt);
      ]
  in
  (parser, printer)

let pp_target fmt m = snd target_conv fmt m

let default_target =
  match Sys.getenv "MIRAGE_DEFAULT_TARGET" with
  | "unix" -> `Unix
  | s -> Fmt.failwith "invalid default target: %S" s
  | exception Not_found -> (
      match Action.run @@ Action.run_cmd_out Bos.Cmd.(v "uname" % "-s") with
      | Ok "Darwin" -> `MacOSX
      | _ -> `Unix)

let target =
  let doc =
    "Target platform to compile the unikernel for. Valid values are: $(i,xen), \
     $(i,qubes), $(i,unix), $(i,macosx), $(i,virtio), $(i,hvt), $(i,spt), \
     $(i,muen), $(i,genode)."
  in
  let env = Cmd.Env.info "MODE" in
  let doc =
    Arg.info ~docs:mirage_section ~docv:"TARGET" ~doc [ "t"; "target" ] ~env
  in
  let key = Arg.(value & opt target_conv default_target doc) in
  Key.create "target" key (snd target_conv)

let is_unix =
  Key.match_ Key.(value target) @@ function
  | #mode_unix -> true
  | #mode_xen | #mode_solo5 -> false

let is_solo5 =
  Key.match_ Key.(value target) @@ function
  | #mode_solo5 -> true
  | #mode_xen | #mode_unix -> false

let is_xen =
  Key.match_ Key.(value target) @@ function
  | #mode_xen -> true
  | #mode_solo5 | #mode_unix -> false

(** {2 OCaml runtime} *)

let backtrace = Key.runtime "Mirage_runtime.GC.backtrace"
let randomize_hashtables = Key.runtime "Mirage_runtime.GC.randomize_hashtables"
let allocation_policy = Key.runtime "Mirage_runtime.GC.allocation_policy"
let minor_heap_size = Key.runtime "Mirage_runtime.GC.minor_heap_size"
let major_heap_increment = Key.runtime "Mirage_runtime.GC.major_heap_increment"
let space_overhead = Key.runtime "Mirage_runtime.GC.space_overhead"
let max_space_overhead = Key.runtime "Mirage_runtime.GC.max_space_overhead"
let gc_verbosity = Key.runtime "Mirage_runtime.GC.gc_verbosity"
let gc_window_size = Key.runtime "Mirage_runtime.GC.gc_window_size"
let custom_major_ratio = Key.runtime "Mirage_runtime.GC.custom_major_ratio"
let custom_minor_ratio = Key.runtime "Mirage_runtime.GC.custom_minor_ratio"

let custom_minor_max_size =
  Key.runtime "Mirage_runtime.GC.custom_minor_max_size"

(** {2 General mirage keys} *)

let configure_key ?(group = "") ~doc ~default xconv name =
  let prefix = if group = "" then group else group ^ "-" in
  let doc =
    Arg.info ~docs:unikernel_section
      ~docv:(String.Ascii.uppercase name)
      ~doc
      [ prefix ^ name ]
  in
  let key = Arg.(value & opt xconv default doc) in
  Key.create (prefix ^ name) key (snd xconv)

(** {3 File system keys} *)

let kv_ro ?group () =
  let conv = Cmdliner.Arg.enum [ ("crunch", `Crunch); ("direct", `Direct) ] in
  let doc =
    Fmt.str
      "Use a $(i,crunch) or $(i,direct) pass-through implementation for %a."
      pp_group group
  in
  configure_key ~doc ?group ~default:`Crunch conv "kv_ro"

(** {3 Block device keys} *)
let block ?group () =
  let conv =
    Cmdliner.Arg.enum
      [ ("xenstore", `XenstoreId); ("file", `BlockFile); ("ramdisk", `Ramdisk) ]
  in
  let doc =
    Fmt.str
      "Use a $(i,ramdisk), $(i,xenstore), or $(i,file) pass-through \
       implementation for %a."
      pp_group group
  in
  configure_key ~doc ?group ~default:`Ramdisk conv "block"

(** {3 Stack keys} *)

let dhcp ?group () =
  let doc = Fmt.str "Enable dhcp for %a." pp_group group in
  configure_key ~doc ?group ~default:false Arg.bool "dhcp"

let net ?group () : [ `Socket | `Direct ] option Key.key =
  let conv = Cmdliner.Arg.enum [ ("socket", `Socket); ("direct", `Direct) ] in
  let doc =
    Fmt.str "Use $(i,socket) or $(i,direct) group for %a." pp_group group
  in
  configure_key ~doc ?group ~default:None (Arg.some conv) "net"

let runtime_key fmt = Fmt.kstr Key.runtime ("Mirage_runtime." ^^ fmt)
let pp_group ppf = function None -> () | Some g -> Fmt.pf ppf "~group:%S " g

let pp_option pp ppf = function
  | None -> Fmt.pf ppf "None"
  | Some d -> Fmt.pf ppf "(Some %a)" pp d

let escape pp ppf = Fmt.kstr (fun str -> Fmt.Dump.string ppf str) "%a" pp

(** {3 Network keys} *)

let runtime_network_key fmt =
  Fmt.kstr Key.runtime ("(Mirage_runtime_network." ^^ fmt ^^ ")")

let interface ?group default =
  runtime_network_key "interface %a%S" pp_group group default

module V4 = struct
  open Ipaddr.V4

  let pp_prefix ppf p =
    Fmt.pf ppf "(Ipaddr.V4.Prefix.of_string_exn %a)" (escape Prefix.pp) p

  let pp ppf p = Fmt.pf ppf "(Ipaddr.V4.of_string_exn %a)" (escape pp) p

  let network ?group default =
    runtime_network_key "V4.network %a%a" pp_group group pp_prefix default

  let gateway ?group default =
    runtime_network_key "V4.gateway %a%a" pp_group group (pp_option pp) default
end

module V6 = struct
  open Ipaddr.V6

  let pp_prefix ppf p =
    Fmt.pf ppf "(Ipaddr.V6.Prefix.of_string_exn %a)" (escape Prefix.pp) p

  let pp ppf p = Fmt.pf ppf "(Ipaddr.V6.of_string_exn %a)" (escape pp) p

  let network ?group default =
    runtime_network_key "V6.network %a%a" pp_group group (pp_option pp_prefix)
      default

  let gateway ?group default =
    runtime_network_key "V6.gateway %a%a" pp_group group (pp_option pp) default

  let accept_router_advertisements ?group () =
    runtime_network_key "V6.accept_router_advertisements %a()" pp_group group
end

let ipv4_only ?group () = runtime_network_key "ipv4_only %a()" pp_group group
let ipv6_only ?group () = runtime_network_key "ipv6_only %a()" pp_group group

let resolver ?(default = []) () =
  let pp_default ppf = function
    | [] -> ()
    | l -> Fmt.pf ppf "~default:%a " Fmt.Dump.(list string) l
  in
  runtime_network_key "resolver %a()" pp_default default

let pp_ipaddr ppf p = Fmt.pf ppf "Ipaddr.of_string %a" (escape Ipaddr.pp) p
let syslog default = runtime_key "syslog %a" (pp_option pp_ipaddr) default

let syslog_port default =
  runtime_key "syslog_port %a" (pp_option Fmt.int) default

let syslog_hostname default = runtime_key "syslog_hostname %S" default
let logs = Key.runtime "Mirage_runtime.logs"

include (Key : Functoria.KEY)
