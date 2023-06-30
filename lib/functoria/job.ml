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

let src = Logs.Src.create "functoria" ~doc:"functoria library"

module Log = (val Logs.src_log src : Logs.LOG)
open Astring

type t = JOB

let t = Type.v JOB

(* Noop, the job that does nothing. *)
let noop = Impl.v "Unit" t

module Keys = struct
  let configure ~file i =
    Log.info (fun m -> m "Generating: %a (keys)" Fpath.pp file);
    Action.with_output ~path:file ~purpose:"key_gen file" (fun ppf ->
        let keys = Key.Set.of_list @@ Info.keys i in
        Fmt.pf ppf "@[<v>%a@]@." Fmt.(iter Key.Set.iter Key.serialize) keys)
end

let keys ?(runtime_package = "functoria-runtime")
    ?(runtime_modname = "Functoria_runtime") (argv : Argv.t Impl.t) =
  let packages = [ Package.v runtime_package ] in
  let extra_deps = [ Impl.abstract argv ] in
  let key_gen = Key.module_name in
  let file = Fpath.(v (String.Ascii.lowercase key_gen) + "ml") in
  let configure = Keys.configure ~file in
  let files _ = [ file ] in
  let connect info _ = function
    | [ argv ] ->
        Fmt.str "return %s.(with_argv (runtime_keys ()) %S %s)" runtime_modname
          (Info.name info) argv
    | _ -> failwith "The keys connect should receive exactly one argument."
  in
  Impl.v ~files ~configure ~packages ~extra_deps ~connect key_gen t
