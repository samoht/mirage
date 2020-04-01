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

open Astring

type stanza = string option

type t = string list

let stanza v = Some (String.trim v)

let stanzaf fmt = Fmt.kstr stanza fmt

let v x : t =
  List.fold_left
    (fun acc -> function None -> acc | Some f -> f :: acc)
    [] (List.rev x)

let pp_list pp = Fmt.(list ~sep:(unit "\n\n") pp)

let pp ppf (t : t) = Fmt.pf ppf "%a" (pp_list Fmt.string) t

let to_string t = Fmt.to_to_string pp t ^ "\n"

let pp_args ppf args =
  match Cli.argv_of_args args with
  | [||] | [| "" |] -> ()
  | argv -> Fmt.pf ppf " %a" Fmt.(array ~sep:(unit " ") string) argv

let headers ~name ~version =
  let module M = Filegen.Make (struct
    let name = name

    let version = version
  end) in
  M.headers `Sexp

let base ~packages ~name ~version =
  let headers = headers ~name ~version in
  let pkgs =
    match packages with
    | [] -> ""
    | pkgs ->
        let pkgs =
          List.fold_left
            (fun acc pkg ->
              let pkgs = String.Set.of_list (Package.libraries pkg) in
              String.Set.union pkgs acc)
            String.Set.empty pkgs
          |> String.Set.elements
        in
        String.concat ~sep:" " pkgs
  in
  (*  let args = { args with Cli.setup = [] } in *)
  let contents =
    Fmt.strf
      {|%s

(executable
  (name config)
  (flags (:standard -warn-error -A))
  (modules config)
  (libraries %s))
|}
      headers pkgs
  in
  v [ stanza contents ]

let files a = Fpath.Set.elements (Action.generated_files a)

let pp_files = Fmt.(list ~sep:(unit " ") Fpath.pp)

let configure ~packages ~name ~version ~configure ?(extra = []) args =
  let configure_files = files configure in
  let config_file = Fpath.base args.Cli.config_file in
  let args = { args with Cli.config_file } in
  let dune_base = base ~packages ~name ~version in
  let contents =
    Fmt.str
      {|(rule
  (alias %s-configure)
  (targets %a)
  (deps %a)
  (action
    (run ./config.exe configure%a)))
|}
      name pp_files configure_files Fpath.pp config_file pp_args args
  in
  dune_base @ v (stanza contents :: extra)

let build ~packages ~name ~version ~configure:c ~build ?(extra = []) args =
  let build_files = files build in
  let config_file = Fpath.base args.Cli.config_file in
  let args = { args with Cli.config_file } in
  let dune_configure = configure ~packages ~name ~version ~configure:c args in
  let contents =
    Fmt.str
      {|
(rule
  (alias %s-build)
  (targets %a)
  (deps %a)
  (action
    (run ./config.exe build%a)))
|}
      name pp_files build_files Fpath.pp config_file pp_args args
  in
  dune_configure @ v (stanza contents :: extra)
