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

open Misc

type 'a key = {
  name : string;
  arg : 'a Cmdliner.Term.t;
  pp : 'a Fmt.t;
  key : 'a Context.key;
}

type t = Any : 'a key -> t | Run : string -> t
type 'a runtime_key = t

let runtime s = Run s

(* evaluate the key in an empty context to get the default value. *)
let default_v k =
  let i = Cmdliner.Cmd.info "empty" in
  match
    Cmdliner.Cmd.eval_value ~argv:[| "empty" |]
      ~env:(fun _ -> None)
      Cmdliner.Cmd.(v i k.arg)
  with
  | Ok (`Ok v) -> Some v
  | _ -> None

let equal x y = String.equal x.name y.name && default_v x = default_v y

(* Set of keys, without runtime name conflicts. This is useful to create a
   valid cmdliner term. *)
module Names = Stdlib.Set.Make (struct
  type nonrec t = t

  let compare x y =
    match (x, y) with
    | Any x, Any y -> String.compare x.name y.name
    | Run x, Run y -> String.compare x y
    | Any _, Run _ -> 1
    | Run _, Any _ -> -1
end)

(* Set of keys, where keys with the same name but with different
   defaults are distinguished. This is useful to build the graph of
   devices. *)
module Set = struct
  module M = struct
    type nonrec t = t

    let compare = compare
  end

  include Set.Make (M)

  let add k set =
    if mem k set then
      if k != find k set then
        match k with
        | Any k -> Fmt.invalid_arg "Duplicate key name: %s" k.name
        | Run k -> Fmt.invalid_arg "Duplicate runtime key name: %s" k
      else set
    else add k set

  let pp_gen = Fmt.iter ~sep:(Fmt.any ",@ ") iter

  let pp_elt fmt = function
    | Any k -> Fmt.string fmt k.name
    | Run k -> Fmt.string fmt k

  let pp = pp_gen pp_elt
end

let v x = Any x
let abstract = v
let name k = k.name
let stage = function Any _ -> `Configure | Run _ -> `Run
let is_runtime k = match stage k with `Run -> true | `Configure -> false
let is_configure k = match stage k with `Configure -> true | `Run -> false

let filter_stage stage s =
  match stage with
  | `Run -> Set.filter is_runtime s
  | `Configure | `NoEmit -> Set.filter is_configure s

(* Key Map *)

type context = Context.t

let add_to_context t = Context.add t.key
let find (type a) ctx (t : a key) : a option = Context.find t.key ctx

exception Not_found of string

let get ctx t =
  match find ctx t with
  | Some x -> x
  | None -> (
      match default_v t with Some v -> v | None -> raise (Not_found t.name))

(* {2 Values} *)

type +'a value = { deps : Set.t; v : context -> 'a }

let eval p v = v.v p
let pure x = { deps = Set.empty; v = (fun _ -> x) }

let app f x =
  { deps = Set.union f.deps x.deps; v = (fun p -> (eval p f) (eval p x)) }

let map f x = app (pure f) x
let pipe x f = map f x
let if_ c t e = pipe c @@ fun b -> if b then t else e
let match_ v f = map f v
let ( $ ) = app

let value k =
  let v c = get c k in
  { deps = Set.singleton (Any k); v }

let of_deps deps = { (pure ()) with deps }
let deps k = k.deps

let mem p v =
  Set.for_all (function Any x -> Context.mem x.key p | Run _ -> false) v.deps

let peek p v = if mem p v then Some (eval p v) else None
let default v = eval Context.empty v

(* {2 Pretty printing} *)

let pp = Set.pp_elt
let pp_deps fmt v = Set.pp fmt v.deps

let pps p ppf l =
  let l = filter_stage `Configure l in
  let pp' fmt k v =
    let default_v = default_v k in
    let is_default = Some v = default_v in
    let default = if is_default then Fmt.any " (default)" else Fmt.nop in
    Fmt.pf fmt "%a=%a%a" Fmt.(styled `Bold string) k.name k.pp v default ()
  in
  let f fmt = function Run _ -> () | Any k -> pp' fmt k (get p k) in

  let pp = Fmt.vbox @@ fun ppf s -> Set.(pp_gen f ppf @@ s) in
  pp ppf l

(* {2 Automatic documentation} *)

(* {2 Key creation} *)

(* Unexposed smart constructor. *)
let make (type a) ~(arg : a Cmdliner.Term.t) ~(pp : a Fmt.t) ~name =
  let key = Context.new_key name in
  { arg; name; pp; key }

let create name arg pp =
  if name = "" then
    invalid_arg "Key.create: key name cannot be the empty string";
  make ~arg ~name ~pp

(* {2 Cmdliner interface} *)

let context l =
  let stage = filter_stage `Configure l in
  let names = Names.of_list (Set.elements stage) in
  let gather k rest =
    match k with
    | Run _ -> rest
    | Any k ->
        let f v p = Context.add k.key v p in
        Cmdliner.Term.(const f $ k.arg $ rest)
  in
  Names.fold gather names (Cmdliner.Term.const Context.empty)

(* {2 Code emission} *)

let module_name = "Key_gen"
let ocaml_name k = String.lowercase_ascii (Name.ocamlify k)

let serialize_call fmt = function
  | Any k -> Fmt.pf fmt "(%s.%s ())" module_name (ocaml_name k.name)
  | Run k -> Fmt.pf fmt "(%s.%s ())" module_name (ocaml_name k)

let serialize fmt = function
  | Run k ->
      Format.fprintf fmt "@[<2>let %s =@ @[Functoria_runtime.key@ %s@]@]@,"
        (ocaml_name k) k
  | _ -> ()
