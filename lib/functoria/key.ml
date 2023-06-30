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

module Serialize = struct
  let option x ppf v =
    match v with
    | None -> Fmt.(Dump.option x) ppf v
    | Some _ -> Fmt.(parens (Dump.option x)) ppf v

  let list x = Fmt.Dump.list x
  let pair a b = Fmt.Dump.pair a b
end

module Arg = struct
  (** {1 Converters} *)

  type 'a serialize = Format.formatter -> 'a -> unit
  type 'a runtime_conv = string

  type 'a converter = {
    conv : 'a Cmdliner.Arg.conv;
    serialize : 'a serialize;
    runtime_conv : 'a runtime_conv;
  }

  let conv ~conv ~serialize ~runtime_conv = { conv; serialize; runtime_conv }
  let converter x = x.conv
  let serialize x = x.serialize
  let runtime_conv x = x.runtime_conv

  let string =
    conv ~conv:Cmdliner.Arg.string ~runtime_conv:"string" ~serialize:(fun fmt ->
        Format.fprintf fmt "%S")

  let bool =
    conv ~conv:Cmdliner.Arg.bool ~runtime_conv:"bool" ~serialize:(fun fmt ->
        Format.fprintf fmt "%b")

  let int =
    conv ~conv:Cmdliner.Arg.int ~runtime_conv:"int" ~serialize:(fun fmt i ->
        Format.fprintf fmt "(%i)" i)

  let int64 =
    conv ~conv:Cmdliner.Arg.int64 ~runtime_conv:"int64" ~serialize:(fun fmt i ->
        Format.fprintf fmt "(%LiL)" i)

  let list ?sep d =
    let runtime_conv =
      match sep with
      | None -> Fmt.str {ocaml|(Cmdliner.Arg.list %s)|ocaml} (runtime_conv d)
      | Some sep ->
          Fmt.str {ocaml|(Cmdliner.Arg.list ~sep:'\x%02x' %s)|ocaml}
            (Char.code sep) (runtime_conv d)
    in
    conv
      ~conv:(Cmdliner.Arg.list ?sep (converter d))
      ~runtime_conv
      ~serialize:(Serialize.list (serialize d))

  let pair ?sep a b =
    let runtime_conv =
      match sep with
      | None ->
          Fmt.str {ocaml|(Cmdliner.Arg.pair %s %s)|ocaml} (runtime_conv a)
            (runtime_conv b)
      | Some sep ->
          Fmt.str {ocaml|(Cmdliner.Arg.pair ~sep:'\x%02x' %s %s)|ocaml}
            (Char.code sep) (runtime_conv a) (runtime_conv b)
    in
    conv
      ~conv:(Cmdliner.Arg.pair ?sep (converter a) (converter b))
      ~runtime_conv
      ~serialize:(Serialize.pair (serialize a) (serialize b))

  let some d =
    conv
      ~conv:(Cmdliner.Arg.some (converter d))
      ~runtime_conv:(Fmt.str "(Cmdliner.Arg.some %s)" (runtime_conv d))
      ~serialize:(Serialize.option (serialize d))

  (** {1 Information about arguments} *)

  type info = {
    doc : string option;
    docs : string;
    docv : string option;
    names : string list;
    env : string option;
  }

  let info ?(docs = "APPLICATION OPTIONS") ?docv ?doc ?env names =
    { doc; docs; docv; names; env }

  let cmdliner_of_info { docs; docv; doc; env; names } =
    let env =
      match env with Some s -> Some (Cmdliner.Cmd.Env.info s) | None -> None
    in
    Cmdliner.Arg.info ~docs ?docv ?doc ?env names

  (** {1 Arguments} *)

  type 'a kind =
    | Opt : 'a * 'a converter -> 'a kind
    | Opt_all : 'a converter -> 'a list kind
    | Required : 'a converter -> 'a option kind
    | Flag : bool kind

  let pp_conv c = snd (converter c)

  let pp_kind : type a. a kind -> a Fmt.t = function
    | Opt (_, c) -> pp_conv c
    | Opt_all c -> pp_conv (list c)
    | Required c -> pp_conv (some c)
    | Flag -> Fmt.bool

  let compare_kind : type a b. a kind -> b kind -> int =
   fun a b ->
    let default cx x = Fmt.to_to_string (snd cx.conv) x in
    match (a, b) with
    | Opt (x, cx), Opt (y, cy) -> String.compare (default cx x) (default cy y)
    | Required _, Required _ -> 0
    | Opt_all _, Opt_all _ -> 0
    | Flag, Flag -> 0
    | Opt _, _ -> 1
    | _, Opt _ -> -1
    | Required _, _ -> 1
    | _, Required _ -> -1
    | Opt_all _, _ -> 1
    | _, Opt_all _ -> -1

  type 'a t = { info : info; kind : 'a kind }

  let pp t = pp_kind t.kind
  let equal x y = x.info = y.info && compare_kind x.kind y.kind = 0
  let opt conv default info = { info; kind = Opt (default, conv) }
  let flag info = { info; kind = Flag }
  let required conv info = { info; kind = Required conv }
  let opt_all conv info = { info; kind = Opt_all conv }

  let default (type a) (t : a t) =
    match t.kind with
    | Opt (d, _) -> d
    | Flag -> (false : bool)
    | Required _ -> (None : _ option)
    | Opt_all _ -> ([] : _ list)

  (* XXX(dinosaure): I don't understand why we wrapped
   * value with ['a option]. *)

  let make_opt_cmdliner wrap i default desc =
    let none =
      match default with
      | Some d -> Some (Fmt.str "%a" (pp_conv desc) d)
      | None -> None
    in
    Cmdliner.Arg.(wrap @@ opt (some ?none @@ converter desc) None i)

  let make_opt_all_cmdliner wrap i desc =
    Cmdliner.Arg.(wrap @@ opt_all (converter desc) [] i)

  let to_cmdliner (type a) (t : a t) : a option Cmdliner.Term.t =
    let i = cmdliner_of_info t.info in
    match t.kind with
    | Flag -> Cmdliner.Arg.(value & vflag None [ (Some true, i) ])
    | Opt (default, desc) ->
        make_opt_cmdliner Cmdliner.Arg.value i (Some default) desc
    | Required desc ->
        make_opt_cmdliner Cmdliner.Arg.required i None (some (some desc))
    | Opt_all desc ->
        let list_to_option = function
          | [] -> None
          | _ :: _ as lst -> Some lst
        in
        let wrap arg =
          let open Cmdliner in
          Term.(const list_to_option $ Arg.value arg)
        in
        make_opt_all_cmdliner wrap i desc
end

type 'a key = { name : string; arg : 'a Arg.t; key : 'a Context.key }
type t = Any : 'a key -> t | Run : string -> t
type 'a runtime_key = t

let runtime s = Run s
let equal_any x y = String.equal x.name y.name && Arg.equal x.arg y.arg

let equal x y =
  match (x, y) with
  | Any x, Any y -> equal_any x y
  | Run x, Run y -> String.equal x y
  | _ -> false

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
let name = function Any k -> k.name | Run k -> k
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
let get ctx t = match find ctx t with Some x -> x | None -> Arg.default t.arg
let mem_u ctx t = Context.mem t.key ctx

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
  Set.for_all (function Any x -> mem_u p x | Run _ -> false) v.deps

let peek p v = if mem p v then Some (eval p v) else None
let default v = eval Context.empty v

(* {2 Pretty printing} *)

let pp = Set.pp_elt
let pp_deps fmt v = Set.pp fmt v.deps

let pps p ppf l =
  let l = filter_stage `Configure l in
  let pp' fmt k v =
    let default = if mem_u p k then Fmt.nop else Fmt.any " (default)" in
    Fmt.pf fmt "%a=%a%a"
      Fmt.(styled `Bold string)
      k.name (Arg.pp k.arg) v default ()
  in
  let f fmt = function
    | Run _ -> ()
    | Any k -> (
        match (k.arg.Arg.kind, get p k) with
        | Arg.Required _, None -> Fmt.(styled `Bold string) fmt k.name
        | Arg.Opt _, v -> pp' fmt k v
        | Arg.Required _, v -> pp' fmt k v
        | Arg.Flag, v -> pp' fmt k v
        | Arg.Opt_all _, v -> pp' fmt k v)
    (* Warning 4 and GADT don't interact well. *)
  in
  let pp = Fmt.vbox @@ fun ppf s -> Set.(pp_gen f ppf @@ s) in
  pp ppf l

(* {2 Automatic documentation} *)

let info_arg (type a) (arg : a Arg.kind) =
  match arg with
  | Arg.Required _ -> " This key is required."
  | Arg.Flag -> ""
  | Arg.Opt _ -> ""
  | Arg.Opt_all _ -> ""

let add_extra_info arg =
  match arg.Arg.info.doc with
  | None -> arg
  | Some doc ->
      let doc =
        [ doc; info_arg arg.kind ]
        |> List.filter (( <> ) "")
        |> String.concat " "
      in
      { arg with info = { arg.info with doc = Some doc } }

(* {2 Key creation} *)

(* Unexposed smart constructor. *)
let make ~arg ~name =
  let key = Context.new_key name in
  let arg = add_extra_info arg in
  { arg; name; key }

let create name arg =
  if name = "" then
    invalid_arg "Key.create: key name cannot be the empty string";
  make ~arg ~name

(* {2 Cmdliner interface} *)

let context l =
  let stage = filter_stage `Configure l in
  let names = Names.of_list (Set.elements stage) in
  let gather k rest =
    match k with
    | Run _ -> rest
    | Any k -> (
        let f v p =
          match v with None -> p | Some v -> Context.add k.key v p
        in
        let key = Arg.to_cmdliner k.arg in
        match k.arg.Arg.kind with
        | Arg.Opt _ -> Cmdliner.Term.(const f $ key $ rest)
        | Arg.Required _ -> Cmdliner.Term.(const f $ key $ rest)
        | Arg.Flag -> Cmdliner.Term.(const f $ key $ rest)
        | Arg.Opt_all _ -> Cmdliner.Term.(const f $ key $ rest))
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
