open Functoria
open Cmdliner

let key_a = Key.create "a" Arg.(value & flag & info [ "a" ]) Fmt.bool
let key_b = Key.create "b" Arg.(value & opt int 0 & info [ "b" ]) Fmt.int

let key_c =
  Key.create "c"
    Arg.(required & opt (some string) None & info [ "c" ])
    Fmt.Dump.string

let key_d =
  Key.create "d" Arg.(value & opt_all int [] & info [ "d" ]) Fmt.(Dump.list int)

let empty = Context.empty
let ( & ) (k, v) c = Key.add_to_context k v c
let ( && ) x y = x & y & empty

let test_eval () =
  let context = (key_a, true) & (key_b, 0) && (key_c, "foo") in

  let if_ = Key.if_ Key.(value key_a) "hello" "world" in
  let r = Key.eval context if_ in
  Alcotest.(check string) "if" "hello" r;

  let match_1 =
    Key.match_ Key.(value key_b) (function 0 -> "hello" | _ -> "world")
  in
  let r = Key.eval context match_1 in
  Alcotest.(check string) "match 1" "hello" r;

  let match_2 =
    Key.match_ Key.(value key_c) (function "foo" -> "hello" | _ -> "world")
  in
  let r = Key.eval context match_2 in
  Alcotest.(check string) "match 1" "hello" r

let keys = Key.Set.of_list Key.[ v key_a; v key_b; v key_c; v key_d ]
let keys_no_required = Key.Set.of_list Key.[ v key_a; v key_b; v key_d ]

let eval f keys argv =
  let argv = Array.of_list ("" :: argv) in
  match
    Cmdliner.Cmd.eval_value ~argv
      (Cmdliner.Cmd.v (Cmdliner.Cmd.info "keys") (f keys))
  with
  | Error _ -> Alcotest.fail "Error"
  | Ok (`Ok x) -> x
  | Ok `Version -> Alcotest.fail "version"
  | Ok `Help -> Alcotest.fail "help"

exception Error

let test_get () =
  let context = eval Key.context keys [ "-a"; "-c"; "foo" ] in
  Alcotest.(check bool) "get a" true (Key.get context key_a);
  Alcotest.(check int) "get b" 0 (Key.get context key_b);
  Alcotest.(check (option string)) "get c" (Some "foo") (Key.find context key_c);

  let context = eval Key.context keys_no_required [ "-a" ] in
  Alcotest.(check (option string))
    "get c with_required:false" None (Key.find context key_c);

  Alcotest.check_raises "get c with_required:true" Error (fun () ->
      try ignore (eval Key.context keys [ "-a" ]) with _ -> raise Error)

let test_find () =
  let context = eval Key.context keys_no_required [] in
  Alcotest.(check (option bool)) "find a" (Some false) (Key.find context key_a);
  Alcotest.(check (option int)) "find b" (Some 0) (Key.find context key_b);
  Alcotest.(check (option string)) "find c" None (Key.find context key_c)

let test_diff () =
  let cache = (key_a, true) && (key_c, "foo") in
  let cli = (key_a, false) && (key_b, 2) in
  let diff = Context.diff ~base:cache cli in
  Alcotest.(check (list string)) "diff" [ "a" ] diff

let key = Alcotest.testable (Fmt.of_to_string Key.name) Key.equal

let test_equal () =
  let k1 =
    Key.(create "foo" Arg.(value & opt int 1 (info [ "foo" ])) Fmt.int)
  in
  let k2 =
    Key.(create "foo" Arg.(value & opt int 2 (info [ "foo" ])) Fmt.int)
  in
  let k3 =
    Key.(create "foo" Arg.(value & opt int 1 (info [ "foo" ])) Fmt.int)
  in
  Alcotest.(check @@ neg key) "different defaults" k1 k2;
  Alcotest.(check @@ key) "same defaults" k1 k3

let test_cmdliner () =
  let k1 =
    Key.(v @@ create "foo" Arg.(value & opt int 1 (info [ "foo" ])) Fmt.int)
  in
  let k2 =
    Key.(v @@ create "foo" Arg.(value & opt int 2 (info [ "foo" ])) Fmt.int)
  in
  let keys = Key.Set.of_list [ k1; k2 ] in
  let context = Key.context keys in
  let _ = eval (fun x -> x) context [] in
  ()

let test_opt_all () =
  let context =
    eval Key.context keys_no_required [ "-d"; "1"; "-d"; "2"; "-d"; "3" ]
  in
  Alcotest.(check (list int)) "get d" [ 1; 2; 3 ] (Key.get context key_d);
  let context = eval Key.context keys_no_required [] in
  Alcotest.(check (list int)) "get d" [] (Key.get context key_d);
  match
    Cmdliner.Cmd.eval_value ~argv:[| ""; "-d" |]
      Cmdliner.(Cmd.v (Cmd.info "keys") (Key.context keys_no_required))
  with
  | Ok (`Ok _ | `Help | `Version) ->
      Alcotest.failf "Invalid given command-line, eval must fail."
  | Error _ -> Alcotest.(check pass) "invalid opt-all argument" () ()

let suite =
  List.map
    (fun (n, f) -> (n, `Quick, f))
    [
      ("equal", test_equal);
      ("eval", test_eval);
      ("get", test_get);
      ("find", test_find);
      ("diff", test_diff);
      ("cmdliner", test_cmdliner);
      ("opt-all", test_opt_all);
    ]
