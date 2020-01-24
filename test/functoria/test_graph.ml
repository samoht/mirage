open Functoria
module Graph = Functoria_graph

let zero = Device.v "z" Functoria.Type.job

let x = Device.v "Foo.Bar" Functoria.Type.job

let y = Device.v "X.Y" Functoria.Type.(job @-> job)

let z = Device.v "Bar" Type.job

let apply f d =
  let id = Device.id d in
  let g = Graph.create (Impl.of_device d) in
  let v =
    match
      Graph.find_all g (function Graph.Dev d -> Device.id d = id | _ -> false)
    with
    | [ x ] -> x
    | _ -> assert false
  in
  f v

let var_name x = apply Graph.var_name x

let impl_name x = apply Graph.impl_name x

let id () = Scanf.sscanf (var_name zero) "z__%d" (fun i -> i)

let ident s i x = Fmt.strf "%s__%d" s (i + x)

let test_var_name () =
  let id = id () in
  Alcotest.(check string) "x" (ident "foo_bar" id 1) (var_name x);
  Alcotest.(check string) "y" (ident "x_y" id 2) (var_name y);
  Alcotest.(check string) "z" (ident "bar" id 3) (var_name z)

let test_impl_name () =
  let id = id () in
  Alcotest.(check string) "x" "Foo.Bar" (impl_name x);
  Alcotest.(check string) "y" (ident "X_y" id 2) (impl_name y);
  Alcotest.(check string) "z" "Bar" (impl_name z)

let d1 = Device.v ~packages:[ Package.v "a" ] "Foo.Bar" Type.job

let d2 = Device.v ~packages:[ Package.v "b" ] "Foo.Bar" Type.job

let i1 = Impl.of_device d1

let i2 = Impl.of_device d2

let if1 = Impl.if_ (Functoria_key.pure true) i1 i2

let if2 = Impl.if_ (Functoria_key.pure true) i2 i1

let normalise_lines str =
  let open Astring in
  let lines = String.cuts ~empty:true ~sep:"\n" str in
  let lines =
    List.map
      (fun line -> if String.for_all Char.Ascii.is_blank line then "" else line)
      lines
  in
  String.concat ~sep:"\n" lines

let graph_str g = normalise_lines (Fmt.to_to_string Functoria_graph.pp_dot g)

let digraph i =
  let j = i + 1 and k = i + 2 in
  Fmt.str
    {|digraph G {
  ordering=out;
  %d [shape=box, label="foo_bar__%d
Foo.Bar
", ];
  %d [shape=box, label="foo_bar__%d
Foo.Bar
", ];
  %d [label="If
", ];


  %d -> %d [headport=n, style="dotted,bold", ];
  %d -> %d [headport=n, style="dotted", ];

  }|}
    i i j j k k i k j

let test_graph () =
  let id = id () in
  let t1 = Functoria_graph.create if1 in
  Alcotest.(check string) "t1.dot" (digraph (id + 1)) (graph_str t1);
  let t2 = Functoria_graph.create if2 in
  Alcotest.(check string) "t2.dot" (digraph (id + 4)) (graph_str t2);
  let module M = struct
    type t = (string * string list) list

    let empty = []

    let union = List.append
  end in
  let packages t =
    let ctx = Functoria_key.empty_context in
    Functoria_graph.collect
      (module M)
      (function
        | If _ | App -> []
        | Functoria_graph.Dev d ->
            let pkgs = Functoria_key.(eval ctx (Device.packages d)) in
            List.map
              (fun pkg ->
                (Functoria_package.name pkg, Functoria_package.libraries pkg))
              pkgs)
      (Graph.eval ~context:ctx t)
  in
  let label = Alcotest.(list (pair string (list string))) in
  Alcotest.(check label) "t1" [ ("a", [ "a" ]) ] (packages t1);
  Alcotest.(check label) "t2" [ ("b", [ "b" ]) ] (packages t2)

let suite =
  [
    ("var_name", `Quick, test_var_name);
    ("impl_name", `Quick, test_impl_name);
    ("test_graph", `Quick, test_graph);
  ]
