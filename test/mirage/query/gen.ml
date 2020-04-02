type t = { cmd : string; file : string }

let v x = { cmd = "query " ^ x; file = x }

let gen t =
  Format.printf
    {|
(rule
 (action
  (with-stdout-to %s
  (with-stderr-to %s.err
   (run ./config.exe %s)))))

(rule
 (alias runtest)
 (package mirage)
 (action
  (diff %s.expected %s)))

(rule
 (alias runtest)
 (package mirage)
 (action
  (diff %s.err.expected %s.err)))
|}
    t.file t.file t.cmd t.file t.file t.file t.file

let () =
  List.iter gen
    [
      v "name";
      v "opam";
      v "packages";
      v "files-configure";
      v "files-build";
      v "Makefile";
      { file = "Makefile.no-depext"; cmd = "query Makefile --no-depext" };
      { file = "Makefile.depext"; cmd = "query Makefile --depext" };
      { file = "help-query"; cmd = "help query --man-format=plain" };
      { file = "query-help"; cmd = "query --help=plain" };
      { file = "version"; cmd = "query --version" };
      v "dune-base";
      v "dune-configure";
      v "dune-build";
    ]
