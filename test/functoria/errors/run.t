Try to use the locally defined `functoria` in cram tests:

  $ ./test.exe configure -f test.ml
  File "dune.config", line 9, characters 12-14:
  9 |  (libraries f0 functoria))
                  ^^
  Error: Library "f0" not found.
  -> required by _build/default/.config.eobjs/byte/dune__exe__Config.cmi
  -> required by _build/default/.config.eobjs/native/dune__exe__Config.cmx
  -> required by _build/default/config.exe
  run ['dune' 'build' './config.exe' '--root' '.' '--workspace'
       './test/dune-workspace.config']: exited with 1
  [1]
