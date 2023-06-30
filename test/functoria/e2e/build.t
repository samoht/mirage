Build an application.

  $ ./test.exe configure --file app/config.ml
  $ ./test.exe build -v --file app/config.ml
  test.exe: [INFO] run: build:
                        { "context" = ;
                          "config_file" = app/config.ml;
                          "output" = None;
                          "dry_run" = false }
  test.exe: [INFO] Generating: app/test/dune-workspace.config (base)
  test.exe: [INFO] Generating: dune-project (base)
  test.exe: [INFO] Generating: app/dune.config (base)
  config.exe: [INFO] reading cache app/test/context
  config.exe: [INFO] Name       noop
                     Keys       vote=cat (default),
                                warn_error=false (default)
  config.exe: [INFO] dune build --root .
  $ ls -a app/
  .
  ..
  app.ml
  config.ml
  dist
  dune
  dune.build
  dune.config
  key.ml
  main.exe
  test
  $ ls -a app/test
  .
  ..
  context
  dune-workspace.config
  key_gen.ml
  main.ml
  noop.opam
  vote
  warn_error
  $ ./app/main.exe --required=foo
  Success: hello=Hello World! arg=-
  $ ./test.exe clean --file app/config.ml
  $ ls -a app/
  .
  ..
  app.ml
  config.ml
  key.ml

Test `--output`:

  $ ./test.exe configure --file app/config.ml -o toto
  $ ./test.exe build -v --file app/config.ml
  test.exe: [INFO] run: build:
                        { "context" = ;
                          "config_file" = app/config.ml;
                          "output" = None;
                          "dry_run" = false }
  test.exe: [INFO] Generating: app/test/dune-workspace.config (base)
  test.exe: [INFO] Generating: dune-project (base)
  test.exe: [INFO] Generating: app/dune.config (base)
  config.exe: [INFO] reading cache app/test/context
  config.exe: [INFO] Name       noop
                     Keys       vote=cat (default),
                                warn_error=false (default)
                     Output     toto
  config.exe: [INFO] dune build --root .
  $ ls -a app/
  .
  ..
  app.ml
  config.ml
  dist
  dune
  dune.build
  dune.config
  key.ml
  test
  toto.exe
  $ ls -a app/test
  .
  ..
  context
  dune-workspace.config
  key_gen.ml
  noop.opam
  toto.ml
  vote
  warn_error
  $ ./app/toto.exe --required=foo
  Success: hello=Hello World! arg=-
  $ ./test.exe clean --file app/config.ml
  $ ls -a app/
  .
  ..
  app.ml
  config.ml
  key.ml
