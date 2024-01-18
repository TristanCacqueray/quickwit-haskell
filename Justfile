journald:
  ghcid --command "cabal repl journald-ingestor" --test Main.main -W

ui:
  ghcid --command "cabal repl quickwit-ui" --test Main.main -W

web:
  ghcid --command "cabal repl quickwit-web" --test Main.main -W

ci: build format lint

build:
  cabal build -O0 all --ghc-options=-Werror

lint:
  hlint -XQuasiQuotes journald-ingestor log-quickwit quickwit-client

format:
  fourmolu -i .
