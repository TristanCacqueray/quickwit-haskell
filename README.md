# quickwit-haskell

Haskell libraries for the [quickwit](https://quickwit.io) search engine.

- [quickwit-client](./quickwit-client): API client

## Usage

Start the service:

```
nix develop --command quickwit run
```

Run the client demo:

```
nix develop --command ghcid --command "cabal repl quickwit-client" --test Quickwit.demo
```
