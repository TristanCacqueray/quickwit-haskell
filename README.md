# quickwit-haskell

Haskell libraries for the [quickwit](https://quickwit.io) search engine.

- [quickwit-client](./quickwit-client): API client
- [log-quickwit](./log-quickwit): log-base backend
- [journald-ingestor](./journald-ingestor): a demo application to ingest journald events
- [quickwit-ui](./quickwit-ui): a [monomer](https://github.com/fjvallarino/monomer) base gui to perform search

## Usage

Start the service:

```
nix develop --command quickwit run
```

Run the client demo:

```
nix develop --command ghcid --command "cabal repl quickwit-client" --test Quickwit.demo
```
