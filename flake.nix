{
  description = "quickwit";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  #  inputs.openapi-codegen.url =
  #    "github:Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        localSystem = "x86_64-linux";
        config.allowUnfree = true;
      };

      # A standalone/easy package for quickwit based on the binary release
      quickwit = pkgs.stdenv.mkDerivation rec {
        pname = "quickwit";
        version = "0.6.5";
        src = pkgs.fetchurl {
          url =
            "https://github.com/quickwit-oss/quickwit/releases/download/v${version}/quickwit-v${version}-x86_64-unknown-linux-gnu.tar.gz";
          sha256 = "sha256-yWJZVa7zKRVGp1ipoC5SwwR5+FYh4eDD5QjKd/ZvGAk=";
        };
        unpackPhase = ''
          # Unpack sources
          tar xzvf $src
          mkdir -p $out/bin
          mv quickwit-*/quickwit $out/bin
        '';
        dontStrip = true;
        dontInstall = true;
      };

      # The haskell package set override
      haskellExtend = hpFinal: hpPrev: {
        # Add the local libraries
        quickwit-client =
          hpPrev.callCabal2nix "quickwit-client" ./quickwit-client { };
        log-quickwit = hpPrev.callCabal2nix "log-quickwit" ./log-quickwit { };
        journald-ingestor =
          hpPrev.callCabal2nix "journald-ingestor" ./journald-ingestor { };

        # Bump http2 to the latest version
        http2 = let
          src = pkgs.fetchFromGitHub {
            owner = "kazu-yamamoto";
            repo = "http2";
            rev = "v5.0.0";
            sha256 = "sha256-Yf8XO8UQAME8xdPHb3XV01UcAlp2P6f55hOacqLpUpA=";
          };
        in hpPrev.callCabal2nix "http2" src { };
        network-control = let
          src = pkgs.fetchFromGitHub {
            owner = "kazu-yamamoto";
            repo = "network-control";
            rev = "6fdc7a30bacb18287db3f4be54d5f4ef77a1af31";
            sha256 = "sha256-wgkbs05s4u4+mAFIH6lKflgIZ/jKxVxIqejUIq/uf0Y=";
          };
        in hpPrev.callCabal2nix "network-control" src { };
      };
      hsPkgs = pkgs.haskellPackages.extend haskellExtend;

      ciTools = [ quickwit pkgs.cabal-install pkgs.haskellPackages.fourmolu ];
      devTools = [
        pkgs.haskell-language-server
        pkgs.ghcid
        pkgs.haskellPackages.cabal-fmt
        pkgs.just
      ];
    in {
      packages.x86_64-linux.default = quickwit;
      devShell.x86_64-linux = hsPkgs.shellFor {
        packages = p: [ p.quickwit-client p.log-quickwit p.journald-ingestor ];
        buildInputs = ciTools ++ devTools;
      };
    };
}
