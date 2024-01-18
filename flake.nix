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
        quickwit-ui = hpPrev.callCabal2nix "quickwit-ui" ./quickwit-ui { };
        quickwit-web = hpPrev.callCabal2nix "quickwit-web" ./quickwit-web { };

        # Bump http2 to the latest version
        http2 = hpPrev.http2_5_0_0;
        # Warp build fails because of a missing command in the test depends
        warp = pkgs.haskell.lib.dontCheck hpPrev.warp_3_3_31;
      };
      hsPkgs = pkgs.haskellPackages.extend haskellExtend;

      ciTools = [
        quickwit
        pkgs.cabal-install
        pkgs.haskellPackages.fourmolu
        pkgs.hlint
      ];
      devTools = [
        pkgs.haskell-language-server
        pkgs.ghcid
        pkgs.haskellPackages.cabal-fmt
        pkgs.just
      ];
    in {
      packages.x86_64-linux.default = quickwit;
      devShell.x86_64-linux = hsPkgs.shellFor {
        packages = p: [
          p.quickwit-client
          p.log-quickwit
          p.journald-ingestor
          p.quickwit-ui
          p.quickwit-web
        ];
        buildInputs = ciTools ++ devTools;
        ROBOTO_TTF = "${pkgs.roboto}/share/fonts/truetype/Roboto-Regular.ttf";
      };
    };
}
