{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    cradle = {
      url = "github:garnix-io/cradle";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };
    tinted-terminal = {
      url = "github:tinted-theming/tinted-terminal";
      flake = false;
    };
    base16-i3 = {
      url = "github:tinted-theming/base16-i3";
      flake = false;
    };
    base16-rofi = {
      url = "github:tinted-theming/base16-rofi";
      flake = false;
    };
    base16-vim = {
      url = "github:tinted-theming/base16-vim";
      flake = false;
    };
    base16-helix = {
      url = "github:tinted-theming/base16-helix";
      flake = false;
    };
    base16-quickshell = {
      url = "github:soenkehahn/base16-quickshell";
      flake = false;
    };
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        flakeInputs = pkgs.linkFarm "flakeInputs"
          (lib.map
            (x: { name = x; path = inputs."${x}"; })
            [
              "schemes"
              "tinted-terminal"
              "base16-i3"
              "base16-rofi"
              "base16-vim"
              "base16-helix"
              "base16-quickshell"
            ]);
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        haskellPackages = pkgs.haskellPackages.override {
          overrides = final: prev: {
            cradle = (inputs.cradle.lib.${system}.mkCradle final);
          };
        };
        ghc = (haskellPackages.ghc.withPackages (p: [
          p.colour
          p.cradle
          p.getopt-generics
          p.interpolate
          p.lens
          p.temporary
          p.yaml
        ]));
        haskellScript =
          { name, text }: pkgs.runCommand
            name
            {
              buildInputs = [ ghc ];
              meta.mainProgram = name;
            }
            ''
              ghc -threaded \
                -Wall \
                -Werror \
                -Wno-name-shadowing \
                -Wno-incomplete-uni-patterns \
                ${pkgs.writeText "Main.hs" text} \
                -o ${name}
              mkdir -p $out/bin
              cp ./${name} $out/bin/
            '';
        haskellPackage = haskellScript {
          name = "set-colortheme";
          text = lib.readFile ./Main.hs;
        };
        set-colortheme = pkgs.writeShellApplication {
          name = "set-colortheme";
          text = ''
            export FLAKE_INPUTS=${flakeInputs}
            exec ${lib.getExe haskellPackage} "$@"
          '';
        };
      in
      {
        packages = {
          default = set-colortheme;
        };
        apps = {
          default = {
            type = "app";
            program = lib.getExe set-colortheme;
          };
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [
            ghc
            pkgs.haskellPackages.haskell-language-server
          ];
        };
      }
    );
}
