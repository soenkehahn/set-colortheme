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
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        haskellPackages = pkgs.haskellPackages.override {
          overrides = final: prev: {
            cradle = (inputs.cradle.lib.${system}.mkCradle final);
          };
        };
        themeInputs = removeAttrs inputs [ "self" "nixpkgs" "flake-utils" "cradle" ];
        toEnvName = name: builtins.replaceStrings ["-"] ["_"] name;
        themeInputEnvVars = lib.mapAttrs' (name: path:
          lib.nameValuePair (toEnvName name) path
        ) themeInputs;
        wrapperArgs = lib.concatStringsSep " "
          (lib.mapAttrsToList (name: path: ''--set ${name} "${path}"'') themeInputEnvVars);
        shellExports = lib.concatStringsSep "\n"
          (lib.mapAttrsToList (name: path: ''export ${name}="${path}"'') themeInputEnvVars);
        set-colortheme-unwrapped = haskellPackages.callCabal2nix "set-colortheme" ./. {};
        set-colortheme = pkgs.runCommand "set-colortheme" {
          nativeBuildInputs = [ pkgs.makeWrapper ];
          meta.mainProgram = "set-colortheme";
        } ''
          mkdir -p $out/bin
          makeWrapper ${lib.getExe set-colortheme-unwrapped} $out/bin/set-colortheme \
            ${wrapperArgs}
        '';
      in
      rec {
        packages.default = set-colortheme;
        apps = {
          set-colortheme = {
            type = "app";
            program = builtins.toString (lib.getExe packages.default);
          };
        };
        devShells.default = haskellPackages.shellFor {
          packages = _: [ set-colortheme-unwrapped ];
          nativeBuildInputs = [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
          ];
          shellHook = shellExports;
        };
      }
    );
}
