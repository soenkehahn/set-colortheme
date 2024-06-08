{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    cradle = {
      url = "github:garnix-io/cradle";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    FlatColor = {
      url = "github:soenkehahn/FlatColor/sh";
      flake = false;
    };
    base16-gtk-flatcolor = {
      url = "github:tinted-theming/base16-gtk-flatcolor";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, cradle, FlatColor, base16-gtk-flatcolor }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;

        haskellScript =
          let
            haskellPackages = pkgs.haskellPackages.override {
              overrides = final: prev: {
                cradle = (cradle.lib.${system}.mkCradle final);
              };
            };
          in
          { name, text }: pkgs.runCommand
            name
            {
              buildInputs = [
                (haskellPackages.ghc.withPackages (p: [
                  p.cradle
                  p.getopt-generics
                  p.temporary
                ]))
              ];
              meta.mainProgram = name;
            }
            ''
              ghc -threaded \
                -Wall -Werror \
                -Wno-name-shadowing \
                -XViewPatterns \
                ${pkgs.writeText "Main.hs" text} \
                -o ${name}
              mkdir -p $out/bin
              cp ./${name} $out/bin/
            '';
      in
      rec {
        packages = {
          default = haskellScript {
            name = "set-colortheme-new";
            text = ''
              import Cradle
              import System.Directory
              import System.Environment
              import System.FilePath
              import WithCli

              main :: IO ()
              main = withCli $ \ (theme :: String) -> do
                createColorTheme theme "set-colortheme-temporary"
                switchToColorTheme "set-colortheme-temporary"
                createColorTheme theme "set-colortheme"
                switchToColorTheme "set-colortheme"

              createColorTheme :: String -> String -> IO ()
              createColorTheme base16Theme name = do
                copyFromNixStoreIntoHome
                  ("${FlatColor}" </> "gtk-3.20")
                  (".themes" </> name)
                copyFromNixStoreIntoHome
                  ("${base16-gtk-flatcolor}" </> "gtk-3" </> "base16-" <> base16Theme <> "-gtk.css")
                  (".themes" </> name </> "gtk-3.20" </> "colors.css")

              switchToColorTheme :: String -> IO ()
              switchToColorTheme name = do
                run_ $ cmd "gsettings"
                  & addArgs [ "set", "org.gnome.desktop.interface", "gtk-theme", name]

              copyFromNixStoreIntoHome :: FilePath -> FilePath -> IO ()
              copyFromNixStoreIntoHome source destination = do
                destination <- do
                  home <- getEnv "HOME"
                  return $ home </> destination
                isDir <- doesDirectoryExist source
                isFile <- doesFileExist source
                case (isDir, isFile) of
                  (True, False) -> do
                    removeDirectoryRecursive destination
                    createDirectoryIfMissing True $ destination </> ".."
                    run_ $ cmd "cp"
                      & addArgs [ "-r", source, destination ]
                    run_ $ cmd "chmod"
                      & addArgs [ "u+w", "-R", destination ]
                  (False, True) -> do
                    removeFile destination
                    run_ $ cmd "cp"
                      & addArgs [ source, destination ]
                    run_ $ cmd "chmod"
                      & addArgs [ "u+w", destination ]
                  _ -> error "impossible"
            '';
          };
        };
        apps = {
          set-colortheme-new = {
            type = "app";
            program = builtins.toString (lib.getExe packages.default);
          };
        };
      }
    );
}
