{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
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
    base16-vim = {
      url = "github:tinted-theming/base16-vim";
      flake = false;
    };
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        haskellScript =
          let
            haskellPackages = pkgs.haskellPackages.override {
              overrides = final: prev: {
                cradle = (inputs.cradle.lib.${system}.mkCradle final);
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
              import Control.Monad
              import Cradle
              import Data.Functor
              import Data.List
              import System.Directory
              import System.Environment
              import System.FilePath
              import System.IO
              import WithCli

              main :: IO ()
              main = withCli $ \ (theme :: String) -> do
                hPutStrLn stderr "gtk"
                gtk theme
                hPutStrLn stderr "nvim"
                nvim theme

              nvim :: String -> IO ()
              nvim theme = do
                let colorsVimFile = "${inputs.base16-vim}" </> "colors" </> "base16-" <> theme <.> "vim"
                xdgRuntimeDir <- getEnv "XDG_RUNTIME_DIR"
                nvimSockets <- listDirectory xdgRuntimeDir
                  <&> filter (\ file -> "nvim." `isPrefixOf` file && ".0" `isSuffixOf` file)
                  <&> map (xdgRuntimeDir </>)
                forM_ nvimSockets $ \ socket -> do
                  run_ $ cmd "nvim"
                    & addArgs [
                      "--server",
                      socket,
                      "--remote-send",
                      "<esc>:source " <> colorsVimFile <> "<CR>"
                    ]

              gtk :: String -> IO ()
              gtk theme = do
                createColorTheme theme "set-colortheme-temporary"
                switchToColorTheme "set-colortheme-temporary"
                createColorTheme theme "set-colortheme"
                switchToColorTheme "set-colortheme"

              createColorTheme :: String -> String -> IO ()
              createColorTheme base16Theme name = do
                copyFromNixStoreIntoHome
                  ("${inputs.FlatColor}" </> "gtk-3.20")
                  (".themes" </> name)
                copyFromNixStoreIntoHome
                  ("${inputs.base16-gtk-flatcolor}" </> "gtk-3" </> "base16-" <> base16Theme <> "-gtk.css")
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
                    destinationExists <- doesDirectoryExist destination
                    when destinationExists $ do
                      removeDirectoryRecursive destination
                    createDirectoryIfMissing True $ destination </> ".."
                    run_ $ cmd "cp"
                      & addArgs [ "-r", source, destination ]
                    run_ $ cmd "chmod"
                      & addArgs [ "u+w", "-R", destination ]
                  (False, True) -> do
                    destinationExists <- doesFileExist destination
                    when destinationExists $ do
                      removeFile destination
                    run_ $ cmd "cp"
                      & addArgs [ source, destination ]
                    run_ $ cmd "chmod"
                      & addArgs [ "u+w", destination ]
                  (False, False) -> do
                    error $ "file not found: " <> source
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
