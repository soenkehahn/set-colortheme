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
                  p.colour
                  p.cradle
                  p.getopt-generics
                  p.interpolate
                  p.lens
                  p.temporary
                  p.yaml
                ]))
              ];
              meta.mainProgram = name;
            }
            ''
              ghc -threaded \
                -Wall \
                -Werror \
                -Wno-name-shadowing \
                -Wno-incomplete-uni-patterns \
                -XDeriveAnyClass \
                -XDerivingStrategies \
                -XLambdaCase \
                -XQuasiQuotes \
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
            name = "set-colortheme";
            text = ''
              {-# OPTIONS_GHC -fno-warn-orphans #-}

              import Control.Arrow
              import Control.Lens hiding ((<.>))
              import Control.Monad
              import Cradle
              import Data.Colour.SRGB
              import Data.List
              import Data.Map.Strict (Map, toAscList, (!))
              import qualified Data.Map.Strict as Map
              import Data.Maybe
              import Data.String.Conversions
              import Data.String.Interpolate
              import Data.String.Interpolate.Util
              import Data.Text (Text)
              import Data.Word
              import Data.Yaml
              import qualified Data.ByteString
              import Numeric.Lens
              import System.Directory
              import System.Environment
              import System.FilePath
              import System.IO
              import WithCli

              main :: IO ()
              main = withCli $ \case
                "list" -> list
                theme -> switch theme

              data Colortheme = Colortheme {
                variant :: Text,
                palette :: Map Text (RGB Word8)
              }
                deriving stock (Show, Generic)
                deriving anyclass (FromJSON)

              instance FromJSON (RGB Word8) where
                parseJSON = withText "RGB Word8" $ \ text ->
                  return $ toSRGB24 $ sRGB24read @Double $ cs text

              list :: IO ()
              list = do
                let baseDir = "${inputs.schemes}/base16"
                themes <- listDirectory baseDir
                  <&> filter (\ f -> takeExtension f == ".yaml")
                  <&> map takeBaseName
                  <&> sort
                lines <- forM themes $ \ theme -> do
                  Colortheme _variant palette <- getColortheme theme
                  let colors = concat $ flip map (toAscList palette) $ \(_name, value) ->
                        "\ESC[48;2;"
                        <> intercalate
                          ";"
                          (map show [channelRed value, channelGreen value, channelBlue value])
                        <> "m  \ESC[m"
                  return $ colors <> " " <> theme
                putStr $ unlines lines

              getColortheme :: String -> IO Colortheme
              getColortheme theme = do
                let themeFile = "${inputs.schemes}/base16" </> theme <.> "yaml"
                yaml <- Data.ByteString.readFile themeFile
                case decodeEither' @Colortheme yaml of
                  Left err -> error $ "cannot parse " <> themeFile <> ": " <> show err
                  Right theme -> return theme

              switch :: String -> IO ()
              switch theme = do
                forM_ switchers $ \ (name, switcher) -> do
                  hPutStrLn stderr $ "changing theme in " <> name
                  switcher theme
                run_ $ cmd "notify-send"
                  & addArgs ["colortheme: " <> theme]

              switchers :: [(String, String -> IO ())]
              switchers =
                ("alacritty", alacritty) :
                ("gtk", gtk) :
                ("helix", helix) :
                ("i3status", i3status) :
                ("nvim", nvim) :
                ("rofi", rofi) :
                ("swaylock", swaylock) :
                ("sway", sway) :
                []

              nvim :: String -> IO ()
              nvim theme = do
                  copyFromNixStoreIntoHome
                    ("${inputs.base16-vim}" </> "colors" </> "base16-" <> theme <.> "vim")
                    ".config/nvim/colors.vim"
                  runInAllNvims "<esc>:source ~/.config/nvim/colors.vim<CR>"
                where
                  runInAllNvims :: String -> IO ()
                  runInAllNvims command = do
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
                            command
                          ]
                        & silenceStdout

              helix :: String -> IO ()
              helix theme = do
                copyFromNixStoreIntoHome
                  ("${inputs.base16-helix}" </> "themes" </> "base16-" <> theme <.> "toml")
                  ".config/helix/themes/mine.toml"
                _ :: ExitCode <- run $ cmd "pkill" &
                  addArgs ["--signal", "SIGUSR1", "^hx$"]
                pure ()

              gtk :: String -> IO ()
              gtk theme = do
                  variant <- variant <$> getColortheme theme
                  run_ $ cmd "gsettings"
                    & addArgs [ "set", "org.gnome.desktop.interface", "color-scheme", "prefer-" <> cs variant]

              sway :: String -> IO ()
              sway theme = do
                copyFromNixStoreIntoHome
                  ("${inputs.base16-i3}/colors/base16-" <> theme <.> "config")
                  ".config/sway/colors"
                colors <- getHexColors theme
                let commands = [i|
                        output * background #{colors ! 0} solid_color

                        # class                 border         bground       text           indicator      child_border
                        client.focused          #{colors ! 8}  #{colors ! 0} #{colors ! 8}  #{colors ! 12} #{colors ! 0}
                        client.unfocused        #{colors ! 13} #{colors ! 0} #{colors ! 13} #{colors ! 0}  #{colors ! 0}
                        client.focused_inactive #{colors ! 13} #{colors ! 0} #{colors ! 8}  #{colors ! 0}  #{colors ! 0}
                        client.urgent           #{colors ! 8}  #{colors ! 8} #{colors ! 0}

                        bar bar-0 colors background #{colors ! 0}
                        bar bar-0 colors statusline #{colors ! 13}
                        bar bar-0 colors separator #{colors ! 13}
                        bar bar-0 colors focused_workspace #{colors ! 8} #{colors ! 0} #{colors ! 8}
                        bar bar-0 colors inactive_workspace #{colors ! 13} #{colors ! 0} #{colors ! 13}
                        bar bar-0 colors active_workspace #{colors ! 13} #{colors ! 0} #{colors ! 8}
                        bar bar-0 colors urgent_workspace #{colors ! 8} #{colors ! 8} #{colors ! 0}
                      |]
                      & unindent
                      & lines
                      & filter (/= "")
                      & filter (\ line -> not ("#" `isPrefixOf` line))
                forM_ (commands) $ \ command -> do
                  run_ $ cmd "swaymsg"
                    & addArgs [command]
                    & silenceStdout

              alacritty :: String -> IO ()
              alacritty theme = do
                copyFromNixStoreIntoHome
                  ("${inputs.tinted-terminal}/themes/alacritty/base16-" <> theme <.> "toml")
                  ".config/alacritty/colors.toml"

              i3status :: String -> IO ()
              i3status theme = do
                colors <- getHexColors theme
                let foreground = colors ! 13
                    error = colors ! 8
                    config = unindent [i|
                      general {
                        colors = true
                        color_good = '#{foreground}'
                        color_degraded = '#{error}'
                        color_bad = '#{error}'
                        interval = 5
                      }

                      order += 'memory'
                      order += 'cpu_usage'
                      order += 'wireless _first_'
                      order += 'battery all'
                      order += 'tztime local'

                      wireless _first_ {
                        format_up = 'W: %ip'
                        format_down = 'W: down'
                      }

                      battery all {
                        format = '%status %percentage'
                        low_threshold = 10
                        threshold_type = 'percentage'
                      }

                      tztime local {
                        format = '%Y-%m-%d %H:%M'
                      }

                      cpu_usage {
                        format = 'cpu: %usage'
                      }

                      disk '/' {
                        format = '%avail'
                      }

                      memory {
                        format = 'Free RAM: %available'
                        threshold_degraded = 10%
                        threshold_critical = 5%
                      }
                    |]
                home <- getEnv "HOME"
                writeFile (home </> ".config/i3status/config") config

              swaylock :: String -> IO ()
              swaylock theme = do
                colors <- getHexColors theme
                let foreground = colors ! 13
                    error = colors ! 8
                    background = colors ! 0
                    config = unindent [i|
                      indicator-radius=100
                      indicator-thickness=40

                      color=#{background}
                      ring-color=00000000
                      inside-color=00000000
                      line-color=00000000
                      separator-color=00000000
                      key-hl-color=#{foreground}

                      inside-ver-color=00000000
                      ring-ver-color=#{foreground}
                      line-ver-color=00000000
                      text-ver-color=#{foreground}

                      inside-clear-color=00000000
                      ring-clear-color=#{foreground}
                      line-clear-color=00000000
                      text-clear-color=#{foreground}

                      inside-wrong-color=00000000
                      ring-wrong-color=#{error}
                      line-wrong-color=00000000
                      text-wrong-color=#{error}
                    |]
                home <- getEnv "HOME"
                writeFile (home </> ".config/swaylock/config") config

              getHexColors :: String -> IO (Map Int String)
              getHexColors theme = do
                readFile ("${inputs.base16-i3}/colors/base16-" <> theme <.> "config")
                  <&> lines
                  <&> mapMaybe (stripPrefix "set $base")
                  <&> map (
                    words >>>
                    (\[a, b] -> (a, b)) >>>
                    first (^?! hex @Int)
                  )
                  <&> Map.fromList

              rofi :: String -> IO ()
              rofi theme = do
                copyFromNixStoreIntoHome
                  ("${inputs.base16-rofi}/colors/base16-" <> theme <.> "rasi")
                  ".config/rofi/colors.rasi"

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
          set-colortheme = {
            type = "app";
            program = builtins.toString (lib.getExe packages.default);
          };
        };
      }
    );
}
