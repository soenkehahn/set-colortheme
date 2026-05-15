{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Arrow
import Control.Exception (SomeException, displayException, try)
import Control.Lens hiding ((<.>))
import Control.Monad
import Cradle
import qualified Data.ByteString
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
import Numeric.Lens
import System.Directory
import System.Environment
import System.Exit (exitWith)
import System.FilePath
import System.IO
import WithCli

main :: IO ()
main = withCli $ \case
  "list" -> list
  theme -> do
    result <- switch theme
    case result of
      Right () -> return ()
      Left () -> exitWith (ExitFailure 1)

getInputPath :: String -> IO FilePath
getInputPath name = do
  val <- lookupEnv name
  case val of
    Just path -> return path
    Nothing -> error $ "environment variable " <> name <> " not set"

data Colortheme = Colortheme
  { variant :: Text,
    palette :: Map Text (RGB Word8)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

instance FromJSON (RGB Word8) where
  parseJSON = withText "RGB Word8" $ \text ->
    return $ toSRGB24 $ sRGB24read @Double $ cs text

list :: IO ()
list = do
  schemesDir <- getInputPath "schemes"
  let baseDir = schemesDir </> "base16"
  themes <-
    listDirectory baseDir
      <&> filter (\f -> takeExtension f == ".yaml")
      <&> map takeBaseName
      <&> sort
  lines <- forM themes $ \theme -> do
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
  schemesDir <- getInputPath "schemes"
  let themeFile = schemesDir </> "base16" </> theme <.> "yaml"
  yaml <- Data.ByteString.readFile themeFile
  case decodeEither' @Colortheme yaml of
    Left err -> error $ "cannot parse " <> themeFile <> ": " <> show err
    Right theme -> return theme

switch :: String -> IO (Either () ())
switch theme = do
  errors <- fmap catMaybes $ forM switchers $ \(name, switcher) -> do
    hPutStrLn stderr $ "\ESC[1mchanging theme in " <> name <> "\ESC[m"
    result <- try $ switcher theme
    case result of
      Left (e :: SomeException) -> do
        hPutStrLn stderr $ name <> ": " <> displayException e
        return $ Just e
      Right () -> return Nothing
  run_ $
    cmd "notify-send"
      & addArgs ["colortheme: " <> theme]
  return $ case errors of
    [] -> Right ()
    _ -> Left ()

switchers :: [(String, String -> IO ())]
switchers =
  ("alacritty", alacritty)
    : ("gtk", gtk)
    : ("helix", helix)
    : ("i3status", i3status)
    : ("nvim", nvim)
    : ("rofi", rofi)
    : ("swaylock", swaylock)
    : ("sway", sway)
    : ("quickshell", quickshell)
    : ("jjui", jjui)
    : ("zellij", zellij)
    : []

zellij :: String -> IO ()
zellij theme = do
  Colortheme _variant palette <- getColortheme theme
  let rgb :: String -> String
      rgb name = let c = palette ! (cs name :: Text)
                 in show (channelRed c) <> " " <> show (channelGreen c) <> " " <> show (channelBlue c)
      bg = rgb "base00"
      fg = rgb "base05"
      bgAlt = rgb "base01"
      focused = rgb "base05"
      unfocused = rgb "base0D"
      orange = rgb "base09"
      yellow = rgb "base0A"
      green = rgb "base0B"
      cyan = rgb "base0C"
      magenta = rgb "base0E"
      config = unindent [i|
        themes {
          default {
            // text in unselected panes
            text_unselected {
              base #{fg}
              background 0
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{green}
              emphasis_3 #{magenta}
            }
            // text in selected panes
            text_selected {
              base #{fg}
              background #{bgAlt}
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{green}
              emphasis_3 #{magenta}
            }
            // active tab in the tab bar
            ribbon_selected {
              base #{focused}
              background #{bg}
              emphasis_0 #{unfocused}
              emphasis_1 #{orange}
              emphasis_2 #{magenta}
              emphasis_3 #{green}
            }
            // inactive tabs in the tab bar
            ribbon_unselected {
              base #{unfocused}
              background #{bg}
              emphasis_0 #{focused}
              emphasis_1 #{unfocused}
              emphasis_2 #{magenta}
              emphasis_3 #{green}
            }
            // header row in tables (e.g. session manager)
            table_title {
              base #{focused}
              background 0
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{green}
              emphasis_3 #{magenta}
            }
            // highlighted row in tables
            table_cell_selected {
              base #{fg}
              background #{bgAlt}
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{green}
              emphasis_3 #{magenta}
            }
            // non-highlighted rows in tables
            table_cell_unselected {
              base #{fg}
              background 0
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{green}
              emphasis_3 #{magenta}
            }
            // highlighted item in lists
            list_selected {
              base #{fg}
              background #{bgAlt}
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{green}
              emphasis_3 #{magenta}
            }
            // non-highlighted items in lists
            list_unselected {
              base #{fg}
              background 0
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{green}
              emphasis_3 #{magenta}
            }
            // thin separator lines for the focused pane (pane_frames false)
            border_selected {
              base #{focused}
              background 0
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{magenta}
              emphasis_3 0
            }
            // thin separator lines for unfocused panes (pane_frames false)
            border_unselected {
              base #{unfocused}
              background 0
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{magenta}
              emphasis_3 0
            }
            // pane frame for the focused pane (pane_frames true)
            frame_selected {
              base #{focused}
              background 0
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{magenta}
              emphasis_3 0
            }
            // pane frames for unfocused panes (pane_frames true)
            frame_unselected {
              base #{unfocused}
              background 0
              emphasis_0 #{orange}
              emphasis_1 #{cyan}
              emphasis_2 #{magenta}
              emphasis_3 0
            }
            // pane frame when selecting a pane to move/resize
            frame_highlight {
              base #{orange}
              background 0
              emphasis_0 #{magenta}
              emphasis_1 #{orange}
              emphasis_2 #{orange}
              emphasis_3 #{orange}
            }
            // exit code indicator when command succeeds
            exit_code_success {
              base #{green}
              background 0
              emphasis_0 #{cyan}
              emphasis_1 #{bg}
              emphasis_2 #{magenta}
              emphasis_3 #{focused}
            }
            // exit code indicator when command fails
            exit_code_error {
              base #{focused}
              background 0
              emphasis_0 #{yellow}
              emphasis_1 0
              emphasis_2 0
              emphasis_3 0
            }
            // cursor colors for different users in multiplayer sessions
            multiplayer_user_colors {
              player_1 #{magenta}
              player_2 #{focused}
              player_3 #{cyan}
              player_4 #{yellow}
              player_5 #{green}
              player_6 #{orange}
              player_7 #{unfocused}
              player_8 0
              player_9 0
              player_10 0
            }
          }
        }
      |]
  home <- getEnv "HOME"
  let dest = home </> ".config/zellij/themes/default.kdl"
  createDirectoryIfMissing True (takeDirectory dest)
  writeFile dest config

jjui :: String -> IO ()
jjui theme = do
  base16Jjui <- getInputPath "base16_jjui"
  copyFromNixStoreIntoHome
    (base16Jjui </> "themes" </> "base16-" <> theme <.> "toml")
    ".config/jjui/themes/main.toml"

quickshell :: String -> IO ()
quickshell theme = do
  base16Quickshell <- getInputPath "base16_quickshell"
  copyFromNixStoreIntoHome
    (base16Quickshell </> "quickshell" </> "base16-" <> theme </> "Colors.qml")
    ".config/quickshell/lib/Colors.qml"

nvim :: String -> IO ()
nvim theme = do
  base16Vim <- getInputPath "base16_vim"
  copyFromNixStoreIntoHome
    (base16Vim </> "colors" </> "base16-" <> theme <.> "vim")
    ".config/nvim/colors.vim"
  runInAllNvims "<esc>:source ~/.config/nvim/colors.vim<CR>"
  where
    runInAllNvims :: String -> IO ()
    runInAllNvims command = do
      xdgRuntimeDir <- getEnv "XDG_RUNTIME_DIR"
      nvimSockets <-
        listDirectory xdgRuntimeDir
          <&> filter (\file -> "nvim." `isPrefixOf` file && ".0" `isSuffixOf` file)
          <&> map (xdgRuntimeDir </>)
      forM_ nvimSockets $ \socket -> do
        run_ $
          cmd "nvim"
            & addArgs
              [ "--server",
                socket,
                "--remote-send",
                command
              ]
            & silenceStdout

helix :: String -> IO ()
helix theme = do
  base16Helix <- getInputPath "base16_helix"
  copyFromNixStoreIntoHome
    (base16Helix </> "themes" </> "base16-" <> theme <.> "toml")
    ".config/helix/themes/mine.toml"
  _ :: ExitCode <-
    run $
      cmd "pkill"
        & addArgs ["--signal", "SIGUSR1", "^hx$"]
  pure ()

gtk :: String -> IO ()
gtk theme = do
  variant <- variant <$> getColortheme theme
  run_ $
    cmd "gsettings"
      & addArgs ["set", "org.gnome.desktop.interface", "color-scheme", "prefer-" <> cs variant]

sway :: String -> IO ()
sway theme = do
  base16I3 <- getInputPath "base16_i3"
  copyFromNixStoreIntoHome
    (base16I3 </> "colors/base16-" <> theme <.> "config")
    ".config/sway/colors"
  colors <- getHexColors theme
  let commands =
        [i|
          output * background #{colors ! 0} solid_color

          # class                 border         bground       text           indicator      child_border
          client.focused          #{colors ! 8}  #{colors ! 0} #{colors ! 8}  #{colors ! 0}  #{colors ! 0}
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
          & filter (\line -> not ("#" `isPrefixOf` line))
  forM_ (commands) $ \command -> do
    run_ $
      cmd "swaymsg"
        & addArgs [command]
        & silenceStdout

alacritty :: String -> IO ()
alacritty theme = do
  tintedTerminal <- getInputPath "tinted_terminal"
  copyFromNixStoreIntoHome
    (tintedTerminal </> "themes/alacritty/base16-" <> theme <.> "toml")
    ".config/alacritty/colors.toml"

i3status :: String -> IO ()
i3status theme = do
  colors <- getHexColors theme
  let foreground = colors ! 13
      error = colors ! 8
      config =
        unindent
          [i|
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
      config =
        unindent
          [i|
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
  base16I3 <- getInputPath "base16_i3"
  readFile (base16I3 </> "colors/base16-" <> theme <.> "config")
    <&> lines
    <&> mapMaybe (stripPrefix "set $base")
    <&> map
      ( words
          >>> (\[a, b] -> (a, b))
          >>> first (^?! hex @Int)
      )
    <&> Map.fromList

rofi :: String -> IO ()
rofi theme = do
  base16Rofi <- getInputPath "base16_rofi"
  copyFromNixStoreIntoHome
    (base16Rofi </> "colors/base16-" <> theme <.> "rasi")
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
      run_ $
        cmd "cp"
          & addArgs ["-r", source, destination]
      run_ $
        cmd "chmod"
          & addArgs ["u+w", "-R", destination]
    (False, True) -> do
      createDirectoryIfMissing True (takeDirectory destination)
      destinationExists <- doesFileExist destination
      when destinationExists $ do
        removeFile destination
      run_ $
        cmd "cp"
          & addArgs [source, destination]
      run_ $
        cmd "chmod"
          & addArgs ["u+w", destination]
    (False, False) -> do
      error $ "file not found: " <> source
    _ -> error "impossible"
