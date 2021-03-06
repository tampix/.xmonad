-- XMobase base
import XMonad
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as W

-- XMonad utils
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap)

-- Utils
import Control.Monad.IO.Class (liftIO)
import System.Posix.Env (putEnv)

-- Variables
myWorkspaces = ["term", "firefox", "chrome", "emacs", "dev", "misc"]

myModMask = mod4Mask

myEditor = "emacsclient --alternate-editor=\"\" -c \"$@\""

myWebBrowser = "icecat"

myLauncher = "rofi -show drun -theme slate -icon-theme Papirus -show-icons"

myScreenshot = "scrot -q 90"

myTerminal = "urxvt" -- TODO change with urxvtcd

-- Key configuration
myKeyBindings cfg =
  [ ("M-<Return>",   spawn $ terminal cfg)   -- terminal
  , ("M-S-<Return>", windows W.swapMaster)   -- swap master
  , ("M-w",          spawn myWebBrowser)     -- webbrowser
  , ("M-e",          spawn myEditor)         -- editor
  , ("M-r",          spawn myLauncher)       -- launcher
  , ("<Print>",      spawn myScreenshot)     -- screenshot
  ]
  ++
  -- Handle workspaces
  -- M-[1..N] Switch to workspace 1..N
  -- M-S-[1..N] Shift to workspace 1..N
  [ (("M-" ++ secondModKey ++ numKey), windows $ fn workspace)
  | (numKey, workspace) <- zip (map show [1..]) (workspaces cfg)
  , (secondModKey, fn) <- [("S-", W.shift), ("", W.greedyView)]
  ]

-- Hook ran before xmonad is actually started.
myStartupHook = do
  return ()
  mapM_ (liftIO . putEnv)
    [ "_JAVA_AWT_WM_NONREPARENTING=1"
    , "EDITOR=" ++ myEditor]
  mapM_ spawn
    [ "nitrogen --restore"             -- load wallpaper
    , "xrdb ~/.Xresources"             -- load config
    , "xsetroot -cursor_name left_ptr" -- load mouse cursor
    ]
  checkKeymap myConfig (myKeyBindings myConfig)

-- Main configuration.
myConfig = def
  { modMask = myModMask
  , terminal = myTerminal
  , workspaces = myWorkspaces
  , startupHook = myStartupHook
  }
  `additionalKeysP`
  (myKeyBindings myConfig)

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleBarKeyBiding myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Bar's pretting printing configuration,
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the bar.
toggleBarKeyBiding XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
