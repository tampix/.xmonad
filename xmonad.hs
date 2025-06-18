-- XMobase base
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (docks, docksEventHook, avoidStruts)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers (doRectFloat, isFullscreen, doFullFloat)
import qualified XMonad.StackSet as W

-- XMonad utils
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap)

-- Utils
import System.Posix.Env (putEnv)

-- Variables
myWorkspaces = ["term", "firefox", "chrome", "emacs", "dev", "misc"]

myModMask = mod4Mask

myEditor = "emacsclient --alternate-editor=\"\" -c \"$@\""

myWebBrowser = "firefox"

myLauncher = "rofi -show drun"

myScreenshot = "scrot -q 90"

myTerminal = "xterm"

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
  mapM_ (liftIO . putEnv)
    [ "_JAVA_AWT_WM_NONREPARENTING=1"
    , "EDITOR=" ++ myEditor
    ]
  checkKeymap myConfig (myKeyBindings myConfig)

myManageHook = composeAll
  [ isFullscreen --> doFullFloat
  , role =? "GtkFileChooserDialog" --> doRectFloat (W.RationalRect 0.33 0.33 0.66 0.66)
  ]
  where role = stringProperty "WM_WINDOW_ROLE"

myLogHook = return ()

myHandleEventHook = handleEventHook def

-- Main configuration.
myConfig = def
  { modMask         = myModMask
  , terminal        = myTerminal
  , workspaces      = myWorkspaces
  , startupHook     = myStartupHook
  , manageHook      = myManageHook <+> manageHook def
  , logHook         = myLogHook
  , handleEventHook = myHandleEventHook
  }
  `additionalKeysP`
  (myKeyBindings myConfig)

myBar = "xmobar"

myPP = xmobarPP
  { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the bar.
toggleBarKeyBiding XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main :: IO ()
main = do
  conf <- statusBar myBar myPP toggleBarKeyBiding myConfig
  xmonad $ ewmhFullscreen $ ewmh $ docks conf
