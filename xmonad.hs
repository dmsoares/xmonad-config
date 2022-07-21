import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet           as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Ungrab

main :: IO ()
main =
  xmonad . ewmh =<< statusBar "xmobar" myXmobarPP toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = m} = (m .|. shiftMask, xK_b)

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =
  def
    { modMask = mod4Mask -- Rebind Mod to the Super key
    , focusedBorderColor = myFocusedBorderColor
    , workspaces = myWorkspaces
    }
    `additionalKeysP` (myKeys ++ myWSKeys)

myKeys :: [(String, X ())]
myKeys =
  [ ("M-p", spawn "rofi -show combi") -- Launch Rofi
  , ("M-b", spawn "brave-browser-stable") -- Launch Brave Browser
  , ("M-u", spawn "emacsclient -c -a 'emacs' &") -- Launch Emacs client
  ]

myWorkspaces :: [String]
myWorkspaces = reverse [[c] | c <- ['1' .. '9']]

myWSKeys :: [(String, X ())]
myWSKeys = [("M-" ++ n, windows $ W.greedyView n) | n <- myWorkspaces]

myFocusedBorderColor :: String
myFocusedBorderColor = "#e95678"

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap (blue "[") (blue "]")
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _] -> [ws, l]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
