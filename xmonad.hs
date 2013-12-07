import XMonad
import Graphics.X11.ExtraTypes.XF86  -- for the names of laptop's function keys e.g. XF86MonBrightnessUp
import XMonad.Layout.Spacing  -- add some space between windows
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders(smartBorders)
--import XMonad.Layout.FullScreen
import XMonad.Layout.LayoutHints
import XMonad.Layout.SimpleFloat
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops(fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import System.IO
import XMonad.Hooks.UrgencyHook
-- need xmonad-extras package, installing it from cabal
import XMonad.Actions.Volume(toggleMute, lowerVolume, raiseVolume)


-- app settings
terminals = ["terminator", "gnome-terminal"]
myTerminal = terminals !! 0
myFileManager = "marlin"

-- basic stuff
myModMask = mod4Mask  -- Win key or Super_L
myBorderWidth = 1
marginBetweenWindows = 2  -- add marginBetweenWindows pixels space between windows
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#ff0000"
myFocusFollowsMouse = False
myEventHook = fullscreenEventHook  -- for some apps like chrome which has a problem with fullscreen

myTitleColor = "#eeeeee"
myCurrentWSColor = "#ff6600"
myVisibleWSColor = "#c185a7" -- color of inactive workspace
myUrgentWSColor = "#cc0000" -- color of workspace with 'urgent' window
myCurrentWSLeft = "[" -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft = "(" -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft = "{" -- wrap urgent workspace with these
myUrgentWSRight = "}"
myTitleLength = 80


-- keybindings, `((modkey, key), action)`
-- use `.|.` to combinate modify keys
-- if modkey is 0, it means we need't hold any key
-- use `xev` to find the name of every keyboard button
myKeyBindings = [
    ((myModMask, xK_f), sendMessage ToggleStruts)
    , ((myModMask, xK_a), sendMessage MirrorShrink)
    , ((myModMask, xK_z), sendMessage MirrorExpand)
    , ((myModMask, xK_q), spawn "killall stalonetray;killall conky;xmonad --recompile && xmonad --restart")
    , ((myModMask, xK_p), spawn "synapse")
    , ((myModMask .|. shiftMask, xK_p), spawn "dmenu_run")
    , ((myModMask, xK_e), spawn myFileManager)
    -- , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle && amixer -q set PCM on")
    -- , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 10%-")
    -- , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 10%+")
    , ((0, xF86XK_AudioMute), toggleMute >> return ())
    , ((0, xF86XK_AudioLowerVolume), lowerVolume 3 >> return ())
    , ((0, xF86XK_AudioRaiseVolume), raiseVolume 3 >> return ())
    , ((0, xK_Print), spawn "~/.xmonad/bin/screenshot")
    , ((controlMask, xK_Print), spawn "~/.xmonad/bin/select-screenshot")
    , ((myModMask, xK_m), spawn "~/.xmonad/bin/music toggle")
    , ((myModMask, xK_g), spawn "terminator -x ~/.xmonad/bin/music gui")
    , ((myModMask .|. controlMask, xK_n), spawn "ncmpcpp next")
    , ((myModMask .|. controlMask, xK_p), spawn "ncmpcpp prev")
    ]


-- workspaces
chatWorkspace = "2"
vmWorkspace = "6"
myWorkspaces = [show i | i <- [1..9]]


-- you can continue to use the xmonad default by declaring the like so:
-- myLayoutHook = layoutHook defalutConfig
-- myManageHook = manageHook defaultConfig

-- manage hook
-- to know the className of an app, you can run the command "xprop"
myManage = composeAll $ [
           className =? "VirtualBox" --> doShift vmWorkspace
           , resource =? "skype" --> doFloat
           , isFullscreen --> (doF W.focusDown <+> doFullFloat)
           ]
           ++ [className =? i --> doIgnore | i <- ignoreList]
           ++ [className =? i --> doFloat | i <- floatList]
           ++ [className =? i --> doShift chatWorkspace | i <- chatWorkspaceList]
    where
      chatWorkspaceList = ["XChat", "Skype", "Pywebqq", "Pywebqq.py"]
      floatList = ["Gimp-2.8", "Skype", "Gpicview", "Launcher", "DDELauncher", "OCR"]
      ignoreList = ["Synapse", "Conky", "stalonetray"]

-- add manageDocks to my managehook
myManageHook = myManage <+> manageHook defaultConfig


-- layout hook
-- if you want start a window with xmobar shown, add avoidStruts like this:
-- myLayoutHook = avoidStrutsOn [] $ smartBorders $ tiled ||| Mirror tiled ||| Full
myLayoutHook = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| Full
    where
      -- add some space between windows
      tiled = spacing marginBetweenWindows $ ResizableTall nmaster delta ratio []
      -- default number of the master pane
      nmaster = 1
      -- default proportion of screen occupied by master pane
      ratio = 1 / 2
      -- percent of screen to increment by when resizing panes
      delta = 1 / 100


-- Loghook
myLogHook h = dynamicLogWithPP $ xmobarPP {
                ppOutput = hPutStrLn h
              , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
              , ppCurrent = xmobarColor myCurrentWSColor ""
                            . wrap myCurrentWSLeft myCurrentWSRight
              , ppVisible = xmobarColor myVisibleWSColor ""
                            . wrap myVisibleWSLeft myVisibleWSRight
              , ppUrgent = xmobarColor myUrgentWSColor ""
                           . wrap myUrgentWSLeft myUrgentWSRight
              , ppLayout = const ""  -- to disable the layout info on xmobar
              }


myConfig xmproc = defaultConfig {
               -- basic stuff
               terminal = myTerminal
             , modMask = myModMask
             , borderWidth = myBorderWidth
             , focusFollowsMouse = myFocusFollowsMouse
             , workspaces = myWorkspaces
             , normalBorderColor = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor

             -- hooks, layouts
             , layoutHook = myLayoutHook
             , manageHook = myManageHook
             , logHook = myLogHook xmproc
             , handleEventHook = myEventHook
             , startupHook = do
                 spawn "~/.xmonad/bin/startup-hook"
             } `additionalKeys` myKeyBindings

myStatusBar = "xmobar ~/.xmonad/xmobarrc"

main = do
  xmproc <- spawnPipe myStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ myConfig xmproc
