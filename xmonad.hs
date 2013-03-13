import XMonad
import Graphics.X11.ExtraTypes.XF86  -- for the names of laptop's function keys e.g. XF86MonBrightnessUp
import XMonad.Layout.Spacing  -- add some space between windows
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders  -- for smart Borders
--import XMonad.Layout.FullScreen
import XMonad.Layout.LayoutHints
import XMonad.Layout.SimpleFloat
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops  -- for fullscreenEventHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import System.IO
import XMonad.Hooks.UrgencyHook


-- keybindings, `((modkey, key), action)`
-- use `.|.` to combinate modify keys
-- if modkey is 0, it means we need't hold any key
-- use `xev` to find the name of every keyboard button
myKeyBindings =
    [((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_a), sendMessage MirrorShrink)
    , ((myModMask, xK_z), sendMessage MirrorExpand)
    , ((myModMask, xK_c), spawn "xmonad --recompile")
    , ((myModMask .|. shiftMask, xK_p), spawn "synapse")
    , ((myModMask, xK_p), spawn "~/.cabal/bin/yeganesh -x")
    , ((myModMask, xK_f), spawn "thunar")
    ]


-- basic stuff
myModMask = mod4Mask  -- Win key or Super_L
myTerminal = "terminator"
myBorderWidth = 2
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#ff0000"
myFocusFollowsMouse = False
myEventHook = fullscreenEventHook  -- for some apps like chrome which has a problem with fullscreen
marginBetweenWindows = 2  -- add marginBetweenWindows pixels space between windows

myTitleColor = "#eeeeee"
myTitleLength = 80
myCurrentWSColor = "#e6744c"
myCurrentWSLeft = "["
myCurrentWSRight = "]"
myVisibleWSColor = "#e6744c"
myVisibleWSLeft = "["
myVisibleWSRight = "]"
myUrgentWSColor = "#e6744"
myUrgentWSLeft = "{"
myUrgentWSRight = "}"


-- workspaces
chatWorkspace = "2:Chat"
vmWorkspace = "6:VM"
myWorkspaces = ["1:Main", chatWorkspace, "3:Coding", "4:Docs",
                "5:Media", vmWorkspace, "7:Undefined", "8:Undefined"]


-- you can continue to use the xmonad default by declaring the like so:
-- myLayoutHook = layoutHook defalutConfig
-- myManageHook = manageHook defaultConfig

-- manage hook
-- to know the className of an app, you can run the command "xprop"
myManage = composeAll [
            -- shift list
            className =? "XChat" --> doShift chatWorkspace
           , className =? "Skype" --> doShift chatWorkspace
           , className =? "Pywebqq" --> doShift chatWorkspace
           , className =? "VirtualBox" --> doShift vmWorkspace

           -- float list
           , isFullscreen --> (doF W.focusDown <+> doFullFloat)

           -- ignore list
           , className =? "gimp" --> doIgnore
           , resource =? "synapse" --> doIgnore
           , resource =? "stalonetray" --> doIgnore
           ]

-- add manageDocks to my managehook
myManageHook = myManage <+> manageHook defaultConfig


-- layout hook
myLayoutHook = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| noBorders Full
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
-- myLogHook h = dynamicLogWithPP $ defaultPP
--               -- display current workspace as darkgrey on light grey (oppsite of
--               -- default colors)
--               {
--                 ppCurrent = dzenColor "#303030" "#909090" . pad
--                 -- display other workspaces which contain windows as a brighter grey
--               , ppHidden = dzenColor "#909090" "" . pad
--                 -- display other workspaces with no windows as a normal grey
--               , ppHiddenNoWindows = dzenColor "#606060" "" . pad
--                 -- display the current layout as a brighter grey
--               , ppLayout = dzenColor "#909090" "" . pad
--                 -- if a window on a hidden workspace needs my attention, color it so
--               , ppUrgent = dzenColor "#ff0000" "" . pad . dzenStrip
--                 -- shorten if it goes over 100 characters
--               , ppTitle = shorten 100
--                 -- no separator between workspaces
--               , ppWsSep = ""
--                 -- put a few spaces between each object
--               , ppSep = "  "
--                 -- output to the handle we were given as an argument
--               , ppOutput = hPutStrLn h
--               }
myLogHook h = dynamicLogWithPP $ xmobarPP {
                ppOutput = hPutStrLn h
              , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
              -- , ppCurrent = xmobarColor myCurrentWSColor "" . wrap myCurrentWSLeft myCurrentWSRight
              -- , ppVisible = xmobarColor myVisibleWSColor "" . wrap myVisibleWSLeft myVisibleWSRight
              , ppUrgent = xmobarColor myUrgentWSColor "" . wrap myUrgentWSLeft myUrgentWSRight
              , ppLayout = const ""  -- to disable the layout info on xmobar
              }


myStatusBar = "xmobar ~/.xmonad/xmobarrc"

main = do
  xmproc <- spawnPipe myStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
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
                 spawn "~/.xmonad/bin/start-xmonad"
                 spawn "~/.xmonad/bin/startup-hook"
             } `additionalKeys` myKeyBindings
