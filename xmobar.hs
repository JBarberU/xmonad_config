module Main (main) where

import Xmobar
import Network.HostName
import MyColors

baseCommands :: [Runnable]
baseCommands = [
                  Run $ Cpu ["-t", "<fn=1>\xf2db</fn> <total>%", "-L","5","-H","50","--low",myLightBlue,"--high",myLightRed] 20
                , Run $ Date "%Y-%m-%d %H:%M %A"  "date" 50
                , Run $ CoreTemp ["-t", "<fn=1>\xf2c9</fn> <core0>°C",
                                "-L", "40",
                                "-H", "70",
                                "-l", myLightBlue,
                                "-h", myLightRed
                              ] 50
                , Run $ Memory         [ "--template" ,"<fn=1>\xf538</fn> <usedratio>%"
                                     , "--Low"      , "30"        -- units: %
                                     , "--High"     , "70"        -- units: %
                                     , "--low"      , myLightGreen
                                     , "--high"     , myLightRed
                                     ] 50
                , Run $ Com "cmus-status" ["status-icon"] "cmus" 1
                , Run $ Com "wh1000" [
                    "--status",
                    "--color-connected", myLightGreen,
                    "--color-disconnected", myLightGrey
                ] "wh1000" 1
              , Run $ StdinReader
            ]

baseConfig :: Config
baseConfig =
  defaultConfig
    {
      --font = "xft:DejaVu Sans Mono:pixelsize=14:antialias=true:hinting=true"
      font = "DejaVu Sans Mono"
      --, additionalFonts = [
        --"xft:Font Awesome 6 Free Solid:pixelsize=12",
        --"xft:Font Awesome 6 Brands:pixelsize=12"
      --]
      , alpha = 200
      , border = FullB
      , borderWidth = 1
      , borderColor = myMediumBeige
      , bgColor = myVeryDarkGrey
      , fgColor = myLightBeige
      , position = TopW C 100
      , sepChar = "%"
      , alignSep = "}{"
    }

laptopConfig :: String -> Config
laptopConfig adapterName = 
  baseConfig
    {
      commands = baseCommands ++ [
          Run $ Wireless adapterName [ "-t", "<fn=1>\xf1eb</fn> <ssid> <quality>%"
                                  , "--Low", "50", "--low", myLightRed
                                  , "--High", "90", "--high", myLightGreen
                                  ] 50
          , Run $ Battery [
              "-t", "<acstatus> <left>% (<timeleft>)",
              "--",
              --"-c", "charge_full",
              "-O", "<fc=#fabd2f><fn=1>\xf1e6</fn></fc>",
              "-o", "<fc="++ myLightGreen ++ "><fn=1>\xf242</fn></fc>",
              "-i", "<fc="++ myLightGreen ++ "><fn=1>\xf1e6</fn></fc>",
              "-h", myLightGreen,
              "-l", myLightRed
          ] 50
      ]
      , template = "%StdinReader% }{ %cmus% | %wh1000% | %" ++ adapterName ++ "% | %battery% | %coretemp% | %cpu% | %memory% | <fc=" ++ myLightPurple ++ ">%date%</fc> "
    }

desktopConfig :: Config
desktopConfig = 
  baseConfig
    {
      commands = baseCommands ++ [
      ]
      , template = "%StdinReader% }{ %cmus% | %wh1000% | %coretemp% | %cpu% | %memory% | <fc=" ++ myLightPurple ++ ">%date%</fc> "
    }


desktopConfigWifi :: String -> Config
desktopConfigWifi adapterName = 
  baseConfig
    {
      commands = baseCommands ++ [
          Run $ Wireless adapterName [ "-t", "<fn=1>\xf1eb</fn> <ssid> <quality>%"
                                  , "--Low", "50", "--low", myLightRed
                                  , "--High", "90", "--high", myLightGreen
                                  ] 50
      ]
      , template = "%StdinReader% }{ %cmus% | %" ++ adapterName ++ "% | %coretemp% | %cpu% | %memory% | <fc=" ++ myLightPurple ++ ">%date%</fc> "
    }

config :: String -> Config
config "Fenrisulven" = desktopConfig
config "Frej" = desktopConfig
config "Loke" = laptopConfig "wlp3s0"
config _ = laptopConfig "wlp3s0"

main :: IO ()
main = getHostName >>= \hn -> xmobar $ config hn
