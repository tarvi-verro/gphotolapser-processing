module Process (groupRuns, getRunCycleLength, getRunAvgGroups, getBlendCommands, getSmoothCommands) where

import Luminance
import Image
import Data.Function (on)
import Data.List (groupBy, concatMap)
import Data.Fixed (mod')
import Text.Printf (printf)


groupRuns :: [ImageInfo] -> [[ImageInfo]]
groupRuns = groupBy ((==) `on` cycleRefTime)

getRunCycleLength :: [ImageInfo] -> Double
getRunCycleLength = df . map head . groupBy ((==) `on` luminanceTarget) where
    df (ImageInfo {triggerStartTime=a} : ImageInfo {triggerStartTime=b} : _) = b - a
    df x = error $ show x

getRunAvgGroups run = groupBy ((<) `on` relStartTime) run where
    relStartTime x = (triggerStartTime x - cycleRefTime x) `mod'` rcl
    rcl = getRunCycleLength run

lumiOffsetsCalculate :: [(Int, ImageInfo)] -> [(Int, Double, ImageInfo)]
lumiOffsetsCalculate ps = map loc [0 .. ((length ps) - 1)] where
    loc n = (fst $ ps !! n, offsetFromAverage n, snd $ ps !! n)
    offsetFromAverage n = (ev (snd $ (ps !! n))) - (surroundingAverage n)
    surroundingAverage n = avg $ map (ev . snd) $ take (2*d+1) $ drop (max 0 (n-d)) ps
    ev = (\x -> log x / log 2) . luminanceCalculate
    avg xs = sum xs / fromIntegral (length xs)
    d = 3

getBlendCommands :: [[ImageInfo]] -> [[String]]
getBlendCommands = map cmd . zip [1..] . concatMap getRunAvgGroups where
    cmd (n,[i]) = ["ln", "-s", "../" ++ fileName i, avgp n]
    cmd (n,is) = ["convert"] ++ (map fileName is) ++ ["-evaluate-sequence", "mean", avgp n]
    avgp n = "out-blended/" ++ prnt n ++ ".JPG"
    prnt = printf "%04d" :: Int -> String

getSmoothCommands :: [ImageInfo] -> [[String]]
getSmoothCommands = map geglcmd . lumiOffsetsCalculate . zip [1..] where
    geglcmd (n, o, i) = ["gegl", fileName i, "--", "gegl:exposure", "exposure=" ++ show o, "gegl:jpg-save", "quality=98", "path=" ++ outn n]
    outn n = "out-smoothed/" ++ prnt n ++ ".JPG"
    prnt = printf "%04d" :: Int -> String

