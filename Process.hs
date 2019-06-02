module Process (groupRuns, getRunCycleLength, getRunAvgGroups, getBlendCommands, getSmoothCommands) where

import Luminance
import Image
import Data.Function (on)
import Data.List (groupBy, concatMap)
import Data.Fixed (mod')
import Text.Printf (printf)
import Data.Maybe (mapMaybe)


groupRuns :: [ImageInfo] -> [[ImageInfo]]
groupRuns = map (map snd) . groupBy ((==) `on` (cycleRefTime . fst)) . getGPHs

getRunCycleLength :: [GPHInfo] -> Double
getRunCycleLength = df . map head . groupBy ((==) `on` luminanceTarget) where
    df (GPHInfo {triggerStartTime=a} : GPHInfo {triggerStartTime=b} : _) = b - a
    df x = error $ show x

getGPHs :: [ImageInfo] -> [(GPHInfo, ImageInfo)]
getGPHs = mapMaybe (\ii -> (\g -> (g,ii)) <$> gphInfo ii)

getRunAvgGroups :: [ImageInfo] -> [[ImageInfo]]
getRunAvgGroups run = map (map  snd) $ groupBy ((<) `on` (relStartTime . fst)) gphs where
    relStartTime x = (triggerStartTime x - cycleRefTime x) `mod'` rcl
    gphs = getGPHs run
    rcl = getRunCycleLength $ map fst gphs

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

