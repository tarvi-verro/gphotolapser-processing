module Process (groupRuns, getRunCycleLength, getRunAvgGroups, getOutCommands) where

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

getRunAvgGroups run = map (map fileName) $ groupBy ((<) `on` relStartTime) run where 
    relStartTime x = (triggerStartTime x - cycleRefTime x) `mod'` rcl
    rcl = getRunCycleLength run

getOutCommands :: [[ImageInfo]] -> [[String]]
getOutCommands = map cmd . zip [1..] . concatMap getRunAvgGroups where
    cmd (n,[f]) = ["ln", "-s", "../" ++ f, "out/" ++ prnt n ++ ".JPG"]
    cmd (n,fs) = ["convert"] ++ fs ++ ["-evaluate-sequence", "mean", "out/" ++ prnt n ++ ".JPG"]
    prnt = printf "%04d" :: Int -> String

