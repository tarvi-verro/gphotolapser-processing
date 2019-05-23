import System.Environment (getArgs, getProgName)
import Process (getBlendCommands, getSmoothCommands, groupRuns)
import Image (imgInfos)

action :: [String] -> IO ()
action ("blend" : []) = do
    runs <- groupRuns <$> imgInfos
    putStrLn "mkdir -p out-blended"
    putStr $ unlines $ map unwords $ getBlendCommands runs

action ("smooth" : []) = do
    infs <- imgInfos
    putStrLn "mkdir -p out-smoothed"
    putStr $ unlines $ map unwords $ getSmoothCommands infs

action _ = do
    n <- getProgName
    putStrLn $ "Usage: " ++ n ++ " [blend | smooth]"

main :: IO ()
main = do
    getArgs >>= action


