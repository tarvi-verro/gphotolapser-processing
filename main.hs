import Process (getOutCommands, groupRuns)
import Image (imgInfos)

main :: IO ()
main = do
    runs <- groupRuns <$> imgInfos
    putStr $ unlines $ map unwords $ getOutCommands runs

