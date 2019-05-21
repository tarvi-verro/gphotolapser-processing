module Image (ImageInfo (ImageInfo, fileName, luminanceTarget, cycleRefTime, triggerStartTime), imgInfos) where

import System.Process (readProcess)
import Data.List (isSuffixOf, isPrefixOf)
import Data.Maybe (catMaybes)

data ImageInfo = ImageInfo { fileName :: String
                           , luminanceTarget :: Double
                           , cycleRefTime :: Double
                           , triggerStartTime :: Double 
                           } deriving (Show)

imgListing :: String -> [String]
imgListing = filter (isPrefixOf "MG_" . tail) . filter (isSuffixOf ".JPG") . lines

readExiv2 :: String -> IO (String, String)
readExiv2 fs = (\rs -> (fs,rs)) <$> readProcess "exiv2" ["-p", "a", "-g", "^Xmp\\.xmp\\.GPhotolapser\\.", fs] []

parseExiv2 :: (String, String) -> Maybe ImageInfo
parseExiv2 (fs, es) = toData <$> unwrap where
    toData (lumi, reft, tst) = ImageInfo { fileName = fs, luminanceTarget = lumi, cycleRefTime = reft, triggerStartTime = tst }
    unwrap = case map getVal ids of
            [Just a, Just b, Just c] -> Just (a,b,c)
            otherwise -> Nothing
    ids = ["Xmp.xmp.GPhotolapser.LuminanceTarget", "Xmp.xmp.GPhotolapser.CycleRefTime", "Xmp.xmp.GPhotolapser.TriggerStartTime"]
    getVal id = case filter ((==) id . head) ys of
            [[_,_,_,val]] -> Just $ (read :: String -> Double) $ val
            otherwise -> Nothing
    ys = map words $ lines es

ls = readProcess "ls" [] []

imgInfos :: IO [ImageInfo]
imgInfos = imgListing <$> ls >>= fmap catMaybes . mapM (fmap parseExiv2 . readExiv2)

