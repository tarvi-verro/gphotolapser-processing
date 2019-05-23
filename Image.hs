module Image (ImageInfo (ImageInfo, fileName, luminanceTarget, cycleRefTime, triggerStartTime, fNumber, isoSpeed, exposureTime), getImageInfo, imgInfos) where

import Data.Function (on)
import System.Process (readProcess)
import Data.List (isSuffixOf, isPrefixOf, break)
import Data.Maybe (catMaybes)

data ImageInfo = ImageInfo { fileName :: String
                           , luminanceTarget :: Double
                           , cycleRefTime :: Double
                           , triggerStartTime :: Double
                           , fNumber :: Double
                           , isoSpeed :: Double
                           , exposureTime :: Double
                           } deriving (Show)

imgListing :: String -> [String]
imgListing = filter (isSuffixOf ".JPG") . lines

readExiv2 :: String -> IO (String, String)
readExiv2 fs = (\rs -> (fs,rs)) <$> readProcess "exiv2" ["-p", "a", "-g", regx, fs] [] where
    regx = "^Xmp\\.xmp\\.GPhotolapser\\.\\|^Exif\\.Photo\\."

parseExiv2 :: (String, String) -> Maybe ImageInfo
parseExiv2 (fs, es) = toData <$> unwrap where
    toData (lumi, reft, tst, fnr, iso, et) = ImageInfo {
            fileName = fs, luminanceTarget = lumi, cycleRefTime = reft, triggerStartTime = tst,
            fNumber = fnr, isoSpeed = iso, exposureTime = et }
    unwrap = case map getVal ids of
            [Just a, Just b, Just c, Just bht, Just bl, Just d, Just e, Just f] -> Just (a,b,c,d,e,bht - bl)
            [Just a, Just b, Just c, Nothing, Nothing, Just d, Just e, Just f] -> Just (a,b,c,d,e,f)
            otherwise -> Nothing
    ids = [
           "Xmp.xmp.GPhotolapser.LuminanceTarget", "Xmp.xmp.GPhotolapser.CycleRefTime", "Xmp.xmp.GPhotolapser.TriggerStartTime",
           "Xmp.xmp.GPhotolapser.BulbHoldTime", "Xmp.xmp.GPhotolapser.BulbLag",
           "Exif.Photo.FNumber", "Exif.Photo.ISOSpeedRatings", "Exif.Photo.ExposureTime"
          ]
    getVal id = case filter ((==) id . head) ys of
            [[_,_,_,'F' : val]] -> Just $ (read :: String -> Double) $ val
            [[_,_,_,val]] -> Just $ (read :: String -> Double) $ val
            [[_,_,_,val, "s"]] -> Just $ parseValFraction $ val
            otherwise -> Nothing
    ys = map words $ lines es

parseValFraction v = case break (=='/') v of
                     (ns, '/' : ds) -> ((/) `on` read) ns ds
                     (ns, "") -> read ns

ls = readProcess "ls" [] []

getImageInfo :: String -> IO (Maybe ImageInfo)
getImageInfo = fmap parseExiv2 . readExiv2

imgInfos :: IO [ImageInfo]
imgInfos = imgListing <$> ls >>= fmap catMaybes . mapM getImageInfo

