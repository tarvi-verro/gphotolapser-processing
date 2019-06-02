module Image( ImageInfo(ImageInfo, fileName, fNumber, isoSpeed, exposureTime, gphInfo)
            , GPHInfo (GPHInfo, luminanceTarget, cycleRefTime, triggerStartTime, bulbInfo)
            , BulbInfo (BulbInfo, bulbHoldTime, bulbLag)
            , getImageInfo, imgInfos
            ) where

import Data.Function (on)
import System.Process (readProcess)
import Data.List (isSuffixOf, isPrefixOf, break)
import Data.Maybe (catMaybes)

data BulbInfo = BulbInfo { bulbHoldTime :: Double
                         , bulbLag :: Double
                         } deriving (Show)

data GPHInfo = GPHInfo { luminanceTarget :: Double
                       , cycleRefTime :: Double
                       , triggerStartTime :: Double
                       , bulbInfo :: Maybe BulbInfo
                       } deriving (Show)

data ImageInfo = ImageInfo { fileName :: String
                           , fNumber :: Double
                           , isoSpeed :: Double
                           , exposureTime :: Double
                           , gphInfo :: Maybe GPHInfo
                           } deriving (Show)

imgListing :: String -> [String]
imgListing = filter (isSuffixOf ".JPG") . lines

readExiv2 :: String -> IO (String, String)
readExiv2 fs = (\rs -> (fs,rs)) <$> readProcess "exiv2" ["-p", "a", "-g", regx, fs] [] where
    regx = "^Xmp\\.xmp\\.GPhotolapser\\.\\|^Exif\\.Photo\\."

fixExposureTime :: ImageInfo -> ImageInfo
fixExposureTime ii@ImageInfo{gphInfo = Just GPHInfo{bulbInfo = Just BulbInfo{bulbHoldTime=t,bulbLag=lag}}}
                = ii {exposureTime = t - lag}
fixExposureTime ii = ii

parseExiv2 :: (String, String) -> Maybe ImageInfo
parseExiv2 (fs, es) = unwrap $ map getVal ids where
    unwrap (Just a : Just b : Just c : gphs)
             = Just ImageInfo{fileName = fs, fNumber = a, isoSpeed = b, exposureTime = c, gphInfo = unwrapGPH gphs}
    unwrap _ = Nothing
    unwrapGPH (Just a : Just b : Just c : bulbs)
                = Just GPHInfo{luminanceTarget = a, cycleRefTime = b, triggerStartTime = c, bulbInfo = unwrapBulb bulbs}
    unwrapGPH _ = Nothing
    unwrapBulb [Just a, Just b]
                 = Just BulbInfo{bulbHoldTime = a, bulbLag = b}
    unwrapBulb _ = Nothing
    ids = [
           "Exif.Photo.FNumber", "Exif.Photo.ISOSpeedRatings", "Exif.Photo.ExposureTime",
           "Xmp.xmp.GPhotolapser.LuminanceTarget", "Xmp.xmp.GPhotolapser.CycleRefTime", "Xmp.xmp.GPhotolapser.TriggerStartTime",
           "Xmp.xmp.GPhotolapser.BulbHoldTime", "Xmp.xmp.GPhotolapser.BulbLag"
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
getImageInfo = fmap (fmap fixExposureTime . parseExiv2) . readExiv2

imgInfos :: IO [ImageInfo]
imgInfos = imgListing <$> ls >>= fmap catMaybes . mapM getImageInfo

