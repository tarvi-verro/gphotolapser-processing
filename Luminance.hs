module Luminance (luminanceCalculate, luminanceOffset) where

import Data.Function (on)
import Image

luminanceCalculate :: ImageInfo -> Double
luminanceCalculate ImageInfo{fNumber = n, isoSpeed = s, exposureTime = t} = let k=12.5 in n**2 * k / (t * s)

luminanceOffset :: ImageInfo -> Maybe Double
luminanceOffset ii@ImageInfo{gphInfo = Just GPHInfo{luminanceTarget = lt}} = Just $ ((-) `on` logBase 2) lt (luminanceCalculate ii)
luminanceOffset _ = Nothing

