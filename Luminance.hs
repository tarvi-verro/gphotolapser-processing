module Luminance (luminanceCalculate, luminanceOffset) where

import Data.Function (on)
import Image

luminanceCalculate :: ImageInfo -> Double
luminanceCalculate (ImageInfo {fNumber = n, isoSpeed = s, exposureTime = t}) = let k=12.5 in n**2 * k / (t * s)

luminanceOffset :: ImageInfo -> Double
luminanceOffset ii = ((-) `on` (\x -> log x / log 2)) (luminanceTarget ii) (luminanceCalculate ii)

