{- Module contains filters to appply to a particular image.

	This module is imported by main.hs file

-}
module Filters
(
toBlacknWhite
,toSepia
,toGrayScale




) where


import Data.Array.Repa (Array, DIM1, DIM2,DIM3, U, D, Z (..), (:.)(..), (!))
import qualified Data.Array.Repa as R

{- Filter to convert the image to black and white accoring to the RGB pixel average-}
toBlacknWhite :: Array D DIM3 Float -> Array D DIM3 Float
toBlacknWhite arr = R.traverse arr id bnwTransform
					where 
						bnwTransform get (Z:. a :. b :. _)
								= let avg = ((get (Z:.a:.b:.0)) + (get (Z:.a:.b:.1)) + (get (Z:.a:.b:.2))) / 3.0 in
									if avg*255 < 128 then 0
									else 1.0

{- Filter to convert the image to grayscale using the RGB values stored in REPA array -}
toGrayScale :: Array D DIM3 Float -> Array D DIM3 Float
toGrayScale arr = R.traverse arr id grayScaleTransform 
				where
					grayScaleTransform get (Z:. a:.b:. _)
								= ((get (Z:.a:.b:.0)) + (get (Z:.a:.b:.1)) + (get (Z:.a:.b:.2))) / 3.0


{- Filter to add Sepia tone to the image using the RGB pixel values stored in REPA array -}
toSepia:: Array D DIM3 Float -> Array D DIM3 Float
toSepia arr = R.traverse arr id sepiaTransform 
			where 
				sepiaTransform get (Z :. a :.b :.c)
					= case c of 
							0 ->let tr= 0.393* (get (Z:. a:.b:.c)) + 0.769* (get (Z:. a:.b:.c)) + 0.189* (get(Z:. a:.b:.c)) in
							 		if tr >= 1.0 then 1
							 		else tr
							1 ->let tg = 0.349* (get(Z:. a:.b:.c)) + 0.686* (get(Z:. a:.b:.c)) + 0.168* (get(Z:. a:.b:.c)) in
									if tg >=1.0 then 1
									else tg													
							2 ->let tb = 0.272* (get(Z:. a:.b:.c)) + 0.534* (get(Z:. a:.b:.c)) + 0.131* (get(Z:. a:.b:.c)) in
									if tb >=1.0 then 1
									else tb	

