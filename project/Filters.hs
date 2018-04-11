{- Module contains filters to appply to a particular image.

	This module is imported by main.hs file

-}
{-# LANGUAGE QuasiQuotes #-}

module Filters
(
toBlacknWhite
,toSepia
,toGrayScale
,gausBlur
,gaussianBlur
,passes
,edgeDetection
) where


import Data.Array.Repa (Array,delay, DIM1, DIM2,DIM3, U, D, Z (..), (:.)(..), (!))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Stencil.Dim2 
import qualified Data.Array.Repa.Stencil as A
import Language.Haskell.TH.Quote (QuasiQuoter)
import Data.Array.Repa.Slice
import System.Environment

type PartImage = Array D DIM3 Float
type Filter = PartImage -> PartImage
type Channel = Array D DIM2 Float


{- Filter to convert the image to black and white accoring to the RGB pixel average-}
toBlacknWhite :: Filter
toBlacknWhite arr = R.traverse arr id bnwTransform
					where 
						bnwTransform get (Z:. a :. b :. _)
								= let avg = ((get (Z:.a:.b:.0)) + (get (Z:.a:.b:.1)) + (get (Z:.a:.b:.2))) / 3.0 in
									if avg*255 < 128 then 0
									else 1.0

{- Filter to convert the image to grayscale using the RGB values stored in REPA array -}
toGrayScale :: Filter
toGrayScale arr = R.traverse arr id grayScaleTransform 
				where
					grayScaleTransform get (Z:. a:.b:. _)
								= ((get (Z:.a:.b:.0))*0.21 + (get (Z:.a:.b:.1))*0.71 + (get (Z:.a:.b:.2))*0.07)


{- Filter to add Sepia tone to the image using the RGB pixel values stored in REPA array -}
toSepia:: Filter
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

isBoundary:: Int -> Int -> DIM3 -> Bool
isBoundary length breadth (Z:.a:.b:.c) = (a>= length-1 || a==0) || (b>=breadth-1 || b==0)


normalize :: Float -> Channel -> Channel
normalize n  = R.map (/n) 




gaussianBlur :: Filter
gaussianBlur imgArr = R.fromFunction (Z:.length:.breadth:.3) formArray
							where 
								_:.length:.breadth:._ = R.extent imgArr

								t1= applyStencil gaussStencil $ normalize (159) $ (getChannel 0 imgArr)
								t2= applyStencil gaussStencil $ normalize (159) $ (getChannel 1 imgArr)
								t3= applyStencil gaussStencil $ normalize (159) $ (getChannel 2 imgArr)

								formArray (Z:.a:.b:.c) = case c of 
														0 -> (t1 ! (Z:.a:.b)) / 255.0 
														1 -> (t2 ! (Z:.a:.b)) / 255.0
														2 -> (t3 ! (Z:.a:.b)) / 255.0



passes :: Int -> PartImage -> Filter -> PartImage
passes 1 imgArr filter = filter imgArr 
passes n imgArr filter = passes (n-1) (filter imgArr) filter

gausBlur:: Filter
gausBlur imgArr = R.traverse imgArr id blurImage
				where 
					_ :.length :. breadth :. depth = R.extent imgArr

					blurImage get (Z:. a:.b :.c)
							= if (isBoundary length breadth (Z:.a:.b:.c) )then get (Z:.a:.b:.c)
							  else (get(Z:.a-1:.b:.c) + get(Z:.a+1:.b:.c) + get(Z:.a-1:.b-1:.c)+get(Z:.a+1:.b+1:.c) 
									+ get(Z:.a:.b-1:.c) + get(Z:.a:.b+1:.c) + get(Z:.a-1:.b+1:.c) + get (Z:.a+1:.b-1:.c)) /8.0






