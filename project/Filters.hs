{- Module contains filters to appply to a particular image.

	This module is imports Stencil.hs for using the stencils described for
	adding various effects to the image

-}
{-# LANGUAGE QuasiQuotes #-}

module Filters
(
toBlacknWhite
,toSepia
,toGrayScale
,gaussBlur
,gaussianBlur
,passes
,edgeDetection
,emboss
,saturate
) where


import Data.Array.Repa (Array,delay, DIM1, DIM2,DIM3, U, D, Z (..), (:.)(..), (!))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Stencil.Dim2 
import qualified Data.Array.Repa.Stencil as A
import Language.Haskell.TH.Quote (QuasiQuoter)
import Data.Array.Repa.Slice
import System.Environment
import Stencils

-- User defined types used in functions which add filters
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

myif :: Bool -> a -> a -> a
myif True x _ = x
myif False _ y = y

getMaxPixel :: Float -> Float -> Float
getMaxPixel r g 
			| r<=g = g
			| otherwise = r 
getMinPixel :: Float -> Float -> Float
getMinPixel r g 
			| r>=g = g
			| otherwise = r
{-Function to get the luminance from the rgb values-}
getluminance :: Float -> Float -> Float -> Float
getluminance r g b = ((getMaxPixel r $ getMaxPixel g b) + (getMinPixel r $ getMinPixel g b)) / 2
{-Function to get the Saturation values from rgb values-}
getSaturation :: Float -> Float -> Float -> Float
getSaturation l maxColor minColor =  myif (l < 0.5) ((maxColor - minColor)/(maxColor + minColor)) ((maxColor - minColor)/ (2.0-(maxColor + minColor)))
{-Function to get the Hue values form rgb values-}
getHue :: Float -> Float -> Float -> Float -> Float -> Float
getHue maxColor minColor r g b = (myif (r==maxColor) ((g - b) / (maxColor - minColor)) $ myif (g==maxColor) (2.0 + (b - r) / (maxColor - minColor)) (4.0 + (r - g) / (maxColor - minColor))) / 6
{-Function to convert the RGB Pixel values to HSL values-}
convertRGBtoHSL :: PartImage -> PartImage
convertRGBtoHSL imgArr = R.traverse imgArr id toHSL
							where 
				  	 		  toHSL get (Z:.a:.b:.c) 
									  = case c of
									  		0 -> 360.0 * ((getHue (getMaxPixel (get (Z:.a:.b:.0)) $ getMaxPixel (get (Z:.a:.b:.1)) (get(Z:.a:.b:.2))) (getMinPixel (get (Z:.a:.b:.0)) $ getMinPixel (get (Z:.a:.b:.1)) (get(Z:.a:.b:.2))) (get (Z:.a:.b:.0)) (get (Z:.a:.b:.1)) (get(Z:.a:.b:.2)) / 6))
									  		1 -> 255.0 * (getSaturation (getluminance (get (Z:.a:.b:.0)) (get (Z:.a:.b:.1)) (get(Z:.a:.b:.2))) (getMaxPixel (get (Z:.a:.b:.0)) $ getMaxPixel (get (Z:.a:.b:.1)) (get(Z:.a:.b:.2))) (getMinPixel (get (Z:.a:.b:.0)) $ getMinPixel (get (Z:.a:.b:.1)) (get(Z:.a:.b:.2))))  
									  		2 -> 255.0 * (getluminance (get (Z:.a:.b:.0)) (get (Z:.a:.b:.1)) (get(Z:.a:.b:.2)))
									
{-Function to get the temp2 value from luminance and saturation of the image-}
getTemp2 :: Float -> Float -> Float
getTemp2 l s = myif (l < 0.5) (l*(1+s)) ((l+s) - (l*s))
{-Function to get the temp1 value from luminance and saturation-}
getTemp1 :: Float -> Float -> Float
getTemp1 l s =  2 * l - getTemp2 l s
{-Function to get tempr value from the hue value of the image-}
getTempr :: Float -> Float 
getTempr h  = myif ((h + 1.0 / 3.0) > 1) ((h + 1.0 / 3.0)-1) (h + 1.0 / 3.0) 
{-Function to get tempg value from the hue value of the image-}
getTempg :: Float -> Float
getTempg h = h
{-Function to get the tempb value from the hue value of the image-}
getTempb :: Float -> Float
getTempb h = myif ((h - 1.0 / 3.0) < 0) ((h - 1.0 / 3.0)+1) (h - 1.0 / 3.0) 

{-Function to convert the HSL values to RGB values-}



convertHSLtoRGB :: PartImage -> PartImage
convertHSLtoRGB hslArr = R.traverse hslArr id toRGB
							where
								toRGB get (Z:.a:.b:.c)
										= case c of
											0 -> if tempr  < (1.0/6.0) then temp1 + (temp2 - temp1) * 6.0 * tempr
												else 
													if tempr < 0.5 then temp2
													else 
														if tempr < (2.0/3.0) then temp1 + (temp2 - temp1) * ((2.0/3.0) - tempr) * 6.0
														else temp1
											1 -> if tempg < (1.0/6.0) then temp1 + (temp2 - temp1) * 6.0 * tempg
												 else 
												 	if tempg < 0.5 then temp2
												    else 
												    	if tempg < (2.0/3.0) then temp1 + (temp2 - temp1) * ((2.0/3.0) - tempg) * 6.0
														else temp1
											2 -> if tempb < (1.0/6.0) then temp1 + (temp2 - temp1) * 6.0 * tempb
												 else 
												 	if tempb < 0.5 then temp2
												    else 
												    	if tempb < (2.0/3.0) then temp1 + (temp2 - temp1) * ((2.0/3.0) - tempb) * 6.0
														else temp1
											
											where 
												 temp1 = getTemp1 ((get (Z:.a:.b:.2))/256.0) ((get (Z:.a:.b:.1))/256.0)
												 temp2 = getTemp2 ((get (Z:.a:.b:.2))/256.0) ((get (Z:.a:.b:.1))/256.0)
												 tempr = getTempr (((get (Z:.a:.b:.0)))/360.0)  
												 tempg = getTempg (((get (Z:.a:.b:.0)))/360.0)  
												 tempb = getTempb (((get (Z:.a:.b:.0)))/360.0)  
												 


{-Function to saturate the image by converting from RGB to HSL and then back to RGB-}
saturate :: PartImage -> Float -> PartImage
saturate imgArr level = convertHSLtoRGB $ R.traverse (convertRGBtoHSL imgArr) id saturateIt
							where
								saturateIt get (Z:.a:.b:.c)
										= case c of 
											0 -> (get (Z:.a:.b:.c)) 
											1 -> (get (Z:.a:.b:.c)) * level 
											2 -> (get (Z:.a:.b:.c))


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

{-Function to get the channels from the image Array-}
getChannel :: Int-> PartImage -> Channel
getChannel index imgArr = R.traverse imgArr (\(Z:.a:.b:._) -> (Z:.a:.b)) (\f (Z:.a:.b) -> (f (Z:.a:.b:.index)) * 255.0)

{-Function for edge detection filter-}
edgeDetection :: Filter
edgeDetection imgArr = R.fromFunction (Z:.length:.breadth:.3) formArray	
							where 
								_:.length:.breadth:._ = R.extent imgArr

								t1 = applyStencil laplaceStencil $ (getChannel 0 imgArr)
								t2 = applyStencil laplaceStencil $ (getChannel 1 imgArr)
								t3 = applyStencil laplaceStencil $ (getChannel 2 imgArr)

								formArray (Z:.a:.b:.c) = case c of
														0 -> (t1 ! (Z:.a:.b)) / 255.0 
														1 -> (t2 ! (Z:.a:.b)) / 255.0
														2 -> (t3 ! (Z:.a:.b)) / 255.0

{-Fuction to implement gaussian blur using 5X5 guassian stencil-}
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

{-Function to add emboss filter using emboss stencil-}
emboss ::Filter
emboss imgArr = R.fromFunction (Z:.length:.breadth:.3) formArray
					where
						_:.length:.breadth:._ = R.extent imgArr
						formArray (Z:.a:.b:.c) = case c of
												0 -> (t1 ! (Z:.a:.b)) / 255.0
												1 -> (t2 ! (Z:.a:.b)) / 255.0
												2 -> (t3 ! (Z:.a:.b)) / 255.0
								where
									t1 = applyStencil embossStencil $ (getChannel 0 imgArr)
									t2 = applyStencil embossStencil $ (getChannel 1 imgArr)
									t3 = applyStencil embossStencil $ (getChannel 2 imgArr)
							
{-Function for gauss blur without stencil operation-}
gaussBlur:: Filter
gaussBlur imgArr = R.traverse imgArr id blurImage
				where 
					_ :.length :. breadth :. depth = R.extent imgArr

					blurImage get (Z:. a:.b :.c)
							= if (isBoundary length breadth (Z:.a:.b:.c) )then get (Z:.a:.b:.c)
							  else (get(Z:.a-1:.b:.c) + get(Z:.a+1:.b:.c) + get(Z:.a-1:.b-1:.c)+get(Z:.a+1:.b+1:.c) 
									+ get(Z:.a:.b-1:.c) + get(Z:.a:.b+1:.c) + get(Z:.a-1:.b+1:.c) + get (Z:.a+1:.b-1:.c)) /8.0



{-Function for passing the image throught the same filter multiple number of times-}
passes :: Int -> PartImage -> Filter -> PartImage
passes 1 imgArr filter = filter imgArr 
passes n imgArr filter = passes (n-1) (filter imgArr) filter

{-Function to normalize the pixel values-}
normalize :: Float -> Channel -> Channel
normalize n  = R.map (/n) 
