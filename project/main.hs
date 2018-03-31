import Data.List
import System.IO
import Codec.Picture
import Control.Monad
import Data.Word
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2,DIM3, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R
import qualified Filters as F -- for Repa

{- The image obtained from readImage using JuicyPixel module is a Dyanmic Image on converting the image to type
RGBA8 it becomes Image PixelRGBA8 type

The Alpha channel is used to test the opacity of the pixels

If RGB8 value is [123,12,0] then RGBF values are [123/255.0,12/255.0,0255.0]
-}

getImageArr :: Image PixelRGBF-> IO(Array D DIM3 Float)
getImageArr img = do
		return $ R.fromFunction
			(Z :. imageWidth img :. imageHeight img :. 3)
			(\(Z :. x :. y :. c) -> case (pixelAt img x y) of 
									PixelRGBF r g b  ->
										case c of 
											0 -> r
											1 -> g
											2 -> b
											 )


toImage ::  Array U DIM3 Float -> Image PixelRGBF
toImage imgArray = generateImage generator width height 
	where
		Z :. width :. height :. depth = R.extent imgArray
		generator x y = let (r,g,b) = (imgArray !(Z :. x :. y :. 0),imgArray !(Z :. x :. y :. 1),imgArray !(Z :. x :. y :. 2))
						in PixelRGBF r g b 






{-edgeDrawing :: Array D DIM3 Float -> int -> Array D DIM3 Float
edgeDrawing arr mag = R.traverse arr id edgeDetection
					where 
						_:. width :. height = R.extent arr

						edgeDetection get (Z:.a :.b :.c) 
								= Let opix = ((get (Z:.a:.b:.0)) + (get (Z:.a:.b:.1)) + (get (Z:.a:.b:.2))) / 3.0
									  bpix = ((get (Z:.a:.b+1:.0)) + (get (Z:.a:.b+1:.1)) + (get (Z:.a:.b+1:.2))) / 3.0	
									  lpix = ((get (Z:.a-1:.b:.0)) + (get (Z:.a-1:.b:.1)) + (get (Z:.a-1:.b:.2))) / 3.0 
									  

						{-# INLINE isBorder #-}			  	
						isBorder i j 
								= (i==0 || i>=width -1 ) || (j==0 || j>= height -1 )

-}

						

main :: IO()
main = do
	path <- getLine
	eimg <- readImage path
	let savePath = "mod.jpg"
	case eimg of 
		Left err -> putStrLn ("Error loading the file " ++ err)
		Right img -> do
			imgArray <- getImageArr $ M.promoteImage $ convertRGB8 img 	-- ImgArray contains the Pixel value
			
			finalArray <- R.computeUnboxedP $ F.toSepia imgArray    -- To obtain manifest array      
			saveJpgImage 100 savePath (ImageRGBF $ toImage finalArray)
			

	return ()






