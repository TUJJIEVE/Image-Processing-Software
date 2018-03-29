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
import qualified Data.Array.Repa     as R -- for Repa


getImageArr :: Image PixelRGBA8-> IO(Array D DIM3 Word8)
getImageArr img = do
		return $ R.fromFunction
			(Z :. imageHeight img :. imageWidth img :. 4)
			(\(Z :. y :. x :. c) -> case (pixelAt img x y) of 
									PixelRGBA8 r g b a ->
										case c of 
											0 -> r
											1 -> g
											2 -> b
											3 -> a )

toImage ::  Array U DIM3 Word8 -> Image PixelRGBA8
toImage imgArray = generateImage generator width height 
	where
		Z :. width :. height :. depth = R.extent imgArray
		generator x y = let (r,g,b,a) = (imgArray !(Z :. x :. y :. 0),imgArray !(Z :. x :. y :. 1),imgArray !(Z :. x :. y :. 2),imgArray !(Z :. x :. y :. 3))
						in PixelRGBA8 r g b a


main :: IO()
main = do
	path <- getLine
	eimg <- readImage path
	case eimg of 
		Left err -> putStrLn ("Error loading the file " ++ err)
		Right img -> do
			imgArray <- getImageArr $ convertRGBA8 img
			return ()
		Right _ -> putStrLn ("Pixel format not recognized")

	return ()






