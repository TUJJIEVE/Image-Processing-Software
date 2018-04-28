import Data.List
import System.IO
import Codec.Picture
import Control.Monad
import Data.Word
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2,DIM3, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M -- For juicy pixel api'selected
import qualified Data.Array.Repa     as R -- For repa
import qualified Filters as F -- for Filters
import qualified Graphics.UI.Gtk 	as G  -- For graphics
import qualified Graphics.UI.Gtk.Builder as B  -- for Builder objects


{-

The image obtained from readImage using JuicyPixel module is a Dyanmic Image on converting the image to type
RGBA8 it becomes Image PixelRGBA8 type

The Alpha channel is used to test the opacity of the pixels

If RGB8 value is [123,12,0] then RGBF values are [123/255.0,12/255.0,0255.0]

-}


type PartImage = Array D DIM3 Float
type Filter = PartImage -> PartImage

{- Function to convert the image to obtain the pixel values from the image the pixel values are stored in 3D Delayed REPA array-}
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

{- Function used to convert the Manifest array containing the pixel values back to image using juicy pixel api's -}
toImage ::  Array U DIM3 Float -> Image PixelRGBF
toImage imgArray = generateImage generator width height 
	where
		Z :. width :. height :. depth = R.extent imgArray
		generator x y = let (r,g,b) = (imgArray !(Z :. x :. y :. 0),imgArray !(Z :. x :. y :. 1),imgArray !(Z :. x :. y :. 2))
						in PixelRGBF r g b 
						





{-Add filter function adds filter to the image depending on the entered filtered-}
addFilter :: PartImage -> String -> IO(Array D DIM3 Float)
addFilter imgArray filter = do 
						case filter of 
							"Sepia" -> return(F.passes 3 imgArray F.toSepia)
							"Gaussian Blur" -> return(F.passes 1 imgArray F.gaussianBlur)
							"Saturate" -> return(F.saturate imgArray 2)
							"Gray Scale" -> return(F.passes 2 imgArray F.toGrayScale)
							"Edge detection" -> return(F.passes 1 imgArray F.edgeDetection)
							"Emboss" -> return(F.passes 1 imgArray F.emboss)  


{-processImage function takes input the image file path and the selected filter and returns sucess if filter is applied-}
processImage :: String -> String -> String -> IO(String)
processImage imagePath savePath filter = do
						eimg <- readImage imagePath
						
						case eimg of 
							Left err -> putStrLn ("Error loading the file " ++ err)
							Right img -> do
								imgArray <- getImageArr $ M.promoteImage $ convertRGB8 img 	-- ImgArray contains the Pixel value RGB
								filteredImage <-addFilter imgArray filter   -- To obtain the new Delayed array     
								finalArray <- R.computeP filteredImage      -- To obtain the Manifest array using the delayed array
								saveJpgImage 100 savePath (ImageRGBF $ toImage finalArray) -- For saving the image
								

						return ("Sucess")

   
main = do

    G.initGUI -- For initializing the GUI

    builder <- B.builderNew 
    B.builderAddFromFile builder "interface.glade"  -- For Initializing the builder

    mainWindow <- B.builderGetObject builder G.castToWindow "main_window" -- For initializing the main window
    G.onDestroy mainWindow G.mainQuit       -- For destroying the window on quit
    
    imgpath <- B.builderGetObject builder G.castToEntry "path"     -- textbar saves the image path entered
    savepath <- B.builderGetObject builder G.castToEntry "savepath" -- textbar saves the save path entered
  
    output <- B.builderGetObject builder G.castToEntry "output"    -- for printing the sucess of image conversion
    G.entrySetText output "please click OK button after selecting filter and entring path "
    G.entrySetText savepath "Enter output file name"
    G.entrySetText imgpath "Enter input file path here !"
    -- For linking the buttons 
    done    <- B.builderGetObject builder G.castToButton "done"
    filter1 <- B.builderGetObject builder G.castToButton "h1-1"
    filter2 <- B.builderGetObject builder G.castToButton "h1-2"
    filter3 <- B.builderGetObject builder G.castToButton "h1-3"
    filter4 <- B.builderGetObject builder G.castToButton "h2-1"
    filter5 <- B.builderGetObject builder G.castToButton "h2-2"
    filter6 <- B.builderGetObject builder G.castToButton "h2-3"
    fid <- B.builderGetObject builder G.castToEntry "filter"
    --onClicked done $ putStrLn (addd (do x <- entryGetText imgpath  return x) "shit")) --do
		     --name <- entryGetText imgpath
             --putStrLn (y) 
              --    where y = add name "shit"
     -- When a particular button is clicked text field is set accordingly
    G.onClicked filter1 $ do                  
		 G.entrySetText fid "Gaussian Blur"  
    G.onClicked filter2 $ do
		 G.entrySetText fid "Saturate"
    G.onClicked filter3 $ do
		 G.entrySetText fid "Gray Scale"
    G.onClicked filter4 $ do
		 G.entrySetText fid "Sepia"
    G.onClicked filter5 $ do
		 G.entrySetText fid "Edge detection"
    G.onClicked filter6 $ do
		 G.entrySetText fid "Emboss"
    G.onClicked done $ do
         G.entrySetText output "processing...wait till it finishes.."
         inpath <- G.entryGetText imgpath
         outpath <- G.entryGetText savepath
         id <- G.entryGetText fid 		    -- for getting which filter to apply        -- 
         y <- processImage inpath outpath id     -- for processing the image         
         G.entrySetText output y
    
    G.widgetShowAll mainWindow
    G.mainGUI







