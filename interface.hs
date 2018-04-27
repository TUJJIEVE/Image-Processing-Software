import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

addd :: Monad m => [Char] -> [Char] -> m [Char]
addd x y = do 
            let z = x ++ " " ++ y 
            return z

main :: IO ()
main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "interface.glade"

    mainWindow <- builderGetObject builder castToWindow "main_window"
    onDestroy mainWindow mainQuit
    
    imgpath <- builderGetObject builder castToEntry "path"
    output <- builderGetObject builder castToEntry "output"
    entrySetText output "please click OK button after selecting filter and entring path "
    done    <- builderGetObject builder castToButton "done"
    filter1 <- builderGetObject builder castToButton "h1-1"
    filter2 <- builderGetObject builder castToButton "h1-2"
    filter3 <- builderGetObject builder castToButton "h1-3"
    filter4 <- builderGetObject builder castToButton "h2-1"
    filter5 <- builderGetObject builder castToButton "h2-2"
    filter6 <- builderGetObject builder castToButton "h2-3"
    fid <- builderGetObject builder castToEntry "filter"
    --onClicked done $ putStrLn (addd (do x <- entryGetText imgpath  return x) "shit")) --do
		     --name <- entryGetText imgpath
             --putStrLn (y) 
              --    where y = add name "shit"

    onClicked filter1 $ do
		 entrySetText fid "selected filter1"  
    onClicked filter2 $ do
		 entrySetText fid "selected filter2"
    onClicked filter3 $ do
		 entrySetText fid "selected filter3"
    onClicked filter4 $ do
		 entrySetText fid "selected filter4"
    onClicked filter5 $ do
		 entrySetText fid "selected filter5"
    onClicked filter6 $ do
		 entrySetText fid "selected filter6"
    onClicked done $ do
         entrySetText output "processing...wait till it finishes.."
         path <- entryGetText imgpath
         id <- entryGetTexidt fid           -- 
         y <- addd path id              
         putStrLn y
		 --entrySetText output "processing finished"
    
    widgetShowAll mainWindow
    mainGUI
