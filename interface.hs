import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

addd x y = do 
            let shit = x ++ " " ++ y 
            return shit


main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "interface.glade"

    mainWindow <- builderGetObject builder castToWindow "main_window"
    onDestroy mainWindow mainQuit
    
    imgpath <- builderGetObject builder castToEntry "path"
    savepath <- builderGetObject builder castToEntry "savepath"
    output <- builderGetObject builder castToEntry "output"
    entrySetText output "please click OK button after selecting filter and entring path "
    entrySetText savepath "Enter output file name"
    entrySetText imgpath "Enter input file path here !"

    done    <- builderGetObject builder castToButton "done"
    filter1_1 <- builderGetObject builder castToButton "h1-1-1"
    filter1_2 <- builderGetObject builder castToButton "h1-1-2"
    filter1_3 <- builderGetObject builder castToButton "h1-1-3"
   -- filter1_4 <- builderGetObject builder castToButton "h1-1-4"
    --filter1_5 <- builderGetObject builder castToButton "h1-1-5"
   --filter2 <- builderGetObject builder castToButton "h1-2"
   -- filter3 <- builderGetObject builder castToButton "h1-3"
    filter4 <- builderGetObject builder castToButton "h2-1"
    filter5 <- builderGetObject builder castToButton "h2-2"
    filter6 <- builderGetObject builder castToButton "h2-3"
    fid <- builderGetObject builder castToEntry "filter"
    --onClicked done $ putStrLn (addd (do x <- entryGetText imgpath  return x) "shit")) --do
		     --name <- entryGetText imgpath
             --putStrLn (y) 
              --    where y = add name "shit"

    onClicked filter1_1 $ do
		 entrySetText fid "selected filter1 1 times"
    onClicked filter1_2 $ do
         entrySetText fid "selected filter1 2 times"
    onClicked filter1_3 $ do
         entrySetText fid "selected filter1 3 times"
    onClicked filter4 $ do
         entrySetText fid "selected filter4"
    onClicked filter5 $ do
		 entrySetText fid "selected filter5"
    onClicked filter6 $ do
		 entrySetText fid "selected filter6"
    onClicked done $ do
         entrySetText output "processing...wait till it finishes.."
         path <- entryGetText imgpath
         pathe <- entryGetText savepath
         id <- entryGetText fid 
         y <- addd path id
         putStrLn y
         putStrLn pathe
		 --entrySetText output "processing finished"
    
    widgetShowAll mainWindow
    mainGUI
