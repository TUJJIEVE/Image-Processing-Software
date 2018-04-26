import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder




main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "interface.glade"

    mainWindow <- builderGetObject builder castToWindow "main_window"
    onDestroy mainWindow mainQuit
    
    imgpath <- builderGetObject builder castToEntry "path"
    output <- builderGetObject builder castToEntry "output"
    entrySetText output "please click OK button after selecting filter .... .... "
    done    <- builderGetObject builder castToButton "h1-1"
    filter1 <- builderGetObject builder castToButton "h1-2"
    filter2 <- builderGetObject builder castToButton "h1-3"
    filter3 <- builderGetObject builder castToButton "h2-1"
    filter4 <- builderGetObject builder castToButton "h2-2"
    filter5 <- builderGetObject builder castToButton "h2-3"
    filter6 <- builderGetObject builder castToButton "done"
    convert <- builderGetObject builder castToButton "convert"
    onClicked convert $ do
		 name <- entryGetText imgpath
	         entrySetText output  $"converting.. "++ name
    onClicked filter1 $ do
		 entrySetText output "you want to apply filter 1 click save"
    onClicked filter2 $ do
		 entrySetText output "you want to apply filter 2 click save"
    onClicked filter3 $ do
		 entrySetText output "you want to apply filter 3 click save"
    onClicked filter4 $ do
		 entrySetText output "you want to apply filter 4 click save"
    onClicked filter5 $ do
		 entrySetText output "you want to apply filter 5 click save"
    onClicked filter6 $ do
		 entrySetText output "you want to apply filter 6 click save"
    onClicked done $ do
		 name <- entryGetText imgpath
	         putStrLn ("processing...." ++ name )
 			    			
				  
    
    widgetShowAll mainWindow
    mainGUI
