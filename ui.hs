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
    filternumber <- builderGetObject builder castToEntry "filterid"
    queue <- ""
    queuenumber <- 0;
    isprocessing <- 0;
    entrySetText output "please click OK button after selecting filter .... .... "
    helloWorldButton <- builderGetObject builder castToButton "hello_world_button"
    onClicked helloWorldButton $ do
				     name <- entryGetText imgpath
	              		     id <- entryGetText filternumber
				     putStrLn "intialising...."	
				     entrySetText output (name ++ " joining to queue...")
				     do
					queue <- (queue ++ name ++ "@" ++id ++ "*")
				        queuenumber <- (queuenumber + 1)
					entrySetText output (name ++ " joined to queue...")					
				     entrySetText output ("Processing...filter for \""++ name ++ "\"")
    
    widgetShowAll mainWindow
    mainGUI
