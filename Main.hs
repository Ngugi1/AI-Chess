module Main where
import qualified Representation as Rep
import qualified GameStep as GStep
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment

main:: IO ()
main =
    do
    -- Assume the human player chose white color
    let humanPieceColor = "black"
    -- Get the screen dimensions
    (width, height) <- getScreenSize
    let screen = Rep.Screen width height
    -- Frames per second
    let framesPerSecond = 30
    background <- loadBMP "./assets/chess.bmp"
    whitePawn <- loadBMP "./assets/wP.bmp"
    blackPawn <- loadBMP "./assets/bP.bmp"
    whitepics <- Rep.loadPictures $ Rep.whitePieceNames
    blackpics <- Rep.loadPictures $ Rep.blackPieceNames
    let state = Rep.initialState 
                        humanPieceColor background screen 
                        (Rep.getScreenCenter screen) whitePawn 
                        blackPawn whitepics blackpics
    play Rep.window white framesPerSecond state GStep.gameAsPicture GStep.transformGame GStep.updateGame