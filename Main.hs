module Main where
import qualified GameStep as GStep
import Graphics.Gloss
import qualified Representation as Rep
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Debug.Trace

main:: IO ()
main = do
    playIO FullScreen white 30 (Rep.ChooseColor) GStep.gameAsPicture GStep.transformGame GStep.updateGame