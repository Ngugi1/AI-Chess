
module GameStep where
import qualified Representation as Rep
import qualified Validation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace

gameAsPicture:: Rep.State -> Picture
gameAsPicture state =  pictures $ Rep.images state

transformGame:: Event -> Rep.State -> Rep.State
transformGame (EventKey (MouseButton LeftButton) Down _ (x,  y)) state = state
    where 
        rank = (coordinateToPosition y)
        file = (coordinateToPosition x)
        currentPositionValid = Validation.checkValidPosition (rank, file)
        previousPositionValid = Validation.checkValidPosition $ Rep.previousSelection state
        
transformGame _ state = state

updateGame :: Float -> Rep.State -> Rep.State
updateGame _ state = state


-- Convert coordinate to a rank and file
coordinateToPosition :: Float -> Int
coordinateToPosition coordinate
    | coordinate  >= (- Rep.cellSize) * 4 &&  coordinate < (- Rep.cellSize) * 3 = 1
    | coordinate  >= (- Rep.cellSize) * 3 &&  coordinate < (- Rep.cellSize) * 2 = 2
    | coordinate  >= (- Rep.cellSize) * 2   && coordinate < (- Rep.cellSize) = 3
    | coordinate  >= (- Rep.cellSize) &&  coordinate < 0 = 4
    | coordinate  >= 0  &&  coordinate < Rep.cellSize  = 5
    | coordinate  >= Rep.cellSize  &&  coordinate < Rep.cellSize * 2 = 6
    | coordinate  >= Rep.cellSize * 2  &&  coordinate < Rep.cellSize * 3 = 7
    | coordinate  >= Rep.cellSize * 3  &&  coordinate < Rep.cellSize * 4 = 8
    | otherwise = -1




