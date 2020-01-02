
module GameStep where
import qualified Representation as Rep
import qualified AI
import qualified Validation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace

gameAsPicture:: Rep.State -> IO Picture
gameAsPicture state =  return $ pictures $ Rep.images state

-- Transform game when you see LeftButton mouse clicks
transformGame:: Event -> Rep.State -> IO Rep.State
transformGame (EventKey (MouseButton LeftButton) Down _ (x,  y)) state
    -- If the player tries to move his own piece to a position he/she doesn't already occupy and the move is legal - make move
    | previousPositionValid && currentPositionValid && 
      currentPlayerOwnsPreviousPosition &&
      currentPlayerOwnsCurrentPosition == False && legalMove = do
     result <- AI.playAI (Validation.makeMove state pieceToMove (rank, file)) 3 ((Rep.getPiecePosition pieceToMove), (rank,file))
    -- trace (show $ (previousPosition, (file, rank)))
     case result of
         (Right  updated_state) -> return $ updated_state
         (Left message) -> return $ state -- TODO:: Display end game here
    -- Reset the previous step if the player had clicked outside the valid region
    | currentPositionValid  || previousPositionValid == False =
     return $ newState
    -- We don't know what the player is doing - ignore the clicks
    | otherwise =
     return $ state
    where
        rank = (coordinateToPosition y)
        file = (coordinateToPosition x)
        currentPositionValid = Validation.checkValidPosition (rank, file)
        previousPosition = Rep.previousSelection state
        previousPositionValid = Validation.checkValidPosition $ previousPosition
        pieceToMove = Rep.getPieceOnBoard (Rep.board state) (Rep.previousSelection state)
        currentPlayerOwnsCurrentPosition =  Rep.playerOwns (Rep.player state) $ Rep.getPieceOnBoard (Rep.board state) (rank,file)
        currentPlayerOwnsPreviousPosition = Rep.playerOwns (Rep.player state) pieceToMove
        legalMove = Validation.validMove state pieceToMove (previousPosition,(rank, file)) (Rep.player state)
        newState = Rep.State (Rep.background state) (Rep.origin state) (rank, file) (Rep.offset state) (Rep.images state) (Rep.player state) (Rep.center state) (Rep.board state)

-- Ignore all other events
transformGame _ state = return $ state

updateGame :: Float -> Rep.State -> IO Rep.State
updateGame _ state = return $ state


-- Convert coordinate to a rank and file
coordinateToPosition :: Float -> Int
coordinateToPosition coordinate
    | coordinate  >= (- Rep.cellSize) * 4 &&  coordinate < (- Rep.cellSize) * 3 = 0
    | coordinate  >= (- Rep.cellSize) * 3 &&  coordinate < (- Rep.cellSize) * 2 = 1
    | coordinate  >= (- Rep.cellSize) * 2   && coordinate < (- Rep.cellSize) = 2
    | coordinate  >= (- Rep.cellSize) &&  coordinate < 0 = 3
    | coordinate  >= 0  &&  coordinate < Rep.cellSize  = 4
    | coordinate  >= Rep.cellSize  &&  coordinate < Rep.cellSize * 2 = 5
    | coordinate  >= Rep.cellSize * 2  &&  coordinate < Rep.cellSize * 3 = 6
    | coordinate  >= Rep.cellSize * 3  &&  coordinate < Rep.cellSize * 4 = 7
    | otherwise = -1




