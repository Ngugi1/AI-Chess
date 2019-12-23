
module GameStep where
import qualified Representation as Rep
import qualified Validation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace

gameAsPicture:: Rep.State -> Picture
gameAsPicture state =  pictures $ Rep.images state

transformGame:: Event -> Rep.State -> Rep.State
transformGame (EventKey (MouseButton LeftButton) Down _ (x,  y)) state
    | previousPositionValid && currentPositionValid &&
    currentPlayerOwnsPreviousPosition &&
    currentPlayerOwnsCurrentPosition == False && legalMove =
        trace ("Legal move from - " ++ show  previousPosition ++ " To - " ++ show (file, rank))
        Validation.makeMove state pieceToMove (rank, file)
    | (currentPositionValid  && previousPositionValid == False) || legalMove == False  =
        trace ("Setting new previous position from - " ++ show previousPosition ++ " To - " ++ show (rank, file))
        Rep.State (Rep.background state) (Rep.origin state) (rank, file) (Rep.offset state) (Rep.images state) (Rep.player state) (Rep.center state) (Rep.board state)
    | otherwise = 
        trace (show previousPosition ++ " Previous  is valid = " ++ show previousPositionValid ++ "Owns - " ++  show currentPlayerOwnsPreviousPosition)
        trace (show (rank, file) ++ " Current is valid = " ++ show previousPositionValid ++ "Owns - " ++ show currentPlayerOwnsCurrentPosition)
        trace (Rep.player state)
        trace (Rep.piecePlayer pieceToMove)
        state
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

        
transformGame _ state = state

updateGame :: Float -> Rep.State -> Rep.State
updateGame _ state = state


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




