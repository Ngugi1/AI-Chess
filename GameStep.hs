
module GameStep where
import qualified Representation as Rep
import qualified AI
import qualified Validation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace

gameAsPicture:: Rep.State -> Picture
gameAsPicture state =  pictures $ Rep.images state

-- Transform game when you see LeftButton mouse clicks
transformGame:: Event -> Rep.State -> Rep.State
transformGame (EventKey (MouseButton LeftButton) Down _ (x,  y)) state
    -- If the player tries to move his own piece to a position he/she doesn't already occupy and the move is legal - make move
    | previousPositionValid && currentPositionValid && currentPlayerOwnsPreviousPosition && currentPlayerOwnsCurrentPosition == False && legalMove =
     trace ("Is move legal ? " ++ show legalMove)
     trace ("Is current position valid? " ++ show  ((rank, file) ,currentPositionValid))
     trace ("Is current previous valid? " ++ show  (previousPosition, previousPositionValid))
     trace ("Player owns current " ++ show currentPlayerOwnsCurrentPosition)
     trace ("Player owns previous " ++ show currentPlayerOwnsPreviousPosition)
     trace "+++++++++++++++++++++++++++++++++++++++++++++++++"
     AI.playAI (Validation.makeMove state pieceToMove (rank, file)) 3
    --  (Validation.makeMove state pieceToMove (rank, file))

        -- state
    -- Reset the previous step if the player had clicked outside the valid region
    | currentPositionValid  || previousPositionValid == False =
    --  trace (show $ (Validation.forwardMove (previousPosition, (rank, file))))
    --  trace ("Is move legal ? " ++ show legalMove)
     trace ("Is current position valid? " ++ show ((rank, file) ,currentPositionValid))
     trace ("Is current previous valid? " ++ show (previousPosition, previousPositionValid))
     trace ("Player owns current " ++ show currentPlayerOwnsCurrentPosition)
    --  trace ("Player owns previous " ++ show currentPlayerOwnsPreviousPosition)
     trace "----------------------------"
     newState
    -- We don't know what the player is doing - ignore the clicks
    | otherwise =
     trace ("Is move legal ? " ++ show legalMove)
     trace ("Is current position valid? " ++ show  ((rank, file) ,currentPositionValid))
     trace ("Is current previous valid? " ++ show (previousPosition, previousPositionValid))
     trace ("Player owns current " ++ show currentPlayerOwnsCurrentPosition)
     trace ("Piece " ++ show (Rep.getPieceOnBoard (Rep.board state) (rank, file)))
     trace "##################################"


        -- trace (show $ (Validation.forwardMove (previousPosition, (rank, file))))
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
        newState = Rep.State (Rep.background state) (Rep.origin state) (rank, file) (Rep.offset state) (Rep.images state) (Rep.player state) (Rep.center state) (Rep.board state)

-- Ignore all other events
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




