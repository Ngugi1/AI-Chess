
module GameStep where
import qualified Representation as Rep
import qualified AI
import qualified Validation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace
import qualified ChessParser
import qualified Persist
import System.CPUTime
import System.Random

gameAsPicture:: Rep.State -> IO Picture
gameAsPicture (Rep.EndState background message) = return $ pictures [background, message]
gameAsPicture (Rep.ChooseColor) = do
  white_button <- loadBMP "./assets/wK.bmp"
  black_button <- loadBMP "./assets/bK.bmp"
  let title = scale 0.5 0.5 $ Text "Choose a color"
  return $ pictures [translate (-300) 100 $ title, (translate 100 0 $ white_button), (translate (-100) 0 $  black_button)]
gameAsPicture state =  return $ pictures $  [(Rep.difficultyValue state)] ++ imgs
  where img:imgs = Rep.images state

-- White button coordinates
getHumanPlayerColor:: (Float, Float) -> Maybe Rep.PlayerColor
getHumanPlayerColor (x, y)
 | x >= (-140) && x <= (-60)  && y > (-40)  && y < 40 = Just Rep.Black
 | x < 140 && x > 60 && y > (-40)  && y < 40  = Just Rep.White
 | otherwise = Nothing

-- Update the difficulty of the game
updateDifficulty :: Rep.State -> Int -> Rep.State
updateDifficulty state value =
   Rep.State (Rep.background state) (Rep.save state) (Rep.load state)
            newDepth
            difficultyLabel
            (Rep.origin state)
            (Rep.previousSelection state)
            (Rep.offset state) (Rep.images state)
            (Rep.whiteQueen state) (Rep.blackQueen state)
            (Rep.player state) (Rep.center state)
            (Rep.history state) (Rep.board state)
 where newDepth = if ((Rep.depth state)  + value )  < 1  then 1 else ((Rep.depth state)  + value)
       difficultyLabel = translate 0 400 $ scale 0.25 0.25 $ Text ("Difficulty: " ++ (show newDepth))
--  Display the color selection screen
transformGame:: Event -> Rep.State -> IO Rep.State
transformGame (EventKey (MouseButton LeftButton) Down _ (x,  y)) (Rep.ChooseColor) = do
  let color = getHumanPlayerColor (x, y)
  case color of
    Nothing -> return $ Rep.ChooseColor
    (Just c) -> Rep.initialState c
-- Transform game when you see LeftButton mouse clicks
transformGame (EventKey (MouseButton LeftButton) Down _ (x,  y)) state
    -- If the player tries to move his own piece to a position he/she doesn't already occupy and the move is legal - make move
    | previousPositionValid && currentPositionValid &&
      currentPlayerOwnsPreviousPosition &&
      currentPlayerOwnsCurrentPosition == False && legalMove = do
      -- traceIO (show (rank, file) ++ (show (Rep.player state)))
      let p = Validation.makeMove state pieceToMove (rank, file)

      result <- AI.playAI (Validation.makeMove state pieceToMove (rank, file)) (Rep.depth state) ((Rep.getPiecePosition pieceToMove), (rank,file))
      return result
    -- Reset the previous step if the player had clicked outside the valid region
    | x > (-600) && x < (-400) && y < 250 && y > 200 = Persist.loadGame
    | x > (-600) && x < (-400) && y < (30) && y > (-30) && not (Validation.isRobot (Rep.player state)) = Persist.saveHistory (Rep.history state) >> return state
    | x < 700 && x > 400 && y < 250 && y > 150 =  return (updateDifficulty state 1)
    | x < 700 && x > 400 && y > (-30) && y < 40 =  return (updateDifficulty state (-1))
    | currentPositionValid  || previousPositionValid == False = return newState
    -- We don't know what the player is doing - ignore the clicks
    | otherwise = return state
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
        newState = Rep.State (Rep.background state) 
                             (Rep.save state) (Rep.load state)
                             (Rep.depth state) (Rep.difficultyValue state)
                             (Rep.origin state) (rank, file) 
                             (Rep.offset state) (Rep.images state) 
                             (Rep.whiteQueen state) (Rep.blackQueen state) 
                             (Rep.player state) (Rep.center state) 
                             (Rep.history state) (Rep.board state)

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




