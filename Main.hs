module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.ViewPort

window:: Display
window = InWindow "AI-Human Chess" (800, 800) (400, 400)

backgroundColor:: Color
backgroundColor = white

-- Player
type Player = String

-- Position
type Position = (Float, Float)

-- Piece
data Piece = Piece {name:: Char, position:: Position, moved:: Bool, player:: Player}

-- Rank 
type Rank = [Piece]

-- Rank Number
type RankNumber = Int

-- Background
type Background = String
-- Board
type Board = (Picture, (RankNumber, [Rank]))

-- Make a rank
-- makeRank:: Int -> Rank
-- makeRank position
--     | even position = 
--     | otherwise = 

-- Make the initial board
-- initialBoard :: Board
-- initialBoard = 

gameAsPicture:: Board -> Picture
gameAsPicture state = pictures [fst state]

getBackgroundImage:: IO Picture
getBackgroundImage = loadBMP "chess.bmp"

transformGame:: Event -> Board -> Board
transformGame (EventResize _ ) board = board
transformGame _ board = board

updateGame :: Float -> Board -> Board
updateGame _ board = board

main:: IO ()
main =
    do
       img <- getBackgroundImage
       play window backgroundColor 30 (img, (0,[])) gameAsPicture transformGame updateGame