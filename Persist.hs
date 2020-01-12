module Persist where
import qualified ChessParser as CP
import qualified Representation as Rep
import Graphics.Gloss
import Debug.Trace
import qualified Validation

filename:: String
filename = "state.txt"

-- Save the text in a text file
saveHistory:: String -> IO ()
saveHistory contents = do
    writeFile filename contents

-- Load game history
loadGame :: IO Rep.State
loadGame = do
    contents <- readFile filename
    let (((last:steps), _):rest) = CP.parse contents
    state <-  (getInitialState last)
    return $ restoreGame steps state

-- Get the initial state of the board taking care of color which the human player/ai should play
getInitialState:: CP.Step -> IO Rep.State
getInitialState (CP.LastStep "white") = do
    Rep.initialState Rep.White
getInitialState (CP.LastStep "black") = do
    Rep.initialState Rep.Black

-- File to int
intFile 'a' = 0
intFile 'b' = 1
intFile 'c' = 2
intFile 'd' = 3
intFile 'e' = 4
intFile 'f' = 5
intFile 'g' = 6
intFile 'h' = 7
intFile _ = 9
-- Find pawn to move
pawnToMove ::  Rep.Player -> Rep.State -> Rep.Position -> Rep.Piece
pawnToMove  (Rep.Robot _) state (rank, file) = 
    Rep.getPieceOnBoard (Rep.board state) position
    where all@position:positions = [(r, f) | f <- [file, file ..],
                                    r <- [rank + 1, rank + 2 .. 6],
                                    (Rep.pawn $ Rep.getPieceOnBoard (Rep.board state) (r, f)) == True]

pawnToMove  (Rep.Human _) state (rank, file) = 
    Rep.getPieceOnBoard (Rep.board state) position
    where all@position:positions = [(r, f) | f <- [file, file ..],
                                    r <- [rank - 1, rank - 2 .. 1], 
                                    (Rep.pawn $ Rep.getPieceOnBoard (Rep.board state) (r, f)) == True]


-- Restore game
restoreGame:: [CP.Step] -> Rep.State -> Rep.State
restoreGame [] state = state
-- Restore a normal move
restoreGame ((CP.Move (CP.DIS (CP.Square fromFile fromRank)) (CP.Square toFile toRank)):steps)  state =
    restoreGame steps updatedState
    where updatedState = Validation.makeMove
                            state
                            (Rep.getPieceOnBoard (Rep.board state) (fromRank, (intFile fromFile))) 
                            (toRank, intFile toFile)

-- Restore Pawn move
restoreGame ((CP.PawnMove (CP.Square toFile toRank) promote):steps)  state =
    restoreGame steps updatedState
    where piece = pawnToMove (Rep.player state) state (toRank, intFile toFile)
          updatedState = Validation.makeMove state piece (toRank, intFile toFile)

-- Restore pawn capture
restoreGame ((CP.PawnCapture (CP.PDIS fromFile) (CP.Square toFile toRank) promote):steps) state =
    restoreGame steps updatedState
    where piece = Rep.getPieceOnBoard (Rep.board state) (toRank - 1, intFile fromFile)
          updatedState = Validation.makeMove state piece (toRank, intFile toFile)
-- Restore kingside castling
restoreGame ((CP.KSCastling):steps) state
 | Validation.isRobot player = Validation.makeMove state (Rep.getPieceOnBoard currentBoard robot_king_pos) robot_kingside_to
 | otherwise = Validation.makeMove state (Rep.getPieceOnBoard currentBoard human_king_pos) human_kingside_to
 where
    currentBoard = (Rep.board state)
    player = (Rep.player state)
    human_king_pos = (0,4)
    robot_king_pos = (7,4)
    human_kingside_to =  (0,6)
    robot_kingside_to = (7,6)

-- Restore queenside castling
restoreGame ((CP.QSCastling):steps) state
 | Validation.isRobot player = Validation.makeMove state (Rep.getPieceOnBoard currentBoard robot_king_pos) robot_kingside_to
 | otherwise = Validation.makeMove state (Rep.getPieceOnBoard currentBoard human_king_pos) human_kingside_to
 where
    currentBoard = (Rep.board state)
    player = (Rep.player state)
    human_king_pos = (0,4)
    robot_king_pos = (7,4)
    human_kingside_to = (0,2)
    robot_kingside_to = (7,2)

-- Restore queenside castling
restoreGame ((CP.Stalemate):steps) state = Rep.EndState (Rep.background state) (scale 0.15 0.15 $  (Text "Stalemate"))
restoreGame ((CP.Checkmate):steps) state
 | Validation.isRobot player =  Rep.EndState (Rep.background state) (scale 0.15 0.15 $  (Text "Checkmate - AI won!"))
 | otherwise =  Rep.EndState (Rep.background state) (scale 0.15 0.15 $ (Text "Checkmate - Human won!"))
    where player = (Rep.player state)
