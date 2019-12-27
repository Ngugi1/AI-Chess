module AI where
import qualified Representation as Rep
import qualified Validation
import Debug.Trace
type Depth = Int
playAI:: Rep.State -> Depth -> Rep.State 
playAI state depth = validMoves state  (Rep.player state)


-- Find all legal moves a player can make
validMoves:: Rep.State -> Rep.Player -> Rep.State 
validMoves state player =
    trace (show $ Rep.player state)
    -- trace (show allMoves)
    trace "|+++++++++++++++++++++++++++++++++|"
    trace (show legalMoves)
    state
 where
     allMoves = (getPlayerPossibleMoves (Rep.board state)  player)
     legalMoves = filter (\(piece, move) -> Validation.validMove state piece move player) allMoves


-- Find all possible moves a player can make
getPlayerPossibleMoves:: Rep.Board -> Rep.Player -> [(Rep.Piece, Rep.Move)]
getPlayerPossibleMoves board player =
    -- trace (show playerPieces)
    -- trace "++++++++++++++++++++"
    playerMoves
 where
     playerPieces = Rep.getPlayerPieces board player
     playerMoves = concat $ map (\piece ->
                        [(piece,((Rep.getPiecePosition piece), to)) | to <-  Validation.generatePositions piece])
                        playerPieces

