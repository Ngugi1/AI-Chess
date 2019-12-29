module AI where
import qualified Representation as Rep
import qualified Validation
import Debug.Trace

data BreadCram = EmptyCram | Cram {cmove:: Rep.Move, cfitness:: Int} deriving (Show)
data Tree = Root {move:: Rep.Move, fitness:: Int, subtree:: [Tree]} 
            | Node {move:: Rep.Move, fitness:: Int, subtree:: [Tree]} deriving (Show)

type Depth = Int
playAI:: Rep.State -> Depth ->  Rep.State
playAI state depth =
 let tree:trees =  (createTree state depth)  in
     trace (show $ tree)
     trace (show $ visitTree tree ((subtree tree) ++ [tree]))
     state
-- Create trees
createTree:: Rep.State -> Depth -> [Tree]
createTree state 0 = []
createTree state depth  =
    map (\(piece, move) ->
            let newState = Validation.makeMove state piece (snd move)
                ft = (getFitness (Rep.player newState) newState) in
                Root move (getFitness (Rep.player newState) newState) (createSubTree newState (depth -1))) moves
    where moves = validMoves state (Rep.player state)

-- create a subtree
createSubTree:: Rep.State -> Depth -> [Tree]
createSubTree state 0 = []
createSubTree state depth  =
    map (\(piece, move) ->
            let newState = Validation.makeMove state piece (snd move)
                ft = (getFitness (Rep.player newState) newState) in
                Node move (getFitness (Rep.player newState) newState) (createSubTree newState (depth -1))) moves
    where moves = validMoves state (Rep.player state)

visitTree:: Tree -> [Tree] -> Tree
visitTree (Root mv fit children) [] = 
    trace ("Done" ++ (show mv))
    (Root mv fit children)
visitTree (Root mv fit _) (u:enexplored) =
    trace ("--- Begin --- "  ++ (show u))
    -- trace (show $ enexplored)
    visitTree u $ enexplored
visitTree (Node mv ft []) (u:enexplored) = 
    trace ("Visiting " ++ (show mv))
    -- trace (show $ enexplored)
    visitTree u enexplored
visitTree (Node mv fit (child:rest)) unexplored =
    visitTree child (rest ++ [(Node mv fit [])] ++ unexplored)





-- Piece value
pieceValue:: Rep.Piece -> Int
pieceValue piece
 | Rep.pawn piece = 1
 | Rep.rook piece = 5
 | Rep.knight piece = 3
 | Rep.bishop piece = 3
 | Rep.queen piece = 9
 | otherwise = 0

 --Get fitness based on state
getFitness:: Rep.Player -> Rep.State -> Int
getFitness player state
 | (Rep.color player) == "white" = (sum $ map (pieceValue) pieces0) - (sum $ map (pieceValue) pieces1)
 | otherwise = (sum $ map (pieceValue) pieces1) - (sum $ map (pieceValue) pieces0)
 where pieces0 = Rep.getPlayerPieces (Rep.board state) player
       pieces1 =  Rep.getPlayerPieces (Rep.board state) (Rep.otherPlayer player)
-- Find all legal moves a player can make
validMoves:: Rep.State -> Rep.Player -> [(Rep.Piece, Rep.Move)] 
validMoves state player =
 legalMoves
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
                         (take 2 playerPieces)

