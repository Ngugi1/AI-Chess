module AI where
import qualified Representation as Rep
import qualified Validation
import Debug.Trace
data Tree = Root {move:: Rep.Move, player:: Rep.Player, fitness:: Int, subtree:: [Tree]}
            | Node {move:: Rep.Move, player:: Rep.Player, fitness:: Int, subtree:: [Tree]} deriving (Show)

type Depth = Int
playAI:: Rep.State -> Depth ->  Rep.State
playAI state depth = new_state
    where trees = (map (visitTree) (createTree state depth))
          best_fit = maximum (map (fitness) trees)
          tree_map = (map (\tree -> ((fitness tree, move tree))))
          best_tree:moves = filter (\tree -> (fitness tree) == best_fit) trees
          best_move = (move best_tree)
          new_state = Validation.makeMove state (Rep.getPieceOnBoard (Rep.board state) (fst best_move)) (snd best_move)
-- Create trees
createTree:: Rep.State -> Depth -> [Tree]
createTree state 0 = []
createTree state depth  =
    map (\(piece, move) ->
            let newState = Validation.makeMove state piece (snd move)
                ft = (getFitness (Rep.player newState) newState) in
                Root move (Rep.player state) (getFitness (Rep.player newState) newState) (createSubTree newState (depth -1))) moves
    where moves = validMoves state (Rep.player state)

-- create a subtree
createSubTree:: Rep.State -> Depth -> [Tree]
createSubTree state 0 = []
createSubTree state depth  =
    map (\(piece, move) ->
            let newState = Validation.makeMove state piece (snd move)
                ft = (getFitness (Rep.player newState) newState) in
                Node move (Rep.player state)  (getFitness (Rep.player newState) newState) (createSubTree newState (depth -1))) moves
    where moves = validMoves state (Rep.player state)

visitTree:: Tree -> Tree
visitTree root@(Root mv player fit []) = root
visitTree (Root mv player fit children) =
    let subtrees = map  (visitTree) children in
        (Root mv player (maximum $ map (fitness) subtrees) subtrees)
visitTree node@(Node mv player ft []) = 
    node
visitTree node@(Node mv player fit children) =
    let subtrees = map (visitTree) children in
        (Root mv player (maximum $ map (fitness) subtrees) children)


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

