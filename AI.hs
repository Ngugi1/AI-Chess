module AI where
import qualified Representation as Rep
import qualified Validation
import Control.Concurrent
import Control.Monad
import Debug.Trace

-- The last human-ai move
data LastStep = LastStep {human_move:: Rep.Move, ai_move:: Rep.Move} deriving (Show)
-- Dept of a tree
type Depth = Int

-- Value big enough to rep infinity
infinity:: Int
infinity = 10000000

-- Tree structure - keep root distinct from other nodes
data Tree = Root{last_step::LastStep,  subtree:: [Tree]}
            | Node {move:: Rep.Move,
                    player:: Rep.Player,
                    fitness:: Int,
                    subtree:: [Tree]} deriving (Show)


-- Let AI select the best move to counter human move
playAI:: Rep.State -> Depth -> Rep.Move -> IO (Either String Rep.State)
playAI state depth human_move = do
    trees <- (createTree state depth)
    -- trace (show $ take 1 trees)
    return $ Right state
--  | (length trees == 0 ) && length (validMoves state (Rep.otherPlayer player)) > 0 = Left "Stalemate" -- A stalemate
--  | (length trees > 0 ) && length (validMoves state (Rep.otherPlayer player)) == 0 = Left "Stalemate" -- A stalemate
--  | Validation.kingUnderThreat new_state (Rep.player new_state) && length (validMoves new_state (Rep.otherPlayer player)) == 0 = Left (show player ++ "won!")
--  | otherwise = Right new_state -- allow game to continue
--  where  trees = (map (visitTree) (createTree state depth)) -- Create the tree and tag it with fitness values
--         best_fit = maximum (map (fitness) trees) -- Find the best fit
--         tree_map = (map (\tree -> ((fitness tree, move tree)))) -- Get subtree and it's fitness
--         best_tree:moves = filter (\tree -> (fitness tree) == best_fit) trees -- Find tree representing best move
--         best_move = (move best_tree) -- Get the best move
--         final_tree = (Root (LastStep human_move best_move) trees) -- Make one big tree from the many subtrees
--         new_state = Validation.makeMove state (Rep.getPieceOnBoard (Rep.board state) (fst best_move)) (snd best_move) -- Make the move
--         player = (Rep.player state)

-- Create trees
createTree:: Rep.State -> Depth -> IO [Tree]
createTree state 0 = return $  []
createTree state depth  = do
    mvars <- replicateM (length moves) newEmptyMVar
    let mvar_moves = zip moves mvars
    -- Get a list of ThreadIDs
    mapM (forkIO.processMove state depth) mvar_moves
    -- Try to read the MVars
    trees <- mapM readMVar mvars
    let x = take 1 trees
    print (show x)
    return trees
    where moves = validMoves state (Rep.player state)

-- Based on the new state after a move, determine the fitness value of a node
processMove:: Rep.State -> Depth -> ((Rep.Piece, Rep.Move), MVar Tree)  -> IO ()
processMove state depth ((piece ,move), mvar)
 | Validation.checkStalemate state (Rep.player state) = do
    putMVar mvar $ Node move (Rep.player state) 0 []
 | Validation.kingUnderThreat state player && (length $ (validMoves state player)) == 0 =  do
    if (Rep.color player) == Rep.whitePlayer -- Black checked white
    then  putMVar mvar $ Node move player (-infinity) []  -- No more children for this state
    else putMVar mvar $ Node move player infinity []
 | otherwise = do
    subtrees <- (createTree newState (depth - 1))
    putMVar mvar $ Node move (Rep.player newState) (getFitness (Rep.player newState) newState) subtrees -- calculate fitness 
 where player = (Rep.player state)
       newState = Validation.makeMove state piece (snd move)

visitTree:: Tree -> Tree
visitTree node@(Node mv player ft []) =
    node
visitTree node@(Node mv player fit children) =
    let subtrees = map (visitTree) children in
        (Node mv player (minOrMax $ map (fitness) subtrees) children)
    where minOrMax = if (Rep.color player) == "white" then maximum else minimum


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
    playerMoves
 where
     playerPieces = Rep.getPlayerPieces board player
     playerMoves = concat $ map (\piece ->
                        [(piece,((Rep.getPiecePosition piece), to)) | to <-  Validation.generatePositions piece])
                         playerPieces

