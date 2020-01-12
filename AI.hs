module AI where
import qualified Representation as Rep
import qualified Validation
import Control.Concurrent
import Control.Monad
import Debug.Trace
import Graphics.Gloss

-- Dept of a tree
type Depth = Int

-- Value big enough to rep infinity
infinity:: Int
infinity = 10000000

-- Let AI select the best move to counter human move
playAI:: Rep.State -> Depth -> Rep.Move -> IO  Rep.State
playAI state depth human_move = do
    trees <- createTree state depth
    makeAIMove state ai_player trees
    where ai_player = (Rep.player state)
          human_player = (Rep.otherPlayer ai_player)

-- Prune cache tree
pruneCacheTree:: Rep.Tree -> [Rep.Tree]
pruneCacheTree (Rep.Node _ _ _ _ subtree) = subtree

-- Make AI move
makeAIMove :: Rep.State  -> Rep.Player -> [Rep.Tree] -> IO Rep.State
makeAIMove state player [] = return $ Rep.EndState (Rep.background state) (Text "Stalemate")
makeAIMove state player@(Rep.Robot colour) trees@(t:ts) = 
    return new_state
--  | other_player_legal_moves == 0 = return $ Rep.EndState (Rep.background state) (Text "Stalemate")
--  | other_player_legal_moves == 0 && Validation.kingUnderThreat new_state (Rep.otherPlayer player) = return $ Rep.EndState (Rep.background state) (Text "AI Won")
--  | otherwise = return new_state
    where updated_state = Validation.makeMove state (Rep.getPieceOnBoard (Rep.board state) (fst best_move)) (snd best_move)
          best_fitness = 
              if (colour == Rep.whitePlayer) 
            then  maximum $ map (Rep.fitness) trees 
            else  minimum $ map (Rep.fitness) trees
          best_tree = filter (\tree -> (Rep.fitness tree) == best_fitness) trees
          best_move = Rep.move $ best_tree !! 0
          new_state = Rep.State (pruneCacheTree (best_tree !! 0)) -- Keep all the children - moves which human can make then AI and so on
                           (Rep.background updated_state)
                           (Rep.save updated_state) 
                           (Rep.load updated_state) 
                           (Rep.depth updated_state) 
                           (Rep.difficultyValue updated_state)
                           (Rep.origin updated_state)
                           ((-1),(-1)) (Rep.offset updated_state) 
                           (Rep.images updated_state) 
                           (Rep.whiteQueen updated_state) 
                           (Rep.blackQueen updated_state) (Rep.otherPlayer player) 
                           (Rep.center updated_state)
                           (Rep.history updated_state)
                           (Rep.board updated_state)
-- Create trees
createTree:: Rep.State -> Depth -> IO [Rep.Tree]
createTree state 0 = return $  []
createTree state depth  = do
    mvars <- replicateM (length moves) newEmptyMVar
    let mvar_moves = zip moves mvars
    -- Get a list of ThreadIDs
    mapM (forkIO.processMove state depth) mvar_moves
    -- Try to read the MVars
    mapM readMVar mvars >>= return
    where moves = validMoves state (Rep.player state)

-- Search if a move already exists in the cache
getCachedMove :: Rep.State -> Rep.Move -> Rep.Player -> Maybe Rep.Tree
getCachedMove state move player =
    case possible_hits of 
        [] -> Nothing
        hit:hits -> Just hit
    where cached = (Rep.cache state)
          possible_hits = filter (\tree -> (player == (Rep.current_player tree) && (Rep.move tree) == move)) cached

-- Based on the new state after a move, determine the fitness value of a node
processMove:: Rep.State -> Depth -> ((Rep.Piece, Rep.Move), MVar Rep.Tree)  -> IO ()
processMove state depth ((piece , move), mvar)
 |  fst $ cache_hit = do  putMVar mvar (snd  cache_hit) -- first check if a move is available in the cache
 | Validation.checkStalemate state player = do
    putMVar mvar $ Rep.Node move player [] 0 []
 | Validation.kingUnderThreat state player && (length $ (validMoves state player)) == 0 =  do
    if (Rep.color player) == Rep.whitePlayer -- Black checked white
    then  putMVar mvar $ Rep.Node move player [] (-infinity) []  -- No more children for this state
    else putMVar mvar $ Rep.Node move player [] infinity []
 | otherwise = do
    subtrees <- (createTree newState (depth - 1))
    case subtrees of
        [] -> putMVar mvar $ Rep.Node move player [] (getFitness player newState) subtrees
        child:children -> putMVar mvar $ (Rep.Node move player [] (minOrMax $ map (Rep.fitness) subtrees) subtrees)
 where player = (Rep.player state)
       newState = Validation.makeMove state piece (snd move)
       minOrMax = if (Rep.color player) == "white" then maximum else maximum
       cache_hit = case getCachedMove state move player of
                        Nothing -> (False, Rep.Empty)
                        (Just node) -> (True, node) 



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
getFitness player state = (sum $ map (pieceValue) pieces0) - (sum $ map (pieceValue) pieces1)
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

