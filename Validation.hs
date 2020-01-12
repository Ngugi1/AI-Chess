module Validation where
import Representation as Rep
import Text.Regex.Posix
import Graphics.Gloss
import Debug.Trace
-- checkValidPosition
checkValidPosition:: Rep.Position -> Bool
checkValidPosition (rank, file) = rank >= 0 && rank < 8 && file >= 0 && file < 8
-- Remove a piece from a rank by replacing it with an empty piece
removePiece:: Rep.Position -> (Rep.Rank, [Rep.Piece]) -> (Rep.Rank, [Rep.Piece])
removePiece position pieces =
  replacePiece position (noPiece,position, Rep.Unknown, True, blank) pieces
-- Place a piece or replace at the position with the new piece 
replacePiece:: Rep.Position -> Rep.Piece -> (Rep.Rank, [Rep.Piece]) -> (Rep.Rank, [Rep.Piece])
replacePiece (rank, file) (new_pname,_,new_player,_,new_piece_img) (rn, pieces) =
  (rn, map filterPiece pieces)
  where filterPiece (pname,(rrank, ffile),pplayer,mmoved,iimage) = 
         if (rank == rrank && ffile == file)
         then (new_pname, (rank,file),new_player,True,new_piece_img)
         else (pname,(rrank, ffile),pplayer,mmoved, iimage)

-- Is a player a robot?
isRobot:: Rep.Player -> Bool
isRobot (Rep.Robot _) = True
isRobot other = False

-- Is human player
isHuman:: Rep.Player -> Bool
isHuman (Rep.Human _) = True
isHuman other = False
-- Convert Int to file
intToFile :: Int -> String
intToFile 0 ="a"
intToFile 1 = "b"
intToFile 2 = "c"
intToFile 3 = "d"
intToFile 4 = "e"
intToFile 5 = "f"
intToFile 6 = "g"
intToFile 7 = "h"
intToFile _ = "_"

-- Make a move
makeMove:: Rep.State -> Rep.Piece -> Rep.To -> Rep.State
makeMove state piece@(name, position@(rank,file), player, moved, img) to@(toRank, toFile)
 | kingSideCastle =
   if kingNotThreatened then makeMove finalState kRook (toRank, toFile - 1) else state
 | queenSideCastle =
   if kingNotThreatened then makeMove finalState qRook (toRank, toFile + 1) else state
 | pawn piece && isRobot currentPlayer && toRank == 0 =
   if (Rep.color player) == Rep.whitePlayer then
     makeMove state ("wQ", position, player, moved, (Rep.whiteQueen state)) to
    else
       makeMove state ("bQ", position, player, moved, (Rep.blackQueen state)) to
  | pawn piece && isHuman currentPlayer && toRank == 7 =
   if (Rep.color player) == Rep.whitePlayer then
     makeMove state ("wQ", position, player, moved, (Rep.whiteQueen state)) to
    else
       makeMove state ("bQ", position, player, moved, (Rep.blackQueen state)) to
  | otherwise = finalState
 where  finalState  = State (cache state)
                           (background state)
                           (save state) (load state) 
                           (depth state) 
                           (difficultyValue state)
                           (origin state)
                           ((-1),(-1)) (offset state) 
                           boardImages 
                           (Rep.whiteQueen state) 
                           (Rep.blackQueen state) (otherPlayer player) 
                           (center state) newHistory newboard
        kingNotThreatened = not (kingUnderThreat finalState (Rep.player state))
        currentBoard = (Rep.board state)
        currentPlayer = (Rep.player state)
        move = (position, to)
        newHistory = (Rep.history state)  ++ (moveType piece move state)
        newboard = (map (replacePiece to (name, to, player, True, img)) (map (removePiece position) (board state)))
        boardpieces =  concat $ map (\(rNo, rankPieces) -> rankPieces) newboard
        boardOrigin = (Rep.origin state)
        origin_x = (Rep.x_origin boardOrigin)
        origin_y = (Rep.y_origin boardOrigin)
        boardImages = (take 2 (Rep.images state)) ++ 
                      [Rep.save state]
                      ++  (map (\piece -> translate (origin_x + (cellSize * fromIntegral (snd (getPiecePosition piece))))
                                          (origin_y +  (cellSize * fromIntegral (fst (getPiecePosition piece))))
                                           (getPiecePicture piece))
                                           boardpieces)
        (kingSideCastle, kRook) = kingSideCastling currentBoard move piece currentPlayer
        (queenSideCastle, qRook) = queenSideCastling currentBoard move piece currentPlayer

-- Determine type of move
moveType :: Rep.Piece -> Rep.Move -> Rep.State -> String
moveType piece@(name, position@(rank,file), player, moved, img) move@(from, to@(toRank, toFile)) state
 | pawn piece && not (isCapture) && not (promotePawn currentPlayer move) = square ++ "empty" -- Record pawn move no promotion
 | pawn piece && not (isCapture) && (promotePawn currentPlayer move) = square ++ "= Q"
 | pawn piece && isCapture && not (promotePawn currentPlayer move) = (intToFile file) ++ "x" ++ square ++ "empty"-- capture no promotion
 | pawn piece && isCapture && (promotePawn currentPlayer move) = (intToFile file) ++ "x" ++ square ++ "= Q" -- Capture and promote
 | king piece && kingSideCastle = "0-0"
 | king piece && queenSideCastle = "0-0-0"
 | isCapture = dis ++ newName ++ "x" ++ square
 | otherwise = dis ++ newName  ++ square
 where currentHistory = (Rep.history state) -- Moves made so far
       square = (intToFile toFile) ++ (show toRank)
       dis = (intToFile file) ++ (show rank)
       newName = (drop 1 name)
       currentBoard = (Rep.board state) -- Board
       currentPlayer = (Rep.player state) -- Current player
       other_player = (Rep.otherPlayer player) -- Find the other player
       destinationPiece = Rep.getPieceOnBoard currentBoard to -- Get piece at the destination
       isCapture = Rep.playerOwns other_player destinationPiece -- Check if we are capturing a piece
       kingSideCastle = fst $ kingSideCastling currentBoard move piece currentPlayer
       queenSideCastle = fst $ queenSideCastling currentBoard move piece currentPlayer
-- check if a player is in a stalemate
checkStalemate :: Rep.State -> Rep.Player -> Bool
checkStalemate state player = ((length (map (hasValidMoves state player) playerPieces)) == 0)
 where playerPieces = Rep.getPlayerPieces (Rep.board state) player
      --  kingNotThreatened = not (kingUnderThreat state (Rep.player state))

-- Has valid moves
hasValidMoves:: Rep.State -> Rep.Player -> Rep.Piece -> Bool
hasValidMoves state player piece
  | simulateMoves state piece position player positions = True
  | otherwise =  False
  where position = getPiecePosition piece
        positions = generatePositions piece


-- Simulate movements of the pieces
simulateMoves :: Rep.State -> Rep.Piece -> Rep.Position -> Rep.Player -> [Rep.Position] -> Bool
simulateMoves _ _ _ _ [] = False
simulateMoves state piece position player (pos:positions)
  | validMove state piece (position, pos) player =
    if (kingUnderThreat (makeMove state piece pos) player) then (simulateMoves state piece position player positions) else True
    -- if the move is valid, it must not leave this player's king under attack
  | otherwise = simulateMoves state piece position player positions

 -- Valid moves - dispatch based on the piece
validMove:: Rep.State -> Rep.Piece -> Rep.Move -> Rep.Player -> Bool
validMove state piece move player
 | rook piece = checkValidPosition (snd move) && checkRookMove state move player -- Do rook
 | knight piece = checkValidPosition (snd move) &&  checkKnightMove state move player -- Knight
 | bishop piece = checkValidPosition (snd move) &&  checkBishopMove state move player -- Bishop stuff
 | queen piece = checkValidPosition (snd move) &&  checkQueenMove state move player -- Queen stuff
 | king piece && checkValidPosition (snd move) &&  validMoveKing state piece move player = True
 | pawn piece = checkValidPosition (snd move) &&  checkPawnMove state move player
 | otherwise = False

-- King conditions are more complex - put them in own function
validMoveKing :: Rep.State -> Rep.Piece -> Rep.Move -> Rep.Player -> Bool
validMoveKing state piece move player
 | (checkKingMove state move player) || kingSideCast || queenSideCast = True
 | otherwise = False
 where currentBoard = board state
       (kingSideCast, ksRook) = kingSideCastling currentBoard move piece player
       (queenSideCast, qsRook) = queenSideCastling currentBoard move piece player
       newPosition = snd move
       newRookKingSidePos = ((fst (snd move))  , ((snd (snd move)) - 1))
       newRookQueebSidePos = ((fst (snd move))  , ((snd (snd move)) + 1))
-- ####  King movement ###
checkKingMove :: Rep.State -> Rep.Move -> Rep.Player -> Bool 
checkKingMove state move player
 | singleStep && horizontalMove move &&
   not (horizontallyObstructed currentBoard move) &&
   (emptyDestination || otherPlayerPiece) = True
 | singleStep && verticalMove move && not (verticallyObstructed currentBoard move) &&
    (emptyDestination || otherPlayerPiece) = True
 | otherwise = False
 where  destinationPosition = (snd move)
        currentBoard = (board state)
        destinationPiece = getPieceOnBoard currentBoard destinationPosition
        piecePlayed = getPieceOnBoard currentBoard (fst move)
        emptyDestination = emptyPosition currentBoard destinationPosition
        otherPlayerPiece = playerOwns (otherPlayer player) destinationPiece 
        singleStep = (stepsMoved move == (1,0)) || (stepsMoved move == (0,1))
        twoSteps = (stepsMoved move == (0,2))

-- Castling - original rook positions
kingSideRobotRook = (7,7) -- Original position if it hasn't moved
kingSideHumanRook = (0,7) -- Original Position - not moved
queenSideRobotRook = (7,0) -- Original position
queenSideHumanRook = (0,0)



-- Check if the King castled
kingSideCastling:: Rep.Board -> Rep.Move -> Rep.Piece -> Rep.Player -> (Bool, Rep.Piece)
kingSideCastling board move piece (Rep.Robot player)
 | stepsMoved move == (0, 2) && not (hasPieceMoved piece) &&
 rook  (getPieceOnBoard board kingSideRobotRook) &&
 not (horizontallyObstructed board (fst move, kingSideRobotRook)) = (True , (getPieceOnBoard board kingSideRobotRook))
kingSideCastling board move piece (Rep.Human player)
 | stepsMoved move == (0, 2) && not (hasPieceMoved piece) && rook (getPieceOnBoard board kingSideHumanRook)  
 && not (horizontallyObstructed board (fst move, kingSideHumanRook))  = (True, (getPieceOnBoard board kingSideHumanRook))
kingSideCastling _ move piece _ = (False, piece)

-- Queen Side castling
queenSideCastling:: Rep.Board -> Rep.Move -> Rep.Piece -> Rep.Player -> (Bool, Rep.Piece)
queenSideCastling board move piece (Rep.Robot player)
 | stepsMoved move == (0, 2) && not (hasPieceMoved piece) &&
  rook (getPieceOnBoard board queenSideRobotRook)  &&
  not (horizontallyObstructed board (fst move, queenSideRobotRook)) = (True, (getPieceOnBoard board queenSideRobotRook))
queenSideCastling board move piece (Rep.Human player)
 | stepsMoved move == (0, 2)  && not (hasPieceMoved piece) &&
  rook (getPieceOnBoard board queenSideHumanRook) &&
  not (horizontallyObstructed board (fst move, queenSideHumanRook))  = (True, (getPieceOnBoard board queenSideHumanRook))
queenSideCastling _ _ piece _ = (False, piece)

-- Check if king is under threat - being attacked
kingUnderThreat :: Rep.State -> Rep.Player -> Bool
kingUnderThreat state player =
  (length (positionUnderAttack state (getPiecePosition playersKing) player)) > 0 && not kingMoves
  where playersKing = (filter (playerOwns player) (filter king (concat (map (\(rank, pieces) -> pieces) (board state))))) !! 0
        kingMoves = hasValidMoves state player playersKing

-- Pawn rules
-- Rules applicable to the pawn
checkPawnMove:: Rep.State -> Rep.Move -> Rep.Player -> Bool
checkPawnMove state move player
  | diagonalMove move && pieceMovedBefore && pieceOwnedByOtherPlayer && not unoccupiedDestination = True
  | movedForward && movedVertically && (steps == (2,0)) && not pieceMovedBefore && unoccupiedDestination = True
  | movedForward && movedVertically && (steps == (1,0)) && unoccupiedDestination = True
  | otherwise = False
  where destinationPosition = (snd move)
        currentBoard = (board state)
        pieceAtOrigin = (getPieceOnBoard (board state) (fst move))
        pieceAtDestination = (getPieceOnBoard currentBoard destinationPosition)
        unoccupiedDestination = emptyPosition currentBoard destinationPosition
        pieceOwnedByOtherPlayer = playerOwns (otherPlayer player) pieceAtDestination 
        steps = stepsMoved move
        movedVertically = verticalMove move
        movedForward = forwardMove player move
        pieceMovedBefore = (hasPieceMoved pieceAtOrigin)
        -- (enPassantPos, enPassantAttacks) = enPassantPawnCapture state move player
-- Pawn Promotion
promotePawn :: Rep.Player -> Rep.Move -> Bool
promotePawn (Rep.Robot p) (_, (0, _)) = True
promotePawn (Rep.Human p) (_, (7, _)) = True
promotePawn _ _ =  False

-- Is it an enpassant ?
enPassantPawnCapture :: Rep.State -> Rep.Move -> Rep.Player -> (Rep.Position, [Rep.Move])
enPassantPawnCapture state ((fRank, fFile),to) player
 | (isRobot player) && (steps == (2,0)) =  humanAttackingPieces 
 | (isHuman player) && (steps == (2,0)) =  robotAttackingPieces 
 | otherwise = (to, [])
  where robotEnPassantPos = (fRank-1, fFile)
        humanEnPassantPos = (fRank+1, fFile)
        steps = (stepsMoved((fRank, fFile),to))
        humanAttackingPieces = (robotEnPassantPos, (positionUnderAttack state robotEnPassantPos (otherPlayer player)))
        robotAttackingPieces = (robotEnPassantPos, (positionUnderAttack state humanEnPassantPos (otherPlayer player)))

-- Rules for the rook
-- Rook is allowed to move horizontally and vertically as long as it is 
-- not obstructed by any other piece 
checkRookMove::Rep.State -> Rep.Move -> Rep.Player -> Bool
checkRookMove state move player
  | horizontalMove move && (emptyDestination || ownedByOtherPlayer )  && (not $ horizontallyObstructed currentBoard move) = True
  | verticalMove move && (emptyDestination || ownedByOtherPlayer ) && (not $ verticallyObstructed currentBoard move) = True 
  | otherwise = False
  where currentBoard = board state
        destinationPosition = (snd move)
        destinationPiece = getPieceOnBoard currentBoard destinationPosition
        emptyDestination = emptyPosition currentBoard destinationPosition
        ownedByOtherPlayer = playerOwns (otherPlayer player) destinationPiece 
-- Knight movement 
-- Knight can move over pieces
checkKnightMove:: Rep.State -> Rep.Move -> Rep.Player -> Bool 
checkKnightMove state move player = 
  (knightMove move) && (emptyDestination || ownedByOtherPlayer)
 where 
  currentBoard = (board state)
  destinationPosition = (snd move)
  emptyDestination = emptyPosition currentBoard destinationPosition
  ownedByOtherPlayer = playerOwns (otherPlayer player) (getPieceOnBoard currentBoard destinationPosition) 

-- Bishop movements
-- A bishop moves diagonally
checkBishopMove:: Rep.State -> Rep.Move -> Rep.Player -> Bool
checkBishopMove state move player
  | diagonalMove move && (otherPlayerPiece || emptyDestination) && not obstructed = True
  | otherwise = False
  where
    currentBoard = (board state)
    destinationPosition = (snd move)
    otherPlayerPiece = playerOwns (otherPlayer player) (getPieceOnBoard currentBoard destinationPosition)
    emptyDestination = emptyPosition currentBoard destinationPosition
    obstructed = diagonallyObstracted currentBoard move

-- Check queen move 
-- A queen can move just like a bishop or rook
checkQueenMove :: Rep.State -> Rep.Move -> Rep.Player -> Bool
checkQueenMove state move player 
 | diagonalMove move  = checkBishopMove state move player
 | (horizontalMove move || verticalMove move ) =  checkRookMove state move player
 | otherwise = False

 -- ##### Movements ####
-- Forward move
forwardMove:: Rep.Player -> Rep.Move -> Bool
forwardMove (Rep.Robot _) ((fromRank, fromFile), (toRank, toFile)) = (toRank < fromRank)
forwardMove (Rep.Human _) ((fromRank, fromFile), (toRank, toFile)) = (toRank > fromRank)
forwardMove _ _ = False

-- Vertical move 
verticalMove :: Rep.Move -> Bool
verticalMove ((fromRank, fromFile), (toRank, toFile)) = (fromRank /= toRank) && (fromFile == toFile)

-- Horizontal move
horizontalMove:: Rep.Move -> Bool
horizontalMove ((fromRank, fromFile), (toRank, toFile)) = fromRank == toRank && fromFile /= toFile

-- Diagonal movement
diagonalMove :: Rep.Move -> Bool
diagonalMove ((fromRank, fromFile), (toRank, toFile)) = 
  abs (fromRank - toRank)  == abs (fromFile - toFile)

-- Knight move(L o 7) move 
knightMove :: Rep.Move -> Bool
knightMove ((fromRank, fromFile), (toRank, toFile))
  | (rankDistance == 2 && fileDistance == 1) || (rankDistance == 1 && fileDistance  == 2) = True
  | otherwise = False
  where rankDistance = abs (toRank - fromRank)
        fileDistance = abs (toFile - fromFile)
-- Positions moved - in both directions 
stepsMoved:: Rep.Move -> Step
stepsMoved ((fromRank, fromFile), (toRank, toFile)) = 
  (abs $ fromRank - toRank, abs $ fromFile - toFile)

-- ########## check for obstruction
-- Is a piece obstructed horizontally?
horizontallyObstructed :: Rep.Board -> Rep.Move -> Bool
horizontallyObstructed board ((fromRank, fromFile), (toRank, toFile)) 
 | fromFile < toFile =  False `elem` (map (emptyPosition board) (zip [fromRank, fromRank ..] [(fromFile+1) .. (toFile-1)]))
 | fromFile > toFile =  False `elem` (map (emptyPosition board) (zip [fromRank, fromRank ..] [(fromFile-1), (fromFile-2) .. (toFile+1)]))
 | otherwise = False -- This may never happen

-- Is a piece obstructed vertically?
verticallyObstructed :: Rep.Board -> Rep.Move -> Bool
verticallyObstructed currentBoard ((fromRank, fromFile), (toRank, toFile)) 
 | fromRank < toRank = False `elem` (map (emptyPosition currentBoard) (zip [(fromRank+1) .. (toRank-1)] [toFile, toFile ..]))
 | fromRank > toRank = False `elem` (map (emptyPosition currentBoard) (zip [(fromRank-1),(fromRank-2)  .. (toRank+1)] [toFile, toFile ..]))
 | otherwise = True

-- Is piece diagonally obstructed??
diagonallyObstracted:: Rep.Board -> Rep.Move -> Bool
diagonallyObstracted board ((fromRank, fromFile), (toRank, toFile))
 | topRight = False `elem` map (emptyPosition board) (zip [fromRank-1, fromRank-2 .. toRank+1] [fromFile+1 .. toFile-1])
 | topLeft = False `elem` map (emptyPosition board) (zip [fromRank-1, fromRank-2 .. toRank+1] [fromFile-1, fromFile-2 .. toFile+1])
 | bottomLeft = False `elem` map (emptyPosition board) (zip  [fromRank+1 .. toRank-1][fromFile-1,fromFile-2 .. toFile+1])
 | bottomRight = False `elem` map (emptyPosition board)  (zip  [fromRank+1 .. toRank-1][fromFile+1 .. toFile-1])
 | otherwise = True
 where topRight = (fromRank > toRank) && (fromFile < toFile)
       bottomLeft = (fromRank < toRank) && (fromFile > toFile)
       topLeft = (fromRank > toRank) && (fromFile > toFile)
       bottomRight = (fromRank < toRank) && (fromFile < toFile)
--- ########### Check if a position is under attack
  -- Position under attack 
positionUnderAttack:: Rep.State -> Rep.Position -> Rep.Player -> [Rep.Move]
positionUnderAttack state position player = posUnderAttackByKnight state position player ++  
                                            posUnderAttackDiagonally  state position player ++
                                            posUnderAttackHorizontally state position player ++
                                            posUnderAttackVertically state position player

-- A knight attacks in an L or 7 shape
-- Relative to a position, it can attack in L or 7 shape
-- These attacking positions are encoded in the list knightAttackingRelativePos 
-- If a night is in any of these positions relative to the position given, then the position is under attack
posUnderAttackByKnight :: Rep.State -> Rep.Position -> Rep.Player -> [Rep.Move]
posUnderAttackByKnight state (rank,file) player = map (\(_,pos,_,_,_) -> (pos, (rank,file))) (filter knight  validAttackingPieces)
 where proposedKnightPositions = map (\(r,f) -> (r + rank, f + file)) knightAttackingRelativePos -- calculate attacking knight position relative to the position given
       validAttackingPieces = filter (playerOwns player) (map (getPieceOnBoard (board state)) (filter checkValidPosition proposedKnightPositions))
       knightAttackingRelativePos = [(-2,1), (-2,-1), (2,1), (2,-1),  (-1,2), (-1,-2), (1,2), (1,-2)]
-- Position under attack diagonally
posUnderAttackDiagonally :: Rep.State -> Rep.Position -> Rep.Player -> [Rep.Move]
posUnderAttackDiagonally state position player = validAttackingPieces
 where possibleAttackingPieces = filter (playerOwns  player) (map (getPieceOnBoard (board state)) (diagonalPositions position))
       -- Piece that make valid moves
       validAttackingPieces = filter (\move -> (checkValidPosition (fst move))) $  map (\piece -> if validMove state piece ((getPiecePosition piece), position)  player then ((getPiecePosition piece), position) else ((-1,-1), position)) possibleAttackingPieces
-- Position under attack horizontally
posUnderAttackHorizontally :: Rep.State -> Rep.Position -> Rep.Player -> [Rep.Move]
posUnderAttackHorizontally state  (rank, file) player = validAttackingMoves
  where possibleAttackPieces = filter (playerOwns player) (map (getPieceOnBoard (board state)) ((zip [rank,rank .. ] [file - 1,file - 2 .. 0]) ++ (zip [rank,rank .. ] [file + 1 .. 7])))
        validAttackingMoves = filter (\move -> checkValidPosition (fst move) && checkValidPosition (snd move)) $ map (\piece -> if (validMove state piece ((getPiecePosition piece), (rank, file))  player) then ((getPiecePosition piece), (rank, file)) else ((-1,-1),(rank, file))) possibleAttackPieces

-- Position under attack vertically
posUnderAttackVertically:: Rep.State -> Rep.Position -> Rep.Player -> [Rep.Move]
posUnderAttackVertically state  (rank, file) player = validAttackingMoves
  where possibleAttackPieces = filter (playerOwns  player) (map (getPieceOnBoard (board state)) ((zip [rank+1 .. 7] [file,file..]) ++ (zip [rank-1, rank-2 .. 0] [file, file .. ])))
        validAttackingMoves =filter (\move -> checkValidPosition (fst move) && checkValidPosition (snd move)) $ map (\piece -> if (validMove state piece ((getPiecePosition piece), (rank, file))  player) then ((getPiecePosition piece), (rank, file)) else ((-1,-1),(rank, file))) possibleAttackPieces

-- ###  Generate positions
-- Generate all possible diagonal pieces relative to a position (with that position excluded)
diagonalPositions:: Rep.Position -> [Rep.Position]
diagonalPositions (rank, file) =
   filter checkValidPosition (topLeft ++ topRight ++ bottomLeft ++ bottomRight)
  where topLeft = zip [rank-1 ,(rank-2) .. 0] [file-1, (file-2) .. 0] -- Generate one step away from current position
        topRight = zip [rank-1, (rank-2) .. 0] [(file+1) .. 7]
        bottomLeft = zip [(rank + 1) .. 7] [file-1, (file-2) ..]
        bottomRight = zip [(rank + 1) .. 7] [(file+1)..7]

-- Generate horizontal positions -- useful for the rook and queen
horizontalPositions :: Rep.Position -> [Rep.Position]
horizontalPositions (rank, file) = filter checkValidPosition (zip [rank,rank..] [file-1,file-2 ..0]) ++ (zip [rank,rank..] [file+1..7])

-- Generate vertical positions -- useful for rook and queen
verticalPositions :: Rep.Position -> [Rep.Position]
verticalPositions (rank, file) = filter checkValidPosition (zip [rank-1,rank-2 .. 0][file,file..]) ++ (zip [rank+1 .. 7][file,file..])

-- pawn positions
pawnPositions :: Rep.Position -> Rep.Player -> [Rep.Position]
pawnPositions (rank, file) (Rep.Human _) = filter checkValidPosition [(rank+1, file), (rank+1,file+1), (rank+2, file)]
pawnPositions (rank, file) (Rep.Robot _) = filter checkValidPosition [(rank-1, file), (rank-1,file-1), (rank-2, file)]
pawnPositions pos player = []


-- Generate knight posittions
knightPositions :: Rep.Position -> [Rep.Position]
knightPositions (rank,file) = filter checkValidPosition (map (\(r,f) -> (r+rank, f+file)) [(-2,1), (-2,-1), (2,1), (2,-1),  (-1,2), (-1,-2), (1,2), (1,-2)])

-- Generate king positions 
kingPositions :: Rep.Position -> [Rep.Position]
kingPositions (rank, file) = filter checkValidPosition [(rank+1,file), (rank-1, file), (rank+1,file), (rank-1, file),(rank, file+2), (rank, file-2) ]

-- generate positions 
generatePositions :: Rep.Piece -> [Rep.Position]
generatePositions piece
 | pawn piece = pawnPositions position (Rep.piecePlayer piece)
 | rook piece = (verticalPositions position) ++ (horizontalPositions position)
 | knight piece = knightPositions position
 | bishop piece =  diagonalPositions position
 | queen piece = (verticalPositions position) ++ (horizontalPositions position) ++ (horizontalPositions position)
 | king piece = kingPositions position
 | otherwise = []
 where position = getPiecePosition piece

