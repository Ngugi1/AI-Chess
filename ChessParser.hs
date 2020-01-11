module ChessParser where
import qualified Data.Char
import Debug.Trace
import Parser
-- Destination of a piece
data Square = Square {sfile:: Char, srank:: Int} deriving (Show)
-- Piece and pawn disambiguation
data DISAMB = DIS {file_rank:: Square}
              | PDIS {dfile:: Char} deriving (Show)
-- Nature of a step
data Step = Move {dis:: DISAMB, square:: Square}
            | PawnMove {square:: Square, promote:: Bool}
            | PawnCapture {pdis:: DISAMB , square:: Square, promote:: Bool}
            | LastStep {human_color:: String}
            | Win {player:: String}
            | Stalemate
            | Checkmate
            | KSCastling
            | QSCastling deriving (Show)
-- Last step
lastStepBlack:: Parser Step
lastStepBlack = do
   keyword "black"
   return $ LastStep  "black"

lastStepWhite:: Parser Step
lastStepWhite = do
   keyword "white"
   return $ LastStep  "white"
-- Parse checkmate
checkmate:: Parser Step
checkmate = do
    keyword "+"
    return Checkmate 
-- Parse checkmate
stalemate:: Parser Step
stalemate = do
    keyword "#"
    return Stalemate
-- Piece Move
pieceMove :: Parser Step
pieceMove = do
    fromFile <- blank >> sat Data.Char.isAlpha
    fromRank <- blank >> sat Data.Char.isDigit
    sat (\c -> Data.Char.isAlpha c && Data.Char.isUpper c)
    toFile <- blank >> sat Data.Char.isAlpha
    toRank <- blank >> sat Data.Char.isDigit
    return $ Move (DIS (Square fromFile (Data.Char.digitToInt fromRank))) (Square toFile (Data.Char.digitToInt toRank))

-- Piece move capture 
-- Piece Move
pieceMoveCapture :: Parser Step
pieceMoveCapture = do 
    fromFile <- blank >> sat Data.Char.isAlpha
    fromRank <- blank >> sat Data.Char.isDigit
    sat (\c -> Data.Char.isAlpha c && Data.Char.isUpper c)
    keyword "x"
    toFile <- blank >> sat Data.Char.isAlpha
    toRank <- blank >> sat Data.Char.isDigit
    return $ Move (DIS (Square fromFile (Data.Char.digitToInt fromRank))) (Square toFile (Data.Char.digitToInt toRank))
-- Pawn move
pawnMoveNoPromote:: Parser Step
pawnMoveNoPromote = do
    toFile <- blank >> sat Data.Char.isAlpha -- Read to File
    toRank <- blank >> sat Data.Char.isDigit -- Eat the rank
    keyword "empty"
    return $ PawnMove (Square toFile (Data.Char.digitToInt toRank)) False

-- Pawn Capture and Promote Parser
pawnMovePromote:: Parser Step
pawnMovePromote = do
    toFile <- blank >> sat Data.Char.isAlpha -- Read to File
    toRank <- blank >> sat Data.Char.isDigit -- Eat the rank
    prom <- keyword "= Q"
    return $ PawnMove (Square toFile (Data.Char.digitToInt toRank)) True

pawnCapturePromote :: Parser Step
pawnCapturePromote = do
    pdis <- blank >> sat Data.Char.isAlpha -- Pawn disambiguation
    keyword "x" -- Eat a cross
    toFile <- blank >>  sat Data.Char.isAlpha
    toRank <- blank >> sat Data.Char.isDigit
    keyword "= Q"
    return $ PawnCapture (PDIS pdis) (Square toFile (Data.Char.digitToInt toRank)) True

pawnCaptureNoPromote :: Parser Step
pawnCaptureNoPromote = do
    pdis <- blank >> sat Data.Char.isAlpha -- Pawn disambiguation
    keyword "x" -- Eat a cross
    toFile <- blank >>  sat Data.Char.isAlpha
    toRank <- blank >> sat Data.Char.isDigit
    keyword "empty"
    return $ PawnCapture (PDIS pdis) (Square toFile (Data.Char.digitToInt toRank)) False

kingSideCastling:: Parser Step
kingSideCastling =  do
    keyword "0-0" >> return KSCastling

queenSideCastling:: Parser Step
queenSideCastling = do
    keyword "0-0-0" >> return QSCastling
parsers = oneof [stalemate, checkmate, lastStepWhite, lastStepBlack, queenSideCastling, kingSideCastling, pieceMoveCapture, pieceMove, pawnMovePromote, pawnMoveNoPromote, pawnCapturePromote, pawnCaptureNoPromote]
parse str = apply (some parsers) str


