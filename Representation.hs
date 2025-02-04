module Representation where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.ViewPort
import Data.Typeable
import qualified Data.List as DL
import Debug.Trace
import Graphics.Gloss.Interface.Environment

import Control.Monad (forM)

-- Chess assets borrowed from :: https://github.com/madnight/chessboard.git
-- Origin 
data Origin = Origin {x_origin:: Float, y_origin:: Float}deriving (Show)
-- Offset 
data Offset = Offset {x_offset:: Float, y_offset:: Float} deriving (Show)
-- Center
data Center = Center{x_axis:: Int, y_axis::Int} deriving (Show)
-- Screen dimensions
data Screen = Screen {width:: Int, height::Int} deriving (Show)
data PlayerColor = White | Black deriving (Eq, Show)
-- Player
data Player = Robot {color:: String} | Human {color:: String} | Unknown deriving (Eq, Show)
-- Piece Name
type PieceName = String
-- Has the piece moved
type Moved = Bool
-- Position being vacated by player 
type From = Position
-- Target position of the player
type To = Position
-- Move is current position and the target position of a piece on the board
type Move = (From, To)
-- Move Type

-- Position
-- data Position = Position {rank:: Int, file::Int} deriving(Show)
type Position = (Int, Int)
-- data Piece = Piece {name:: String, position:: Position, moved:: Bool, player:: Player, image:: Picture} deriving (Show)
type Piece = (PieceName, Position, Player, Moved, Picture)
-- Is this the first time the piece is moved?
hasPieceMoved:: Piece -> Bool
hasPieceMoved (p,_,_,moved,_) = moved
-- Owner of piece
piecePlayer :: Piece -> Player
piecePlayer (_,_,p,_,_) = p
-- Does a piece belong to a given player? 
playerOwns::Player -> Piece  -> Bool
playerOwns (Human player) (p,_,(Human hplayer),_,_)  = True
playerOwns (Robot player) (p,_,(Robot rplayer),_,_)  = True
playerOwns _ _ = False

-- A rank 
type Rank = Int
-- A board is an arrangement of list of pieces in ranks (8 ranks)
type Board = [(Rank, [Piece])]
-- Get Piece on a board
getPieceOnBoard:: Board -> Position -> Piece

getPieceOnBoard board (rank, file) =
    if rank > 7 || file > 7 then
     (snd (board !! rank)) !! file
    else (snd (board !! rank)) !! file

-- AI tree
-- Tree structure - keep root distinct from other nodes
data Tree = Node {  move:: Move,
                    current_player:: Player,
                    cache_player:: [Tree],
                    fitness:: Int,
                    subtree:: [Tree]}
            | Empty
             deriving (Show)
-- State is the Board Positions occupied by both players and the the current player
data State = State {
                    cache:: [Tree],
                    background:: Picture,
                    save:: Picture,
                    load:: Picture,
                    depth:: Int,
                    difficultyValue:: Picture,
                    origin:: Origin,
                    previousSelection:: Position,
                    offset :: Offset,
                    images :: [Picture],
                    whiteQueen:: Picture,
                    blackQueen:: Picture,
                    player:: Player,
                    center::Center,
                    history:: String,
                    board::  Board}
            | ChooseColor
            | EndState {background:: Picture, message:: Picture}deriving (Show)
-- Steps moved by a piece
type Step = (Int, Int)

-- Get player pieces
getPlayerPieces :: Board -> Player -> [Piece]
getPlayerPieces board player = flatBoard
  where flatBoard = filter (playerOwns player) (concat (map (\(rank, pieces) -> pieces) board))

-- Chess Pieces 
pawn (p,_,_,_, _) = p == "bP" || p == "wP"
rook (r,_,_,_, _) = r == "bR" || r == "wR"
knight (n,_,_,_, _) = n == "bN" || n == "wN"
bishop (b,_,_,_, _) = b == "bB" || b == "wB"
queen (q,_,_,_, _) = q == "bQ" || q == "wQ"
king (k,_,_,_, _) = k == "bK" || k == "wK"
emptyPiece (e,_,_,_,_) = e == "."
-- Get piece position
getPiecePosition :: Piece -> Position
getPiecePosition (_,pos,_,_,_) = pos
-- Empty Position?
emptyPosition:: Board -> Position -> Bool
emptyPosition board position = emptyPiece (getPieceOnBoard board position)
-- Get piece image 
getPiecePicture :: Piece -> Picture
getPiecePicture (_,_,_,_,img) = img
-- Players 
-- White player owns the pieces indicated in capital letters
whitePlayer = "white"
-- Black player is indicated by lowercase pieces 
blackPlayer = "black"
-- No player means that a position belongs to no player 
noplayer = "_"
-- Find out the your opponent
otherPlayer::Player -> Player
otherPlayer (Human "black") = Robot "white"
otherPlayer (Human "white") = Robot "black"
otherPlayer (Robot "black") = Human "white"
otherPlayer (Robot "white") = Human "black"
otherPlayer Unknown = Unknown


getScreenCenter :: Screen -> Center
getScreenCenter (Screen w h) = Center  (w `div` 2) (h `div`2)

window:: Display
window = FullScreen

boardSize:: Float
boardSize = 640

cellSize:: Float
cellSize = 80

backgroundColor:: Color
backgroundColor = white

-- Game pieces
whitePieceNames = ["wR", "wN", "wB", "wQ", "wK", "wB", "wN", "wR"]
whitePawnName = "wP"
blackPieceNames = ["bR", "bN", "bB", "bQ", "bK", "bB", "bN", "bR"]
blackPawnName = "bP"
noPiece = "."
emptyPieces = replicate 8 noPiece


makeRank:: PlayerColor -> Int -> [Picture] -> [Picture] -> Picture -> Picture -> (Rank, [Piece])
makeRank White rank whitePics blackPics whitePawnPic blackPawnPic
    | rank == 0 = (0 , (DL.zip5 whitePieceNames (zip [0,0..] [0..7]) (replicate 8 (Human whitePlayer)) (replicate 8 False) whitePics))
    | rank == 1 = (1, (DL.zip5 (replicate 8 whitePawnName) (zip [1,1..] [0..7])   (replicate 8 (Human whitePlayer)) (replicate 8 False) (replicate 8 whitePawnPic)))
    | rank == 6 = (6, (DL.zip5 (replicate 8 blackPawnName) (zip [6,6..] [0..7])  (replicate 8 (Robot blackPlayer)) (replicate 8 False) (replicate 8 blackPawnPic)))
    | rank == 7 = (7, (DL.zip5 blackPieceNames (zip [7,7..] [0..7]) (replicate 8 (Robot blackPlayer)) (replicate 8 False) blackPics))
    | otherwise = (rank, (DL.zip5 emptyPieces (zip [rank,rank..] [0..7]) (replicate 8 Unknown) (replicate 8 False) (replicate 8 blank)))

makeRank Black rank whitePics blackPics whitePawnPic blackPawnPic 
    | rank == 0 = ( 0,  (DL.zip5 blackPieceNames  (zip [0,0..] [0..7]) (replicate 8 (Human blackPlayer)) (replicate 8 False) blackPics))
    | rank == 1 = ( 1,  (DL.zip5 (replicate 8 blackPawnName) (zip [1,1..] [0..7])   (replicate 8 (Human blackPlayer)) (replicate 8 False) (replicate 8 blackPawnPic)))
    | rank == 6 = ( 6,  (DL.zip5 (replicate 8 whitePawnName) (zip [6,6..] [0..7])  (replicate 8 (Robot whitePlayer)) (replicate 8 False) (replicate 8 whitePawnPic)))
    | rank == 7 = ( 7,  (DL.zip5 whitePieceNames (zip [7,7..] [0..7]) (replicate 8 (Robot whitePlayer)) (replicate 8 False) whitePics))
    | otherwise = (rank,  (DL.zip5 emptyPieces (zip [rank,rank..] [0..7]) (replicate 8 Unknown) (replicate 8 False) (replicate 8 blank)))
-- The first rank will always face the human player and the last rank always on the AI side
-- This function takes the color chosen by the human player and generates the 8 ranks
initialState::  PlayerColor -> IO State
initialState humanPieceColor = do
    -- cabal run +RTS -S -N2 -I
    -- Get the screen dimensions
    (width, height) <- getScreenSize
    let screen = Screen width height
    let center =  getScreenCenter screen
    -- Frames per second
    let framesPerSecond = 30
    -- Load images to be used on the chess board
    background <- loadBMP "./assets/chess.bmp"
    whitePawn <- loadBMP "./assets/wP.bmp"
    blackPawn <- loadBMP "./assets/bP.bmp"
    whiteQueen <- loadBMP "./assets/wQ.bmp"
    blackQueen <- loadBMP "./assets/bQ.bmp"
    whitepics <- loadPictures whitePieceNames
    blackpics <- loadPictures blackPieceNames

    let loadBtn = translate (-600) 200 $ scale 0.25 0.25 $ Text "Load State"
    let saveBtn = translate (-600) 0 $ scale 0.25 0.25 $ Text "Save State"
    let difficultyLabel = translate 0 400 $ scale 0.25 0.25 $ Text "Difficulty: 3"
    let addDepthBtn = translate (400) 200 $ scale 0.25 0.25 $ Text "Increase Difficulty"
    let reduceDepthBtn = translate (400) 0 $ scale 0.25 0.25 $ Text "Reduce Difficulty"
    let allowSave = if humanPieceColor == White then blank else saveBtn
    -- Determine the AI color and human Color
    let human_color = if (humanPieceColor == White) then whitePlayer else blackPlayer

    -- Get screen center
    let boardCenterX =  fromIntegral $ x_axis center :: Float -- find the center of the board
    let boardCenterY =  fromIntegral $ y_axis center :: Float
    let originCenterX = boardCenterX - (boardSize / 2)

    -- Offsets come in handy when translating images
    let originCenterY = boardCenterY - (boardSize / 2)
    let boardOffsetX = (fromIntegral width) - originCenterX
    let boardOffsetY = (fromIntegral height) - originCenterY

    -- Keep the origin used for drawing and not the mathematical origin
    -- This is due to how gloss places elements
    let drawingOriginCenterX = (cellSize / 2 ) + originCenterX
    let drawingOriginCenterY = (cellSize/ 2 ) + originCenterY
    let translateToOriginX = (drawingOriginCenterX - boardCenterX)
    let translateToOriginY = (drawingOriginCenterY - boardCenterY)
    -- Process board ranks
    let boardRanks = [makeRank humanPieceColor rank whitepics blackpics whitePawn blackPawn | rank <- [0..7]]
    -- Process pieces 
    let boardpieces =  concat $ map (\(rNo, rankPieces) -> rankPieces) boardRanks
    -- Prepare images and translate them to fit nicely on the board
    let boardImages = [difficultyLabel] ++[ pictures[ background, allowSave , loadBtn, reduceDepthBtn, addDepthBtn]] ++  map (\piece -> translate (translateToOriginX + (cellSize * fromIntegral (snd (getPiecePosition piece))))
                                          (translateToOriginY +  (cellSize * fromIntegral (fst (getPiecePosition piece))))
                                           (getPiecePicture piece))
                                           boardpieces
    -- Return the state
    return $ State 
          [] -- Cache empty initially
          (pictures[ background, saveBtn, loadBtn, reduceDepthBtn, addDepthBtn])
          saveBtn
          loadBtn
          3 -- Initial depth
           difficultyLabel
          (Origin translateToOriginX translateToOriginY)
          ((-1), (-1)) -- This is used to know when the human player wants to make a move, -1,-1 means no piece selected yet
          (Offset boardOffsetX boardOffsetY)
          boardImages
          whiteQueen
          blackQueen
          (Human human_color)
          center
          human_color
          boardRanks


-- -- Load picture
loadPictures::[String] -> IO [Picture]
loadPictures names = do
    let file_names = map (\name -> "./assets/" ++ name ++ ".bmp") names
    forM file_names loadBMP




