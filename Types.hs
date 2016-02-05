module Types where

import Data.Set (Set)

type Grid = [[Cell]]
data Cell = Empty | Mine deriving Eq

type Visibility = Set Position
type Markers    = Set Position
type Position   = (Int, Int)
type Score      = Int
data PlayState  = Alive | Dead deriving Eq
type Panel      = (Position, Position) -- The panel is used as limits for recursing down empty cells (It is supposed to be bigger than the terminal)
data Option     = Adventure | AutoOpen | Density Int deriving (Eq, Ord, Show)
type Options    = Set Option

data Move       = Up | Down | Left | Right
data GameState  = GameState
    {
        _grid       :: Grid,
        _visibility :: Visibility,
        _markers    :: Markers,
        _position   :: Position,
        _score      :: Score,
        _highscore  :: Score,
        _playState  :: PlayState,
        _panel      :: Panel,
        _options    :: Options,
        _autopilot  :: Bool
    }


-- List indices are like this: [0, 1, -1, 2, -2..]
getIndex :: [a] -> Int -> a
getIndex l i
    | i <= 0    = l!!(-2*i)
    | otherwise = l!!(2*i-1)

getCell :: Grid -> Position -> Cell
getCell grid (x, y) = getIndex (getIndex grid x) y

surroundingPositions :: Position -> [Position]
surroundingPositions (x, y) = [(i, j) | i<-[x-1..x+1], j<-[y-1..y+1], x /= i || y /= j]

tallyMines :: Grid -> Position -> Int
tallyMines grid pos = length $ filter (==Mine) $ map (getCell grid) (surroundingPositions pos)

