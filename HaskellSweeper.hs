{-# LANGUAGE LambdaCase #-}

module Main(main) where

import Data.Char (toLower, isAlphaNum)
import Data.List (intercalate, foldl')
import Data.Set (Set, empty, insert, delete, member, size, toList)
import Prelude hiding (Either(..))
import qualified Prelude as P
import System.Environment (getArgs)
import System.IO.Error (tryIOError)
import qualified System.IO.Strict as S
import System.Random (StdGen, getStdGen, randomRs, randoms, mkStdGen)
import UI.NCurses (Update,  Window, Curses, Color(..), ColorID, Event(..), Key(..), moveCursor, setColor, drawString, drawLineH, runCurses, setEcho, defaultWindow, newColorID, updateWindow, windowSize, glyphLineH, render, getEvent)
import Control.Monad.IO.Class (liftIO)

import Types
import YicesSweeper

inBounds :: Position -> Panel -> Bool
inBounds (x, y) ((a, b), (c, d)) = a <= x && x <= c && b <= y && y <= d

tallyMarkers :: Markers -> Position -> Int
tallyMarkers markers pos = length $ filter (\m -> member m markers) (surroundingPositions pos)

showGrid :: GameState -> Panel -> Position -> [ColorID] -> Update ()
showGrid gamestate ((left, top), (right, bottom)) (sx, sy) pal = sequence_ [do moveCursor (toInteger $ y - sy) (toInteger $ x - sx); showCell gamestate (x,y) pal | x<-[left..right], y<-[top..bottom]]

showCell :: GameState -> Position -> [ColorID] -> Update ()
showCell GameState{_grid=grid, _visibility=vis, _markers=mar, _playState=playstate, _position=position} pos pal
    | pos == position      = do setColor $ pal!!0; drawString "@"
    | member pos mar       = do markerColor playstate currentCell; drawString "~";
    | playstate == Dead &&
      currentCell == Mine  = drawMine
    | member pos vis       = showCell' currentCell (tallyMines grid pos)
    | otherwise            = do setColor $ pal!!0; drawString " "
    where
        currentCell :: Cell
        currentCell = getCell grid pos

        showCell' :: Cell -> Int -> Update ()
        showCell' Mine  _ = drawMine
        -- showCell' Empty 0 = do setColor $ pal!!0; drawString "•";
        showCell' Empty t = do setColor $ pal!!t; drawString ["##.~PmmX{" !! t]; -- ["77.~PwDX{" !! t];

        drawMine :: Update ()
        drawMine = do setColor $ pal!!8; drawString "X";

        markerColor :: PlayState -> Cell -> Update ()
        markerColor Dead Empty = setColor $ pal!!2
        markerColor _    _     = setColor $ pal!!8

randomGrid :: StdGen -> Int -> Grid
randomGrid gen den = [map (\n -> if n<den then Mine else Empty) $ randomRs (0, 99 :: Int) (mkStdGen g) | g<-(randoms gen) :: [Int]]

createGameStates :: StdGen -> Options -> Score -> [GameState]
createGameStates gen opts highscore =  map (\g -> GameState 
    {
        _grid       = randomGrid (mkStdGen g) (density $ toList opts),
        _visibility = empty,
        _markers    = empty,
        _position   = (0, 0),
        _score      = 0,
        _highscore  = highscore,
        _playState  = Alive,
        _panel      = ((-150, -50), (150, 50)),
        _options    = opts,
        _autopilot  = True
    }) ((randoms gen) :: [Int])
    where
        density :: [Option] -> Int
        density []            = 20
        density (Density x:_) = x
        density (_:xs)        = density xs

argsToOptions :: [String] -> Options
argsToOptions []               = empty
argsToOptions ("auto":xs)      = insert AutoOpen $ argsToOptions xs
argsToOptions ("adventure":xs) = insert Adventure $ argsToOptions xs
argsToOptions ("density":x:xs) = insert (Density $ read x) $ argsToOptions xs
argsToOptions (_:xs)           = argsToOptions xs

highscorePath :: Options -> FilePath
highscorePath options = intercalate "_" $ ".highscore" : (map (filter isAlphaNum . show) $ toList options)

readHighscore :: Options -> IO Score
readHighscore options = do
    strOrExc <- tryIOError $ S.readFile $ highscorePath options
    let
        getScore :: [String] -> Score
        getScore []     = 0
        getScore (x:_) = read $ last $ words x

        highscore = case strOrExc of
            P.Left  _        -> 0
            P.Right contents -> getScore $ lines contents

    return highscore

writeHighscore :: Options -> Score -> IO ()
writeHighscore options score = writeFile (highscorePath options) (show score)

main :: IO ()
main = do
    gen <- getStdGen
    args <- getArgs
    let options = argsToOptions $ map (map toLower) args
    highscore <- readHighscore options

    new_highscore <- runCurses $ do
        setEcho False
        w <- defaultWindow
        palette <- sequence
            [
{-
                newColorID ColorBlue    ColorDefault 1,
                newColorID ColorWhite   ColorDefault 2,
                newColorID ColorYellow  ColorDefault 3,
                newColorID ColorGreen   ColorDefault 4,
                newColorID ColorMagenta ColorDefault 5,
                newColorID ColorCyan    ColorDefault 6,
                newColorID ColorBlack   ColorDefault 7,
                newColorID ColorRed     ColorDefault 8,
                newColorID ColorRed     ColorDefault 9
-}
{-
-- st_ colors
                newColorID ColorBlack   ColorDefault 1,
                newColorID (Color 8)    ColorDefault 2,
                newColorID ColorBlue    ColorDefault 3,
                newColorID ColorCyan    ColorDefault 4,
                newColorID (Color 12)   ColorDefault 5,
                newColorID (Color 14)   ColorDefault 6,
                newColorID ColorYellow  ColorDefault 7,
                newColorID (Color 9)    ColorDefault 8,
                newColorID ColorRed     ColorDefault 9
-}
-- shoals colors
                newColorID ColorWhite   ColorDefault 1,
                newColorID ColorYellow  ColorDefault 2,
                newColorID ColorYellow  ColorDefault 3,
                newColorID (Color 12)   ColorDefault 4,
                newColorID ColorGreen   ColorDefault 5,
                newColorID ColorCyan    ColorDefault 6,
                newColorID (Color 11)   ColorDefault 7,
                newColorID (Color 9)    ColorDefault 8,
                newColorID ColorBlue    ColorDefault 9
{-
-- swamp colors
                newColorID ColorYellow  ColorDefault 1,
                newColorID ColorGreen   ColorDefault 2,
                newColorID ColorYellow  ColorDefault 3,
                newColorID (Color 12)   ColorDefault 4,
                newColorID ColorWhite   ColorDefault 5,
                newColorID ColorYellow  ColorDefault 6,
                newColorID (Color 10)   ColorDefault 7,
                newColorID ColorGreen   ColorDefault 8,
                newColorID ColorBlue    ColorDefault 9
-}
            ]

        let
            restartLoop :: (GameState -> YicesState -> Curses (Score, Bool)) -> [GameState] -> Curses Score
            restartLoop f (g:ng:gs) = do
                ys <- liftIO newYicesState
                quit <- f g ys
                liftIO $ killYicesState ys
                case quit of
                    (hs, True)  -> restartLoop f (ng{_highscore=hs}:gs)
                    (hs, False) -> return hs

        restartLoop (doUpdate w palette) (createGameStates gen options highscore)

    writeHighscore options new_highscore


doUpdate :: Window -> [ColorID] -> GameState -> YicesState -> Curses (Score, Bool)
doUpdate w palette g@GameState{_position=(x, y), _score=score, _highscore=highscore, _playState=playstate, _options=opts} ys = do
    updateWindow w $ do
        (sizeY, sizeX) <- windowSize
        let (sizeX', sizeY') = (fromInteger sizeX, fromInteger sizeY)
        let topLeft@(left, top) = (x - (div sizeX' 2), y - (div sizeY' 2))
        let bottomRight = (left + sizeX' - 1, top + sizeY' - 3)
        let panel = (topLeft, bottomRight)

        moveCursor 0 0
        showGrid g panel (left, top) palette
        moveCursor (sizeY - 2) 0
        setColor $ if _autopilot g then palette!!3 else palette!!2
        drawLineH (Just glyphLineH) sizeX
        moveCursor (sizeY - 1) 0
        setColor $ palette!!1
        drawString $ take (sizeX'-1) $ concat [show o ++ " | " | o <- toList opts ] ++ (case playstate of
            Alive -> "Score: " ++ show score
            Dead  -> "Game over! Your score is: " ++ show score)
          ++ " | Highscore is: " ++ show highscore ++ repeat ' '
        moveCursor (div sizeY 2) (div sizeX 2)
    render
    inputUpdate w palette g ys

inputUpdate :: Window -> [ColorID] -> GameState -> YicesState -> Curses (Score, Bool)
inputUpdate w palette g ys = do
    getEvent w (Just 30) >>= maybe
        (if _autopilot g
         then autopilot
         else doUpdate w palette g ys)
        (\case
            EventCharacter 'q' -> return (_highscore g, False)
            EventCharacter 'Q' -> return (_highscore g, False)
            EventCharacter 'r' -> return (_highscore g, True)
            EventCharacter 'R' -> return (_highscore g, True)
            key                -> doUpdate w palette (stepGameWorld key g) ys)
  where autopilot
          | _playState g == Dead = return (_highscore g, True)
          | _playState g == Alive
            = do
              ys' <- liftIO (updateYices g ys)
              res <- liftIO (queryYices g ys')
              let g' = case res of
                         Nothing -> g { _autopilot = False }
                         Just ((cx, cy), b) ->
                           let (px, py) = _position g
                               keys = waltz (cx - px) (cy - py) ++ [if b then 'm' else ' ']
                               waltz a b
                                 | a < 0 = 'a' : waltz (a+1) b
                                 | a > 0 = 'd' : waltz (a-1) b
                                 | b < 0 = 'w' : waltz a (b+1)
                                 | b > 0 = 's' : waltz a (b-1)
                                 | otherwise = []
                           in foldl' (\gs k -> stepGameWorld (EventCharacter k) gs) g keys
              doUpdate w palette g' ys'

movePosition :: GameState -> Move -> GameState
movePosition g@GameState{_grid=grid, _visibility=vis, _position=(x, y), _panel=((left, top), (right, bottom))} move =
    newGameState{_position = (x+dx, y+dy)}
    where
        (dx, dy) = case move of
            Up    -> ( 0, -1)
            Down  -> ( 0,  1)
            Left  -> (-1,  0)
            Right -> ( 1,  0)

        newPanel :: Panel
        newPanel = ((left+dx, top+dy), (right+dx, bottom+dy))

        cells :: [Position]
        cells = concatMap surroundingPositions $ filter (\p -> (tallyMines grid p == 0) && (member p vis)) $ case move of
            Up    -> [(i, top)    | i <- [left..right]]
            Down  -> [(i, bottom) | i <- [left..right]]
            Left  -> [(left, i)   | i <- [top..bottom]]
            Right -> [(right, i)  | i <- [top..bottom]]

        newGameState = foldl getEmptyCells g{_panel=newPanel} cells

getEmptyCells :: GameState -> Position -> GameState
getEmptyCells g@GameState{_grid=grid, _panel=panel, _visibility=vis, _markers=markers, _score=score, _highscore=highscore} pos
    | not (inBounds pos panel)
      || member pos vis
      || member pos markers    = g
    | t > 0                    = g{_visibility=newVis, _score=score + t, _highscore=max highscore (score + t)}
    | otherwise                = foldl getEmptyCells g{_visibility=newVis} (surroundingPositions pos)
    where
        t :: Int
        t = tallyMines grid pos

        newVis :: Visibility
        newVis = insert pos vis


isSatisfied :: GameState -> Position -> Bool
isSatisfied GameState{_grid=grid, _markers=markers} p = tallyMines grid p == tallyMarkers markers p

updateMarker :: GameState -> Position -> GameState
updateMarker g@GameState{_visibility=vis} pos
    | size vis == size (_visibility newGameState) = newGameState
    | otherwise                                   = updateMarker newGameState pos    
        where
            cells :: [Position]
            cells = concatMap surroundingPositions $ filter (\p -> (member p vis) && (isSatisfied g p)) (surroundingPositions pos)

            newGameState :: GameState
            newGameState = foldl clickCellPos g cells

placeMarker :: GameState -> GameState
placeMarker g@GameState{_markers=markers, _visibility=vis, _position=pos, _options=opts}
    | member pos vis       = g
    | member pos markers   = g{_markers=(delete pos markers)}
    | member AutoOpen opts = updateMarker newGameState pos 
    | otherwise            = newGameState
    where
        newGameState :: GameState
        newGameState = g{_markers=(insert pos markers)}

clickCell :: GameState -> GameState
clickCell g = clickCellPos g (_position g)

clickCellPos :: GameState -> Position -> GameState
clickCellPos g@GameState{_grid=grid, _visibility=vis, _markers=markers} pos
    | member pos markers       = g
    | member pos vis           = updatedMarkers
    | getCell grid pos == Mine = g{_visibility=(insert pos vis), _playState=Dead}
    | otherwise                = getEmptyCells g pos
    where
        updatedMarkers :: GameState
        updatedMarkers = if (isSatisfied g pos)
            then foldl clickCellPos g (filter (\p -> not $ member p vis) (surroundingPositions pos))
            else g

stepGameWorld :: Event -> GameState -> GameState
stepGameWorld (EventSpecialKey KeyUpArrow)    gamestate                    = movePosition gamestate Up
stepGameWorld (EventCharacter  'w')           gamestate                    = movePosition gamestate Up
stepGameWorld (EventCharacter  'W')           gamestate                    = movePosition gamestate Up
stepGameWorld (EventSpecialKey KeyDownArrow)  gamestate                    = movePosition gamestate Down
stepGameWorld (EventCharacter  's')           gamestate                    = movePosition gamestate Down
stepGameWorld (EventCharacter  'S')           gamestate                    = movePosition gamestate Down
stepGameWorld (EventSpecialKey KeyLeftArrow)  gamestate                    = movePosition gamestate Left
stepGameWorld (EventCharacter  'a')           gamestate                    = movePosition gamestate Left
stepGameWorld (EventCharacter  'A')           gamestate                    = movePosition gamestate Left
stepGameWorld (EventSpecialKey KeyRightArrow) gamestate                    = movePosition gamestate Right
stepGameWorld (EventCharacter  'd')           gamestate                    = movePosition gamestate Right
stepGameWorld (EventCharacter  'D')           gamestate                    = movePosition gamestate Right
stepGameWorld _                               g@GameState{_playState=Dead} = g -- If not playing, player can move around but not "play" (open cells)
stepGameWorld (EventCharacter 'm')            gamestate                    = placeMarker  gamestate
stepGameWorld (EventCharacter 'M')            gamestate                    = placeMarker  gamestate
stepGameWorld (EventCharacter 'e')            gamestate                    = placeMarker  gamestate
stepGameWorld (EventCharacter 'E')            gamestate                    = placeMarker  gamestate
stepGameWorld (EventCharacter ' ')            gamestate                    = clickCell    gamestate
stepGameWorld (EventSpecialKey KeyEnter)      gamestate                    = clickCell    gamestate
stepGameWorld (EventCharacter '\t')           gamestate                    = gamestate { _autopilot = not (_autopilot gamestate) }
stepGameWorld _                               gamestate                    = gamestate

-- animate :: GameState -> GameState
-- animate = animate' 10

-- animate' :: Int -> GameState -> GameState
-- animate' 0 g = g
-- animate' n g@GameState{_playState=Dead, _position=(x, y), _grid=grid, _visibility=vis} = animate' (n-1) revealOneMine
--     where
--         revealOneMine :: GameState
--         revealOneMine = g{_visibility=insert minePos vis}

--         minePos :: Position
--         minePos = head $ filter (\p -> (not $ member p vis) && (getCell grid p == Mine)) $ [(x+i, y+j) | d<-[1..], i<-[-d..d], j<-[-d..d]]
-- animate' _ gamestate                  = gamestate
