module YicesSweeper where

import Control.Applicative
import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Ord
import System.IO
import System.Process
import qualified Data.Map as M
import qualified Data.Set as S

import Types

data YicesState = YicesState {
  yin, yout :: Handle,
  yph :: ProcessHandle,
  errThread :: ThreadId,
  yknowledge :: S.Set (Int, Int), -- Cells where Yices knows about the value
  ydefined :: S.Set (Int, Int),   -- Cells that Yices has defined as variables
  ylast :: IORef (Int, Int)       -- Mega-Hack
  }

newYicesState :: IO YicesState
newYicesState = do
  (yicesIn, yicesOut, yicesErr, yicesPH) <- runInteractiveCommand "yices"
  hSetBuffering yicesIn LineBuffering
  tid <- forkIO $ hGetContents yicesErr >>= writeFile "/tmp/yices.log"
  lastRef <- newIORef (0, 0)
  return YicesState {
    yin = yicesIn,
    yout = yicesOut,
    yph = yicesPH,
    errThread = tid,
    yknowledge = S.empty,
    ydefined = S.empty,
    ylast = lastRef
    }

-- All good things must come to an end
killYicesState :: YicesState -> IO ()
killYicesState y = do
  hClose (yin y)
  killThread (errThread y)
  waitForProcess (yph y)
  return ()

-- Tell Yices about the cells that have been newly revealed,
-- and update our record of what Yices knows accordingly
updateYices :: GameState -> YicesState -> IO YicesState
updateYices g y0 = do
  y1 <- foldM defineCell y0 (S.toList $ S.fromList [ c | c0 <- S.toList (_visibility g), c <- nearbyCells c0 ]
                             `S.difference` ydefined y0)
  foldM updateCell y1 (S.toList $ _visibility g `S.difference` yknowledge y1)
  where defineCell y c = do
          hPutStrLn (yin y) ("(define " ++ cellName c ++ " :: int)")
          hPutStrLn (yin y) ("(assert (and (<= 0 " ++ cellName c ++ ") (<= " ++ cellName c ++ " 1)))")
          return y { ydefined = S.insert c (ydefined y) }
        updateCell y c = do
          let n = tallyMines (_grid g) c
          hPutStrLn (yin y) (formulaString c n)
          return y { yknowledge = S.insert c (yknowledge y) }

nearbyCells :: (Int, Int) -> [(Int, Int)]
nearbyCells (cx,cy) = [ (x,y) | x <- [cx-1, cx, cx+1], y <- [cy-1, cy, cy+1] ]

cellName :: (Int, Int) -> String
cellName (cx,cy) = "x" ++ show cx ++ "y" ++ show cy

formulaString :: (Int, Int) -> Int -> String
formulaString c n = "(assert (and (= " ++ show n ++ " (+ " ++
                    unwords [ cellName c' | c' <- nearbyCells c ] ++
                    ")) (= 0 " ++ cellName c ++ ")))"

-- Ask Yices to prove something
queryYices :: GameState -> YicesState -> IO (Maybe ((Int, Int), Bool))
queryYices g y = do
  hPutStrLn (yin y) "(check)"
  cur <- hGetLine (yout y)
  appendFile "/tmp/yices.out" (cur ++ "\n")
  lastFound <- readIORef (ylast y)
  if cur /= "sat" then hPutStr stderr "help" >> return Nothing else
   -- Find squares we would like to know about
   let candidateCells0 =
         ydefined y
         `S.difference` (_visibility g `S.union` _markers g)
       candidateCells = sortBy (comparing (squareDist lastFound)) (S.toList candidateCells0)
       candidates = (,) <$> candidateCells <*> [False, True]
       go [] = -- Guess LOL
         case candidateCells of
          c : _ -> return (Just (c, False))
          []    -> return (Just ((0, 0), False))
       go ((c, b):cs) = do
         isSat <- checkYicesSquare y (c, b)
         if isSat then go cs else writeIORef (ylast y) c >> return (Just (c, not b))
   in go candidates

squareDist :: (Int, Int) -> (Int, Int) -> Int
squareDist (ax, ay) (bx, by) = max (abs (ax - bx)) (abs (ay - by))

-- Is the given extension of the theory satisfiable?
checkYicesSquare :: YicesState -> ((Int, Int), Bool) -> IO Bool
checkYicesSquare y (c, b) = do
  hPutStrLn (yin y) "(push)"
  hPutStrLn (yin y) $ "(assert (= " ++ cellName c ++ " " ++ (if b then "1" else "0") ++ "))"
  hPutStrLn (yin y) "(echo \"\\nsync\\n\")"
  hPutStrLn (yin y) "(check)"
  let loop = do
        l <- hGetLine (yout y)
        appendFile "/tmp/yices.out" (l ++ "\n")
        if l /= "sync" then loop
          else do
          l' <- hGetLine (yout y)
          appendFile "/tmp/yices.out" (l' ++ "\n")
          hPutStrLn (yin y) "(pop)"
          case l' of
           "sat" -> return True
           "unsat" -> return False
           other -> error $ "what's this: " ++ other
  loop
