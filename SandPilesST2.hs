{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module SandPilesST2 where

import Control.Monad
import Control.Monad.ST

import Data.Array.Unboxed
import Data.Array.ST

import qualified Data.ByteString as B
import Codec.BMP
import Data.Word

import System.IO (stdout, hFlush)
import Text.Printf
import System.CPUTime

type Pile     = UArray (Int,Int) Int
type STPile s = STUArray s (Int,Int) Int
type SinkF    = (Int,Int) -> Bool
type SpillF   = (Int,Int) -> [(Int,Int)]

colorScheme :: [[Word8]]
colorScheme =
  [ [0x3E, 0x21, 0xFF, 0xFF]
  , [0x50, 0x79, 0xE8, 0xFF]
  , [0x34, 0xBA, 0xFF, 0xFF]
  , [0x38, 0xE8, 0xDE, 0xFF]
  , [0x09, 0xE8, 0x81, 0xFF]
  , [0x16, 0xFF, 0x52, 0xFF] ]

showPile :: Pile -> String
showPile p = showPileRecurse p (0,0) (snd $ bounds p)

showPileRecurse :: Pile -> (Int,Int) -> (Int,Int) -> String
showPileRecurse p (x,y) (xb,yb)
  | (x,y) == (xb,yb) = show $ p!(x,y)
  | x == xb          = show (p!(x,y)) ++ "\n" ++ showPileRecurse p (0,y+1) (xb,yb)
  | otherwise        = show (p!(x,y)) ++ showPileRecurse p (x+1,y) (xb,yb)

putPile :: Pile -> IO ()
putPile = putStrLn . showPile

step :: (Int,Int) -> (Int,Int) -> (Int,Int)
step (x,y) (xb,yb)
  | x == xb && y == yb = (x,y)
  | x == xb            = (0,y+1)
  | otherwise          = (x+1,y)

backTrack :: (Int,Int) -> (Int,Int)
backTrack (x,y)
  | y == 0 && x == 0 = (0,0)
  | y == 0           = (x-1,0)
  | x == 0           = (x,y)
  | otherwise        = (x-1,y-1)

cullSpills :: (Int,Int) -> SinkF -> (Int,Int) -> Bool
cullSpills (xb,yb) sink (x,y) =
  x >= 0 && x <= xb && y >= 0 && y <= yb && not (sink (x,y))

spillList :: (Int,Int)
          -> (Int,Int)
          -> SpillF
          -> SinkF
          -> [(Int,Int)]
spillList (x,y) (xb,yb) spill sink =
  filter (cullSpills (xb,yb) sink) $ spill (x,y)

flatSTPile :: (Int,Int) -> Int -> ST s (STPile s)
flatSTPile xy = newArray ((0,0),xy)

fireNode :: (Int,Int)
         -> (Int,Int)
         -> Int
         -> SpillF
         -> SinkF
         -> STPile s
         -> ST s ()
fireNode xy xyb nmax spill sink stp = do
  n <- readArray stp xy
  writeArray stp xy (n-nmax)
  forM_ (spillList xy xyb spill sink) $ \xy' -> do
    n' <- readArray stp xy'
    writeArray stp xy' (n'+1)

centerTest :: Pile
centerTest = runSTUArray $ do
  arr <- flatSTPile (4,4) 4
  fireNode (0,0) (4,4) 4 squareSpill (const False) arr
  return arr

fireRecurse :: (Int,Int)
            -> (Int,Int)
            -> Int
            -> SpillF
            -> SinkF
            -> STPile s
            -> ST s ()
fireRecurse xy xyb nmax spill sink stp = do
  n <- readArray stp xy
  if n >= nmax
    then fireNode xy xyb nmax spill sink stp
      >> fireRecurse (backTrack xy) xyb nmax spill sink stp
    else if xy == xyb
      then return ()
      else fireRecurse (step xy xyb) xyb nmax spill sink stp

emptySink :: (Int,Int)
          -> (Int,Int)
          -> SinkF
          -> STPile s
          -> ST s ()
emptySink xy xyb sink stp
  | sink xy   = if xy == xyb
    then writeArray stp xy 0
    else writeArray stp xy 0 >> emptySink (step xy xyb) xyb sink stp
  | otherwise = if xy == xyb
    then return ()
    else emptySink (step xy xyb) xyb sink stp

fireAll :: Int
        -> SpillF
        -> SinkF
        -> STPile s
        -> ST s ()
fireAll nmax spill sink stp = do
  xyb <- snd <$> getBounds stp
  emptySink (0,0) xyb sink stp
  fireRecurse (0,0) xyb nmax spill sink stp

squareSpill :: SpillF
squareSpill (x,y) =
  [ (x-1,y)
  , (x+1,y)
  , (x,y-1)
  , (x,y+1) ]

diamondSink :: Int -> SinkF
diamondSink r (x,y) =
  x+y < r || x-y > r || y-x > r || x+y > 3*r

hexSpill :: SpillF
hexSpill (x,y) =
  [ (x,   y-1)
  , (x+1, y-1)
  , (x-1, y  )
  , (x+1, y  )
  , (x-1, y+1)
  , (x,   y+1) ]

hexSink :: Int -> SinkF
hexSink r (x,y) =
  x+y <r || x+y > 3*r

estimateRadius :: Int -> Int
estimateRadius n = ceiling . sqrt . fromIntegral $ div n 4

tallPile :: Int -> Pile
tallPile n = runSTUArray $ do
  let r = estimateRadius n
  result <- flatSTPile (2*r,2*r) 0
  writeArray result (r,r) n
  fireAll 4 squareSpill (const False) result
  return result

idPile :: (Int,Int) -> Pile
idPile xy = runSTUArray $ do
  p6     <- flatSTPile xy 6
  fireAll 4 squareSpill (const False) p6

  result <- mapArray (6-) p6
  fireAll 4 squareSpill (const False) result
  return  result

diamondPile :: (Int,Int) -> Int -> Pile
diamondPile xy r = runSTUArray $ do
  p6     <- flatSTPile xy 6
  fireAll 4 squareSpill (diamondSink r) p6

  result <- mapArray (6-) p6
  fireAll 4 squareSpill (diamondSink r) result
  return  result

hexPile :: (Int,Int) -> Int -> Pile
hexPile xy r = runSTUArray $ do
  p10    <- flatSTPile xy 10
  fireAll 6 hexSpill (hexSink r) p10

  result <- mapArray (10-) p10
  fireAll 6 hexSpill (hexSink r) result
  return result

packPile :: Pile -> [[Word8]] -> B.ByteString
packPile p colors = B.pack colorList
  where colorList = concat [colors!!(p!(i,j)) | i <- [0..x], j <- [0..y]]
        x         = fst . snd . bounds $ p
        y         = snd . snd . bounds $ p

imgPile :: Pile -> FilePath -> IO ()
imgPile p file = do
  let rgba = packPile p colorScheme
  let bmp  = packRGBA32ToBMP ((1+) . snd . snd . bounds $ p) ((1+) . fst . snd . bounds $ p) rgba
  writeBMP file bmp
  return ()

makeNameId :: (Int,Int) -> String
makeNameId (x,y) = "id-" ++ show x ++ "-" ++ show y ++ ".bmp"

makeNameTall :: Int -> String
makeNameTall n = "tall-" ++ show n ++ ".bmp"

timeComputation :: IO () -> IO Double
timeComputation f = do
  start <- getCPUTime
  f
  end   <- getCPUTime
  return $ fromIntegral (end - start) / (10^12)

main :: IO ()
main = do
  diff <- timeComputation $ imgPile (diamondPile (402,402) 201) "diamond-403.bmp"
  printf "Computation time: %0.3f sec\n" diff

-- main :: IO ()
-- main = do
--   putStr "Select mode -- (t)all or (i)dentity: "
--   hFlush stdout
--   m <- getLine
--   case m of
--     "i" -> do
--       putStr "Enter x dimension: "
--       hFlush stdout
--       rawx <- getLine
--       putStr "Enter y dimension: "
--       hFlush stdout
--       rawy <- getLine
--
--       let x = read rawx :: Int
--       let y = read rawy :: Int
--       let name = makeNameId (x,y)
--
--       diff <- timeComputation $ imgPile (idPile (x-1,y-1)) name
--
--       putStrLn $ "Output written to " ++ name
--       printf "Computation time: %0.3f sec\n" diff
--
--     "t" -> do
--       putStr "Enter grain number: "
--       hFlush stdout
--       rawn <- getLine
--
--       let n = read rawn :: Int
--       let name = makeNameTall n
--
--       diff <- timeComputation $ imgPile (tallPile n) name
--
--       putStrLn $ "Output written to " ++ name
--       printf "Computation time: %0.3f sec\n" (diff :: Double)
--
--     _   -> main
