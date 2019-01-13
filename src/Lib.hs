{-# LANGUAGE BangPatterns #-}
module Lib where

import Graphics.UI.GLUT
import Control.Monad
import Data.Int
import Data.Complex
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import qualified Data.Map as Map

start = 5
iterations = 7
maxIterations = 30

x // y = fromIntegral x / fromIntegral y

-- Divides [a] into [[a], [a], ...] with each sublist of length n,
-- except the last sublist which has length <= n.
chunkify n [] = []
chunkify n xs = let (xs', rest) = splitAt n xs
                in xs' : chunkify n rest

-- Converts a coordinate in screen space to a vertex.
pix2vert (Size w h) (x, y) =
  Vertex2 ((3 // w * fromIntegral x) - 2.0) ((2 // h * fromIntegral y) - 1.0)

-- List of all of the vertices that represent screen pixels.
vertices :: IO [Vertex2 GLfloat]
vertices = get windowSize >>= (\x -> return $ vertices' x)

vertices' (Size w h) = [pix2vert (Size w h) (x, y) | x <- [0..w-1], y <- [0..h-1]]

-- Gets the color for a number of iterations.
color3 r g b = Color3 r g b

getcolor :: Int -> Color3 Float
getcolor numberOfIterations
  | inMandelbrotSet = color3 0 0 0
  | otherwise = color3 (escapeNumber*0.5) (escapeNumber * 0.5) escapeNumber
  where escapeNumber = numberOfIterations // iterations
        inMandelbrotSet = numberOfIterations == iterations

-- Returns the number of iterations <= the maximum iterations of the
-- Mandelbrot set at the given vertex.
mandel numberOfIterations (Vertex2 r i) =
  length . takeWhile (\z -> magnitude z <= 2) .
  take numberOfIterations $ iterate (\z -> z^2 + (r :+ i)) 0

mandel' numberOfIterations (Vertex2 r i) =
  length . takeWhile (\z -> magnitude z <= 2) .
  take numberOfIterations $ iterate (\z -> z^2 + (r :+ i)) 0

mandelMap :: (RealFloat a1, Fractional a2) => Vertex2 a1 -> Map.Map Int (Color3 a2)
mandelMap (Vertex2 r i) = Map.fromList iterList
  where mandelNumbers = iterate (\z -> z^2 + (r :+ i)) 0

        escapeVelocity numIter = length . takeWhile (\z -> magnitude z <= 2) .
                                 take numIter $ mandelNumbers

        --iterList = makeMap (\iter -> getColor (escapeVelocity iter) iter) [1..maxIterations]
        iterList = makeMap (\iter -> getColor (escapeVelocity iter) iter) [start..iterations]

        getColor escapedAt totalNumIters
          | inMandelbrotSet = color3 0 0 0
          | otherwise = color3 (escapeNumber*0.5) (escapeNumber * 0.5) escapeNumber
          where escapeNumber = escapedAt // totalNumIters
                inMandelbrotSet = escapedAt == totalNumIters

colorPoint :: RealFloat a => Int -> Vertex2 a -> Color3 Float
colorPoint numIterations = getcolor . (mandel numIterations)

-- plots one point.
-- drawVert v = do
--   color . getcolor $ mandel v
--   vertex v

drawVert' (point, color_) = do
  color color_
  vertex point

-- draws all the vertices in slices (to update the display while drawing).
-- display' chunks = do
  -- mapM_ render chunks
  -- displayCallback $= display
  -- where render points = do
          -- renderPrimitive Points (mapM_ drawVert points)
          -- flush

getColor :: (Map.Map (Vertex2 GLfloat) (Map.Map Int (Color3 Double))) -> Int -> Vertex2 GLfloat -> Color3 Double
getColor pointMap numIter vertex =
  case Map.lookup vertex pointMap of
    Just colorMap -> case Map.lookup numIter colorMap of
                       Just r -> r
                       Nothing -> Color3 0 0 0
    Nothing -> Color3 0 0 0

drawAll :: Int ->  Map.Map (Vertex2 GLfloat) (Map.Map Int (Color3 Double)) -> [Vertex2 GLfloat] -> IO()
drawAll count pointMap points = do
  render
  displayCallback $= (display (count + 1) pointMap)
  where
        render = do
          renderPrimitive Points (mapM_ drawVert' toDraw)
          flush

        toDraw :: [(Vertex2 GLfloat, Color3 Double)]
        toDraw = makeMap (getColor pointMap count) points

-- draws the whole fractal
display :: Int -> Map.Map (Vertex2 GLfloat) (Map.Map Int (Color3 Double)) -> IO ()
display 200 _ = do exitWith ExitSuccess
display num pointMap = do
  clear [ ColorBuffer ]
  -- displayCallback $= (vertices >>= display' . chunkify 256)
  displayCallback $= (vertices >>= (drawAll num pointMap))
  get currentWindow >>= postRedisplay

keyboard :: KeyboardMouseCallback
keyboard (Char keyPressed) Down _ _ = case keyPressed of
   ' '   -> postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ = return ()

mandelMain = do
  getArgsAndInitialize
  initialDisplayMode $= [ SingleBuffered, RGBMode]
  initialWindowSize $= Size 1200 1024
  initialWindowPosition $= Position 100 100
  createWindow "Mandelbrot"
  --fullScreen
  -- renderString Roman "Test"
  clearColor $= Color4 0 0 0 0
  matrixMode $= Projection
  loadIdentity
  ortho (-2) 1 (-1) 1 (-1) 1
  keyboardMouseCallback $= Just keyboard
  points <- vertices
  putStrLn "PreLoading.."
  let !preload = Map.fromList $ makeMap mandelMap points
  putStrLn "Loaded"
  displayCallback $= (display start preload)
  mainLoop

makeMap :: (a -> b) -> [a] -> [(a, b)]
makeMap fn lst = zip lst (map fn lst)
