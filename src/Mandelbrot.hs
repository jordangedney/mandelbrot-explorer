module Mandelbrot where

import Graphics.UI.GLUT
import Control.Monad
import Data.Int
import Data.Complex
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.IORef
import qualified Data.ByteString as B
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Image
import           System.Directory (removeFile)
import Data.List.Split (splitOn)

convert :: FilePath -> FilePath -> Bool -> IO ()
convert input output remove = do
  ppm <- readImageRGBA VU input
  writeImage output ppm
  when remove $ removeFile input

iterations = 40

x // y = fromIntegral x / fromIntegral y

-- Divides [a] into [[a], [a], ...] with each sublist of length n,
-- except the last sublist which has length <= n.
chunkify n [] = []
chunkify n xs = let (xs', rest) = splitAt n xs
                in xs' : chunkify n rest

-- Converts a coordinate in screen space to a vertex.
pix2vert (Size w h) (x, y) = Vertex2 ((3 // w * fromIntegral x) - 2.0)
                             ((2 // h * fromIntegral y) - 1.0)

-- List of all of the vertices that represent screen pixels.
vertices :: IO [Vertex2 GLfloat]
vertices = get windowSize >>= \(Size w h) ->
           return $ [pix2vert (Size w h) (x, y) | x <- [0..w-1], y <- [0..h-1]]

-- Gets the color for a number of iterations.
color3 r g b = Color3 r g b
getcolor :: Int -> Int -> Color3 Float
getcolor iterations iter
  | iter == iterations = color3 0.1 0.1 0.1
  | otherwise          = color3 (amt*0.5) (amt * 0.5) (amt*0.5)
  where amt = iter // iterations

-- Returns the number of iterations <= the maximum iterations of the
-- Mandelbrot set at the given vertex.
mandel numIterations horizontalShift verticalShift zoom (Vertex2 r i) =
  length . takeWhile (\z -> Data.Complex.magnitude z <= 2) .
  take numIterations $
  iterate (\z -> z^2 + (((r / zoom) + horizontalShift) :+ ((i / zoom) + verticalShift))) 0

-- plots one point.
drawVert iter horShift verShift zoom v = do
  horShift' <- get horShift
  verShift' <- get verShift
  zoom' <- get zoom
  color . (getcolor iter) $ mandel iter horShift' verShift' zoom' v
  vertex v

-- draws the whole fractal
display :: IORef Int -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat ->  IO ()
display numIter horShift verShift zoom = do
  -- clear [ ColorBuffer ]
  displayCallback $= (vertices >>= (display' numIter horShift verShift zoom) . (\x -> [x]))
  get currentWindow >>= postRedisplay

-- draws all the vertices in slices (to update the display while drawing).
display' :: IORef Int -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> [[Vertex2 GLfloat]] -> IO ()
display' numIter horShift verShift zoom chunks = do
  iter <- get numIter
  mapM_ (render iter) chunks
  displayCallback $= display numIter horShift verShift zoom
  -- exitWith ExitSuccess
  where render iter' points = do
          renderPrimitive Points (mapM_ (drawVert iter' horShift verShift zoom) points)
          flush

-- keyboard :: Key -> KeyState -> p1 -> p2 -> IO ()
-- keyboard :: IORef GLfloat -> IORef GLfloat -> Key -> KeyState -> p3 -> p4 -> IO ()
keyboard numIter horShift verShift zoom (Char keyPressed) Down _ _ = case keyPressed of
   ' '   -> postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   '\100' -> do
     val <- shiftAmount zoom
     horShift $~! (+ val)
     displayCallback $= display numIter horShift verShift zoom
   '\97' -> do
     val <- shiftAmount zoom
     horShift $~! (+ (-val))
     displayCallback $= display numIter horShift verShift zoom
   '\115' -> do
     val <- shiftAmount zoom
     verShift $~! (+ (-val))
     displayCallback $= display numIter horShift verShift zoom
   '\119' -> do
     val <- shiftAmount zoom
     verShift $~! (+ val)
     displayCallback $= display numIter horShift verShift zoom
   '\61' -> do
     val <- get zoom
     zoom $~! (+ val)
     displayCallback $= display numIter horShift verShift zoom
   '\45' -> do
     val <- get zoom
     zoom $~! (+ (-(val / 2)))
     displayCallback $= display numIter horShift verShift zoom
   '\49' -> do
     resetIter numIter
     displayCallback $= display numIter horShift verShift zoom
   '\50' -> do
     resetIter numIter
     numIter $~! (+ (20))
     displayCallback $= display numIter horShift verShift zoom
   '\51' -> do
     resetIter numIter
     numIter $~! (+ (100))
     displayCallback $= display numIter horShift verShift zoom
   '\52' -> do
     resetIter numIter
     numIter $~! (+ (400))
     displayCallback $= display numIter horShift verShift zoom
   '\53' -> do
     resetIter numIter
     numIter $~! (+ (1000))
     displayCallback $= display numIter horShift verShift zoom

   'c' -> do
     horShift' <- get horShift
     verShift' <- get verShift
     zoom' <- get zoom
     let name = show horShift' ++ "," ++
                show verShift' ++ "," ++
                show zoom'

     putStrLn ""
     putStrLn ""
     putStrLn "Writing out file:"
     putStrLn $ "images/" ++ name ++ ".png"
     putStrLn ""
     let ppm = "images/" ++ name ++ ".ppm"
         png = "images/" ++ name ++ ".png"
     (>>=) capturePPM (B.writeFile ppm)
     Mandelbrot.convert ppm png True

   _     -> return ()
keyboard _ _ _ _ _ _ _ _ = return ()


shiftAmount zoom = do
  val <- get zoom
  pure (0.1 / val)

resetIter numIter = do
  val <- get numIter
  numIter $~! (+ (iters + (-val)))


parseFile' (file) = return
  where [x, y, z] = splitOn "," $ (splitOn ".png" file) !! 0
        return = ((read x :: GLfloat), (read y :: GLfloat), (read z :: GLfloat))

parseFile :: [String] -> (GLfloat, GLfloat, GLfloat)
parseFile [justOne] = parseFile' $ (splitOn "images/" justOne) !! 1
parseFile _ = (0.0, 0.0, 1.0)

iters = 20
mandelMain = do
  (_, file) <- getArgsAndInitialize
  mapM putStrLn file
  let (initHor, initVer, initZoom) = parseFile file

  initialDisplayMode $= [ SingleBuffered, RGBMode ]
  initialWindowSize $= Size 1200 1024
  initialWindowPosition $= Position 100 100
  createWindow "Mandelbrot"
  clearColor $= Color4 0 0 0 0
  matrixMode $= Projection
  loadIdentity
  ortho (-2) 1 (-1) 1 (-1) 1
  -- fullScreen
  numIter <- newIORef iters
  horShift <- newIORef initHor
  verShift <- newIORef initVer
  zoom <- newIORef initZoom
  keyboardMouseCallback $= Just (keyboard numIter horShift verShift zoom)
  displayCallback $= (display numIter horShift verShift zoom)
  idleCallback $= Just (idle numIter)
  mainLoop

idle :: IORef Int -> IdleCallback
idle numIter = do
  val <- get numIter
  let newVal = if val < 50 then 5 else 100
  numIter $~! (+ 0)
  postRedisplay Nothing

