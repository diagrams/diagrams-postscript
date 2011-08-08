{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Rendering.Postscript
  ( Render(..)
  , renderWith
  , withEPSSurface
  , newPath
  , moveTo
  , lineTo
  , curveTo
  , relLineTo
  , relCurveTo
  , arc
  , closePath
  , stroke
  , fill
  , fillPreserve
  , transform
  , save
  , restore
  , translate
  , scale
  , rotate
  , strokeColor
  , fillColor
  , lineWidth
  , lineCap
  , lineJoin
  ) where

import Diagrams.Attributes(Color(..),LineCap(..),LineJoin(..))
import Control.Monad.Writer
import Data.List(intersperse)
import Data.DList(DList,toList,fromList)
import Data.Word(Word8)
import System.IO (openFile, hPutStr, IOMode(..), hClose)

newtype Render m = Render { runRender :: WriterT (DList String) IO m }
  deriving (Functor, Monad, MonadWriter (DList String))

data Surface = Surface { header :: String, footer :: String, width :: Int, height :: Int, fileName :: String } 

renderWith :: MonadIO m => Surface -> Render a -> m a
renderWith s r = liftIO $ do 
    (v,ss) <- runWriterT $ (runRender r)
    h <- openFile (fileName s) WriteMode
    hPutStr h (header s)
    mapM_ (hPutStr h) (toList ss)
    hPutStr h (footer s)
    hClose h
    return v

withEPSSurface :: String -> Int -> Int -> (Surface -> IO a) -> IO a
withEPSSurface file w h f = f s
  where s = Surface (epsHeader w h) epsFooter w h file

renderPS :: String -> Render ()
renderPS s = tell $ fromList [s, "\n"]

mkPSCall :: Show a => String -> [a] -> Render()
mkPSCall n vs = renderPS . concat $ intersperse " " (map show vs) ++ [" ", n]

newPath :: Render ()
newPath = renderPS "newpath"

closePath :: Render ()
closePath = renderPS "closepath"

arc :: Double -> Double -> Double -> Double -> Double -> Render ()
arc a b c d e = mkPSCall "arc" [a,b,c,d,e]

moveTo :: Double -> Double -> Render ()
moveTo x y = mkPSCall "moveto" [x,y]

lineTo :: Double -> Double -> Render ()
lineTo x y = mkPSCall "lineto" [x,y]

curveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
curveTo ax ay bx by cx cy = mkPSCall "curveto" [ax,ay,bx,by,cx,cy]

relLineTo :: Double -> Double -> Render ()
relLineTo x y = mkPSCall "rlineto" [x,y]

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
relCurveTo ax ay bx by cx cy = mkPSCall "rcurveto" [ax,ay,bx,by,cx,cy]

stroke :: Render ()
stroke = renderPS "stroke"

fill :: Render ()
fill = renderPS "fill"

fillPreserve :: Render ()
fillPreserve = renderPS "gsave fill grestore"

transform :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
transform ax ay bx by tx ty = 
    if vs /= [1.0,0.0,0.0,1.0,0.0,0.0]
      then renderPS (matrixPS vs ++ " concat")
      else return ()
    where vs  = [ax,ay,bx,by,tx,ty]

matrixPS :: Show a => [a] -> String
matrixPS vs = concat $ intersperse " " ("[" : (map show vs) ++ ["]"])

save :: Render ()
save = renderPS "save"

restore :: Render ()
restore = renderPS "restore"

byteRange :: Double -> Word8
byteRange d = floor (d * 255)

colorPS :: Color c => c -> [Word8]
colorPS c = [ s r, s g, s b ]
  where s = byteRange
        (r,g,b,a) = colorToRGBA c

strokeColor :: (Color c) => c -> Render ()
strokeColor c = mkPSCall "setrgbcolor" (colorPS c)

fillColor :: (Color c) => c -> Render ()
fillColor c = mkPSCall "setrgbcolor" (colorPS c)

lineWidth :: Double -> Render ()
lineWidth w = mkPSCall "setlinewidth" [w]

lineCap :: LineCap -> Render ()
lineCap lc = mkPSCall "setlinecap" [fromLineCap lc] 

lineJoin :: LineJoin -> Render ()
lineJoin lj = mkPSCall "setlinejoin" [fromLineJoin lj]

fromLineCap :: LineCap -> String
fromLineCap LineCapRound  = "1"
fromLineCap LineCapSquare = "2"
fromLineCap _             = "0"

fromLineJoin :: LineJoin -> String
fromLineJoin LineJoinRound = "1"
fromLineJoin LineJoinBevel = "2"
fromLineJoin _             = "0"

translate :: Double -> Double -> Render ()
translate x y = mkPSCall "translate" [x,y]

scale :: Double -> Double -> Render ()
scale x y = mkPSCall "scale" [x,y]

rotate :: Double -> Render ()
rotate t = mkPSCall "rotate" [t]

epsHeader w h = concat
          [ "%!PS-Adobe-3.0 EPSF-3.0\n"
          , "%%Creator: diagrams-eps 0.1\n"
          , "%%BoundingBox: 0 0 ", show w, " ", show h, "\n"
          , "%%Pages: 1\n"
          , "%%EndComments\n\n"
          , "%%BeginProlog\n"
          , "%%EndProlog\n"
          , "%%BeginSetup\n"
          , "%%EndSetup\n"
          , "%%Page: 1 1\n"
          ]
epsFooter = concat
          [ "%%PageTrailer\n"
          , "%%EndPage: 1\n"
          ]
