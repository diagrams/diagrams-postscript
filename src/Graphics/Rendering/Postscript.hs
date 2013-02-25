{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Postscript
-- Copyright   :  (c) 2013 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic tools for generating Postscript files.  There is some
-- limited support for tracking the state of the renderer when
-- given a side-effecting (in the Postscript) command.  Only drawing
-- operations are supported, not general Postscript language generation.
--
-- In the future the tracking of rendering state could lead to optimizing
-- output, but for now little optimization is attempted.  Most systems are
-- equiped with tools to optimize Postscript such as 'eps2eps'.
--
-- For details on the PostScript language see the PostScript(R) Language
-- Reference: <http://www.adobe.com/products/postscript/pdfs/PLRM.pdf>
-----------------------------------------------------------------------------
module Graphics.Rendering.Postscript
  ( Render
  , RenderState
  , Surface
  , PSWriter(..)
  , renderWith
  , renderPagesWith
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
  , gsave
  , grestore
  , saveMatrix
  , restoreMatrix
  , translate
  , scale
  , rotate
  , strokeColor
  , fillColor
  , lineWidth
  , lineCap
  , lineJoin
  , setDash
  , setFillRule
  , runImage
  , epsImage
  , parseBoundingBox
  , showText
  , showTextCentered
  , showTextAlign
  , showTextInBox
  , clip
  
  , FontSlant(..)
  , FontWeight(..)
  , setFontFace
  , setFontSlant
  , setFontWeight
  , setFontSize
  ) where

import Diagrams.Attributes(Color(..),LineCap(..),LineJoin(..),colorToRGBA)
import Diagrams.TwoD.Path hiding (stroke)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad(when)
import Data.List(intersperse)
import Data.DList(DList,toList,fromList)
import Data.Word(Word8)
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char(ord,isPrint)
import Numeric(showIntAtBase)
import System.IO (openFile, hPutStr, IOMode(..), hClose)

-- Here we want to mirror the state of side-effecting calls
-- that we have emitted into the postscript file (at least
-- ones that we do not protect in other ways).
data DrawState = DS
                 { _fillRule :: FillRule
                 , _font     :: PostscriptFont
                 } deriving (Eq)

-- This reflects the defaults from the standard.
emptyDS :: DrawState
emptyDS = DS Winding defaultFont

data RenderState = RS
                   { _drawState :: DrawState   -- The current state.
                   , _saved     :: [DrawState] -- A stack of passed states pushed by save and poped with restore.
                   }

emptyRS :: RenderState
emptyRS = RS emptyDS []

-- 

-- | Type for a monad that writes Postscript using the commands we will define later.
newtype PSWriter m = PSWriter { runPSWriter :: WriterT (DList String) IO m }
  deriving (Functor, Monad, MonadWriter (DList String), MonadIO)

-- | Type of the monad that tracks the state from side-effecting commands.
newtype Render m = Render { runRender :: StateT RenderState PSWriter m }
  deriving (Functor, Monad, MonadState RenderState, MonadIO)

-- | Abstraction of the drawing surface details.
data Surface = Surface { header :: Int -> String, footer :: Int -> String, width :: Int, height :: Int, fileName :: String } 

doRender :: Render a -> PSWriter a
doRender r = evalStateT (runRender r) emptyRS

-- | Handles opening and closing the file associated with the
--   passed 'Surface' and renders the commands built up in the
--   'Render' argument.
renderWith :: MonadIO m => Surface -> Render a -> m a
renderWith s r = liftIO $ do 
    (v,ss) <- runWriterT . runPSWriter . doRender $ r
    h <- openFile (fileName s) WriteMode
    hPutStr h (header s 1)
    mapM_ (hPutStr h) (toList ss)
    hPutStr h (footer s 1)
    hClose h
    return v

-- | Renders multiple pages given as a list of 'Render' actions
--   to the file associated with the 'Surface' argument.
renderPagesWith :: MonadIO m => Surface -> [Render a] -> m [a]
renderPagesWith s rs = liftIO $ do 
    h <- openFile (fileName s) WriteMode
    hPutStr h (header s (length rs))
    
    vs <- mapM (page h) (zip rs [1..])
    
    hClose h
    return vs
  where 
    page h (r,i) = do
      (v,ss) <- runWriterT . runPSWriter . doRender $ r
      mapM_ (hPutStr h) (toList ss)
      hPutStr h (footer s i)
      return v

-- | Builds a surface and performs an action on that surface.
withEPSSurface :: String -> Int -> Int -> (Surface -> IO a) -> IO a
withEPSSurface file w h f = f s
  where s = Surface (epsHeader w h) epsFooter w h file

renderPS :: String -> Render ()
renderPS s = Render . lift . tell $ fromList [s, "\n"]

-- | Clip with the current path.
clip :: Render ()
clip = renderPS "clip"

mkPSCall :: Show a => String -> [a] -> Render()
mkPSCall n vs = renderPS . concat $ intersperse " " (map show vs) ++ [" ", n]

mkPSCall' :: String -> [String] -> Render()
mkPSCall' n vs = renderPS . concat $ intersperse " " vs ++ [" ", n]

-- | Start a new path.
newPath :: Render ()
newPath = renderPS "newpath"

-- | Close the current path.
closePath :: Render ()
closePath = renderPS "closepath"

-- | Draw an arc given a center, radius, start, and end angle.
arc :: Double -- ^ x-coordinate of center.
    -> Double -- ^ y-coordiante of center.
    -> Double -- ^ raidus.
    -> Double -- ^ start angle in radians.
    -> Double -- ^ end angle in radians.
    -> Render ()
arc a b c d e = mkPSCall "arc" [a,b,c, d * 180 / pi, e* 180 / pi]

-- | Move the current point.
moveTo :: Double -> Double -> Render ()
moveTo x y = mkPSCall "moveto" [x,y]

-- | Add a line to the current path from the current point to the given point.
--   The current point is also moved with this command.
lineTo :: Double -> Double -> Render ()
lineTo x y = mkPSCall "lineto" [x,y]

-- | Add a cubic Bézier curve segment to the current path from the current point.
--   The current point is also moved with this command.
curveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
curveTo ax ay bx by cx cy = mkPSCall "curveto" [ax,ay,bx,by,cx,cy]

-- | Add a line segment to the current path using relative coordinates.
relLineTo :: Double -> Double -> Render ()
relLineTo x y = mkPSCall "rlineto" [x,y]

-- | Add a cubic Bézier curve segment to the current path from the current point
--   using relative coordinates.
relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
relCurveTo ax ay bx by cx cy = mkPSCall "rcurveto" [ax,ay,bx,by,cx,cy]

-- | Stroke the current path.
stroke :: Render ()
stroke = renderPS "s"

-- | Fill the current path.
fill :: Render ()
fill = do
    (RS (DS {..}) _) <- get
    case _fillRule of
        Winding -> renderPS "fill"
        EvenOdd -> renderPS "eofill"

-- | Fill the current path without affecting the graphics state.
fillPreserve :: Render ()
fillPreserve = do
    gsave
    fill
    grestore

-- | Run an external file rendering an image from a file reference.
runImage :: String -> Render ()
runImage f = do
    stringPS f
    renderPS " run"

-- | Embed an EPS file.
epsImage :: String -> Render ()
epsImage f = do
    renderPS "beginEPS"
    renderPS f
    renderPS "endEPS"

-- | Parse some EPS to find the bounding box comment.
parseBoundingBox :: Num n => String -> Either String (n,n,n,n)
parseBoundingBox f = 
    case filter ("%%BoundingBox:" `isPrefixOf`) $ lines f of
      (l:_) -> case map fromIntegral . catMaybes . map (readMaybe :: String -> Maybe Int) . splitOn " " . drop 14 $ l of
                  (a:b:c:d:_) -> Right (a,b,c,d)
                  _           -> Left "Failed to parse '%%BoundingBox:' line."
      _     -> Left "Failed to find '%%BoundingBox:'."

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = 
  case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

-- | Draw a string at the current point.
showText :: String -> Render ()
showText s = do
    renderFont
    stringPS s
    renderPS " show"

-- | Draw a string by first measuring the width then offseting by half.
showTextCentered :: String -> Render ()
showTextCentered s = do
    renderFont
    stringPS s
    renderPS " showcentered"

-- | Draw a string uniformally scaling to fit within a bounding box.
showTextInBox :: (Double,Double) -> (Double,Double) -> String -> Render ()
showTextInBox (a,b) (c,d) s = do
    renderFont
    renderPS . unwords . map show $ [a,b,c,d]
    stringPS s
    renderPS " showinbox"

-- | Draw a string with offset factors from center relative to the width and height.
showTextAlign :: Double -> Double -> String -> Render ()
showTextAlign xt yt s = do
    renderFont
    renderPS . unwords . map show $ [xt, yt]
    stringPS s
    renderPS " showalign"

-- | Apply a transform matrix to the current transform.
transform :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
transform ax ay bx by tx ty = when (vs /= [1.0,0.0,0.0,1.0,0.0,0.0]) $
      renderPS (matrixPS vs ++ " concat")
    where vs  = [ax,ay,bx,by,tx,ty]

matrixPS :: Show a => [a] -> String
matrixPS vs = unwords ("[" : map show vs ++ ["]"])

-- | Push the current state of the renderer onto the state stack.
save :: Render ()
save = do
    renderPS "save"
    modify $ \rs@(RS{..}) -> rs { _saved = _drawState : _saved }

-- | Replace the current state by popping the state stack.
restore :: Render ()
restore = do
    renderPS "restore"
    modify go
  where
    go rs@(RS{_saved = d:ds}) = rs { _drawState = d, _saved = ds }
    go rs = rs

-- | Push the current graphics state.
gsave :: Render ()
gsave = do
    renderPS "gsave"
    modify $ \rs@(RS{..}) -> rs { _saved = _drawState : _saved }

-- | Pop the current graphics state.
grestore :: Render ()
grestore = do
    renderPS "grestore"
    modify go
  where
    go rs@(RS{_saved = d:ds}) = rs { _drawState = d, _saved = ds }
    go rs = rs

-- | Push the current transform matrix onto the execution stack.
saveMatrix :: Render ()
saveMatrix = renderPS "matrix currentmatrix"

-- | Set the current transform matrix to be the matrix found by popping
--   the execution stack.
restoreMatrix :: Render ()
restoreMatrix = renderPS "setmatrix"

colorPS :: Color c => c -> [Double]
colorPS c = [ r, g, b ]
  where (r,g,b,_) = colorToRGBA c

-- | Set the color of the stroke.
strokeColor :: (Color c) => c -> Render ()
strokeColor c = mkPSCall "setrgbcolor" (colorPS c)

-- | Set the color of the fill.
fillColor :: (Color c) => c -> Render ()
fillColor c = mkPSCall "setrgbcolor" (colorPS c)

-- | Set the line width.
lineWidth :: Double -> Render ()
lineWidth w = mkPSCall "setlinewidth" [w]

-- | Set the line cap style.
lineCap :: LineCap -> Render ()
lineCap lc = mkPSCall "setlinecap" [fromLineCap lc] 

-- | Set the line join method.
lineJoin :: LineJoin -> Render ()
lineJoin lj = mkPSCall "setlinejoin" [fromLineJoin lj]

-- | Set the dash style.
setDash :: [Double] -- ^ Dash pattern (even indices are "on").
        -> Double   -- ^ Offset.
        -> Render ()
setDash as offset = mkPSCall' "setdash" [showArray as, show offset]

-- | Set the fill rule.
setFillRule :: FillRule -> Render ()
setFillRule r = modify (\rs@(RS ds _) -> rs { _drawState = ds { _fillRule = r } })

showArray :: Show a => [a] -> String
showArray as = concat ["[", concat $ intersperse " " (map show as), "]"]

fromLineCap :: LineCap -> Int
fromLineCap LineCapRound  = 1
fromLineCap LineCapSquare = 2
fromLineCap _             = 0

fromLineJoin :: LineJoin -> Int
fromLineJoin LineJoinRound = 1
fromLineJoin LineJoinBevel = 2
fromLineJoin _             = 0

-- | Translate the current transform matrix.
translate :: Double -> Double -> Render ()
translate x y = mkPSCall "translate" [x,y]

-- | Scale the current transform matrix.
scale :: Double -> Double -> Render ()
scale x y = mkPSCall "scale" [x,y]

-- | Rotate the current transform matrix.
rotate :: Double -> Render ()
rotate t = mkPSCall "rotate" [t]

stringPS :: String -> Render ()
stringPS ss = Render $ lift (tell (fromList ("(" : map escape ss)) >> tell (fromList [")"]))
  where escape '\n' = "\\n"
        escape '\r' = "\\r"
        escape '\t' = "\\t"
        escape '\b' = "\\b"
        escape '\f' = "\\f"
        escape '\\' = "\\"
        escape '('  = "\\("
        escape ')'  = "\\)"
        escape c | isPrint c = [c]
                 | otherwise = '\\' : showIntAtBase 7 ("01234567"!!) (ord c) ""

epsHeader w h pages = concat
          [ "%!PS-Adobe-3.0", if pages == 1 then " EPSF-3.0\n" else "\n"
          , "%%Creator: diagrams-postscript 0.1\n"
          , "%%BoundingBox: 0 0 ", show w, " ", show h, "\n"
          , "%%Pages: ", show pages, "\n"
          , "%%EndComments\n\n"
          , "%%BeginProlog\n"
          , "%%BeginResource: procset diagrams-postscript 0 0\n"
          , "/s { 0.0 currentlinewidth ne { stroke } if } bind def\n"
          , "/nvhalf { 2 div neg exch 2 div neg exch } bind def\n"
          , "/showcentered { dup stringwidth nvhalf moveto show } bind def\n"
          , "/stringbbox { 0 0 moveto true charpath flattenpath pathbbox } bind def\n"
          , "/wh { 1 index 4 index sub 1 index 4 index sub } bind def\n"
          , "/showinbox { gsave dup stringbbox wh 11 7 roll mark 11 1 roll "
          , "wh dup 7 index div 2 index 9 index div 1 index 1 index lt "
          , "{ pop dup 9 index mul neg 3 index add 2 div 7 index add "
          , " 6 index 13 index abs add } "
          , "{ exch pop 6 index 12 index abs 2 index mul 7 index add } "
          , "ifelse 17 3 roll cleartomark 4 1 roll translate dup scale "
          , "0 0 moveto show grestore } bind def\n"
          , "/showalign { dup mark exch stringbbox wh 10 -1 roll exch 10 1 roll mul "
          , "neg 9 -2 roll mul 4 index add neg 8 2 roll cleartomark 3 1 roll moveto "
          , "show } bind def\n"
          , "/beginEPS { save /showpage {} def 0 setgray 0 setlinecap 1 setlinewidth "
          , "0 setlinejoin 10 setmiterlimit [ ] 0 setdash newpath } bind def\n"
          , "/endEPS { restore } bind def\n"
          , "%%EndResource\n"
          , "%%EndProlog\n"
          , "%%BeginSetup\n"
          , "%%EndSetup\n"
          , "%%Page: 1 1\n"
          ]
epsFooter page = concat
          [ "showpage\n"
          , "%%PageTrailer\n"
          , "%%EndPage: ", show page, "\n"
          ]

---------------------------
-- Font
data PostscriptFont = PostscriptFont
    { _face   :: String
    , _slant  :: FontSlant
    , _weight :: FontWeight
    , _size   :: Double
    } deriving (Eq, Show)

data FontSlant = FontSlantNormal
               | FontSlantItalic
               | FontSlantOblique
               | FontSlant Double
            deriving (Show, Eq)

data FontWeight = FontWeightNormal
                | FontWeightBold
            deriving (Show, Eq)

defaultFont :: PostscriptFont
defaultFont = PostscriptFont "Helvetica" FontSlantNormal FontWeightNormal 1

renderFont :: Render ()
renderFont = do
    (RS (DS _ (PostscriptFont {..})) _) <- get
    renderPS $ concat ["/", fontFromName _face _slant _weight, " ", show _size, " selectfont"]

-- This is a little hacky.  I'm not sure there are good options.
fontFromName :: String -> FontSlant -> FontWeight -> String
fontFromName n s w = font ++ bold w ++ italic s
  where
    font = map f n
    f ' ' = '-'
    f c   = c

    bold FontWeightNormal = ""
    bold FontWeightBold   = "Bold"

    italic FontSlantNormal = ""
    italic FontSlantItalic = "Italic"
    italic FontSlantOblique = "Oblique"
    italic _                = ""

-- | Set the font face.
setFontFace :: String -> Render ()
setFontFace n = modify (\rs@(RS ds@(DS {..})  _) -> rs { _drawState = ds { _font = _font { _face = n } } })

-- | Set the font slant.
setFontSlant :: FontSlant -> Render ()
setFontSlant s = modify (\rs@(RS ds@(DS {..})  _) -> rs { _drawState = ds { _font = _font { _slant = s } } })

-- | Set the font weight.
setFontWeight :: FontWeight -> Render ()
setFontWeight w = modify (\rs@(RS ds@(DS {..})  _) -> rs { _drawState = ds { _font = _font { _weight = w } } })

-- | Set the font size.
setFontSize :: Double -> Render ()
setFontSize s = modify (\rs@(RS ds@(DS {..})  _) -> rs { _drawState = ds { _font = _font { _size = s } } })

