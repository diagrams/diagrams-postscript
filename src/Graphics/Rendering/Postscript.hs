{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module Graphics.Rendering.Postscript
  ( Render(..)
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

import Diagrams.Attributes(Color(..),LineCap(..),LineJoin(..),colorToSRGBA)
import Diagrams.TwoD.Path hiding (stroke)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad(when)
import Data.List(intersperse)
import Data.DList(DList,toList,fromList)
import Data.Word(Word8)
import Data.Char(ord,isPrint)
import Numeric(showIntAtBase)
import System.IO (openFile, hPutStr, IOMode(..), hClose)

data DrawState = DS
                 { _fillRule :: FillRule
                 , _font     :: PostscriptFont
                 } deriving (Eq)
                 
emptyDS :: DrawState
emptyDS = DS Winding defaultFont

data RenderState = RS
                   { _drawState :: DrawState
                   , _saved     :: [DrawState]
                   }

emptyRS :: RenderState
emptyRS = RS emptyDS []

newtype PSWriter m = PSWriter { runPSWriter :: WriterT (DList String) IO m }
  deriving (Functor, Monad, MonadWriter (DList String))

newtype Render m = Render { runRender :: StateT RenderState PSWriter m }
  deriving (Functor, Monad, MonadState RenderState)

data Surface = Surface { header :: Int -> String, footer :: Int -> String, width :: Int, height :: Int, fileName :: String } 

doRender :: Render a -> PSWriter a
doRender r = evalStateT (runRender r) emptyRS

renderWith :: MonadIO m => Surface -> Render a -> m a
renderWith s r = liftIO $ do 
    (v,ss) <- runWriterT . runPSWriter . doRender $ r
    h <- openFile (fileName s) WriteMode
    hPutStr h (header s 1)
    mapM_ (hPutStr h) (toList ss)
    hPutStr h (footer s 1)
    hClose h
    return v
    
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

withEPSSurface :: String -> Int -> Int -> (Surface -> IO a) -> IO a
withEPSSurface file w h f = f s
  where s = Surface (epsHeader w h) epsFooter w h file

renderPS :: String -> Render ()
renderPS s = Render . lift . tell $ fromList [s, "\n"]

clip :: Render ()
clip = renderPS "clip"

mkPSCall :: Show a => String -> [a] -> Render()
mkPSCall n vs = renderPS . concat $ intersperse " " (map show vs) ++ [" ", n]

mkPSCall' :: String -> [String] -> Render()
mkPSCall' n vs = renderPS . concat $ intersperse " " vs ++ [" ", n]

newPath :: Render ()
newPath = renderPS "newpath"

closePath :: Render ()
closePath = renderPS "closepath"

arc :: Double -> Double -> Double -> Double -> Double -> Render ()
arc a b c d e = mkPSCall "arc" [a,b,c, d * 180 / pi, e* 180 / pi]

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
stroke = renderPS "s"

fill :: Render ()
fill = do
    (RS (DS {..}) _) <- get
    case _fillRule of
        Winding -> renderPS "fill"
        EvenOdd -> renderPS "eofill"

fillPreserve :: Render ()
fillPreserve = do
    gsave
    fill
    grestore

showText :: String -> Render ()
showText s = do
    renderFont
    stringPS s
    renderPS " show"

showTextCentered :: String -> Render ()
showTextCentered s = do
    renderFont
    stringPS s
    renderPS " showcentered"

showTextInBox :: (Double,Double) -> (Double,Double) -> String -> Render ()
showTextInBox (a,b) (c,d) s = do
    renderFont
    renderPS . unwords . map show $ [a,b,c,d]
    stringPS s
    renderPS " showinbox"

showTextAlign :: Double -> Double -> String -> Render ()
showTextAlign xt yt s = do
    renderFont
    renderPS . unwords . map show $ [xt, yt]
    stringPS s
    renderPS " showalign"

transform :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
transform ax ay bx by tx ty = when (vs /= [1.0,0.0,0.0,1.0,0.0,0.0]) $
      renderPS (matrixPS vs ++ " concat")
    where vs  = [ax,ay,bx,by,tx,ty]

matrixPS :: Show a => [a] -> String
matrixPS vs = unwords ("[" : map show vs ++ ["]"])

save :: Render ()
save = do
    renderPS "save"
    modify $ \rs@(RS{..}) -> rs { _saved = _drawState : _saved }

restore :: Render ()
restore = do
    renderPS "restore"
    modify go
  where
    go rs@(RS{_saved = d:ds}) = rs { _drawState = d, _saved = ds }
    go rs = rs

gsave :: Render ()
gsave = do
    renderPS "gsave"
    modify $ \rs@(RS{..}) -> rs { _saved = _drawState : _saved }

grestore :: Render ()
grestore = do
    renderPS "grestore"
    modify go
  where
    go rs@(RS{_saved = d:ds}) = rs { _drawState = d, _saved = ds }
    go rs = rs

saveMatrix :: Render ()
saveMatrix = renderPS "matrix currentmatrix"

restoreMatrix :: Render ()
restoreMatrix = renderPS "setmatrix"

colorPS :: Color c => c -> [Double]
colorPS c = [ r, g, b ]
  where (r,g,b,_) = colorToSRGBA c

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

setDash :: [Double] -> Double -> Render ()
setDash as offset = mkPSCall' "setdash" [showArray as, show offset]

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

translate :: Double -> Double -> Render ()
translate x y = mkPSCall "translate" [x,y]

scale :: Double -> Double -> Render ()
scale x y = mkPSCall "scale" [x,y]

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

setFontFace :: String -> Render ()
setFontFace n = modify (\rs@(RS ds@(DS {..})  _) -> rs { _drawState = ds { _font = _font { _face = n } } })

setFontSlant :: FontSlant -> Render ()
setFontSlant s = modify (\rs@(RS ds@(DS {..})  _) -> rs { _drawState = ds { _font = _font { _slant = s } } })

setFontWeight :: FontWeight -> Render ()
setFontWeight w = modify (\rs@(RS ds@(DS {..})  _) -> rs { _drawState = ds { _font = _font { _weight = w } } })

setFontSize :: Double -> Render ()
setFontSize s = modify (\rs@(RS ds@(DS {..})  _) -> rs { _drawState = ds { _font = _font { _size = s } } })

