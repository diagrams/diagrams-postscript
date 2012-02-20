{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , DeriveDataTypeable
           , ViewPatterns
  #-}
{-|
  The EPS backend.
-}
module Diagrams.Backend.Postscript

  ( Postscript(..) -- rendering token

  , Options(..) -- for rendering options specific to Postscript
  , OutputFormat(..) -- output format options
  ) where

import qualified Graphics.Rendering.Postscript as C

import Diagrams.Prelude

import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Adjust (adjustDia2D, adjustSize)
import Diagrams.TwoD.Text
import Diagrams.TwoD.Path (Clip(..))

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Maybe (catMaybes, fromMaybe)

import Data.VectorSpace

import Data.Monoid
import qualified Data.Foldable as F
import Data.Typeable

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Postscript = Postscript
    deriving Typeable

-- | Postscript could output to several file formats, which each have their own associated properties that affect the output.
data OutputFormat = EPS

instance Monoid (Render Postscript R2) where
  mempty  = C $ return ()
  (C r1) `mappend` (C r2) = C (r1 >> r2)


instance Backend Postscript R2 where
  data Render  Postscript R2 = C (C.Render ())
  type Result  Postscript R2 = IO ()
  data Options Postscript R2 = PostscriptOptions
          { fileName     :: String       -- ^ the name of the file you want generated
          , psSize       :: SizeSpec2D   -- ^ the requested size
          , outputFormat :: OutputFormat -- ^ the output format and associated options
          }

  withStyle _ s t (C r) = C $ do
    C.save
    postscriptMiscStyle s
    r
    postscriptTransf t
    postscriptStyle s
    C.stroke
    C.restore

  doRender _ (PostscriptOptions file size out) (C r) =
    let surfaceF surface = C.renderWith surface r
        -- Everything except Dims is arbitrary. The backend
        -- should have first run 'adjustDia' to update the
        -- final size of the diagram with explicit dimensions,
        -- so normally we would only expect to get Dims anyway.
        (w,h) = case size of
                  Width w'   -> (w',w')
                  Height h'  -> (h',h')
                  Dims w' h' -> (w',h')
                  Absolute   -> (100,100)

    in  case out of
          EPS -> C.withEPSSurface file (round w) (round h) surfaceF

  adjustDia c opts d = adjustDia2D psSize setPsSize c opts d
    where setPsSize sz o = o { psSize = sz }

renderC :: (Renderable a Postscript, V a ~ R2) => a -> C.Render ()
renderC a = case render Postscript a of C r -> r

postscriptMiscStyle :: Style v -> C.Render ()
postscriptMiscStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handleFontFace
                , handle fColor
                ]
  where handle :: AttributeClass a => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        clip     = mapM_ (\p -> renderC p >> C.clip) . getClip
        fSize    = fromMaybe 12 $ getFontSize <$> getAttr s
        fFace    = fromMaybe "" $ getFont <$> getAttr s
        fSlant   = fromFontSlant  . fromMaybe FontSlantNormal
                 $ getFontSlant  <$> getAttr s
        fWeight  = fromFontWeight . fromMaybe FontWeightNormal
                 $ getFontWeight <$> getAttr s
        handleFontFace = Just $ C.selectFontFace fFace fSlant fWeight fSize
        fColor c = C.fillColor (getFillColor c)

fromFontSlant :: FontSlant -> C.FontSlant
fromFontSlant FontSlantNormal   = C.FontSlantNormal
fromFontSlant FontSlantItalic   = C.FontSlantItalic
fromFontSlant FontSlantOblique  = C.FontSlantOblique

fromFontWeight :: FontWeight -> C.FontWeight
fromFontWeight FontWeightNormal = C.FontWeightNormal
fromFontWeight FontWeightBold   = C.FontWeightBold

postscriptStyle :: Style v -> C.Render ()
postscriptStyle s = sequence_ -- foldr (>>) (return ())
              . catMaybes $ [ handle fColor
                            , handle lColor
                            , handle lWidth
                            , handle lJoin
                            , handle lCap
                            ]
  where handle :: (AttributeClass a) => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        lColor = C.strokeColor . getLineColor
        fColor c = C.fillColor (getFillColor c) >> C.fillPreserve
        lWidth = C.lineWidth . getLineWidth
        lCap = C.lineCap . getLineCap
        lJoin = C.lineJoin . getLineJoin

postscriptTransf :: Transformation R2 -> C.Render ()
postscriptTransf t = C.transform a1 a2 b1 b2 c1 c2
  where (a1,a2) = unv2 $ apply t unitX
        (b1,b2) = unv2 $ apply t unitY
        (c1,c2) = unv2 $ transl t

instance Renderable (Segment R2) Postscript where
  render _ (Linear (unv2 -> v)) = C $ uncurry C.relLineTo v
  render _ (Cubic (unv2 -> (x1,y1))
                  (unv2 -> (x2,y2))
                  (unv2 -> (x3,y3))) = C $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Postscript where
  render _ (Trail segs c) = C $ do
    mapM_ renderC segs
    when c C.closePath

instance Renderable (Path R2) Postscript where
  render _ (Path trs) = C $ C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (p, tr) = do
            uncurry C.moveTo (unp2 p)
            renderC tr

instance Renderable Text Postscript where
  render _ (Text tr _ str) = C $ do
      C.save
      postscriptTransf tr
      C.showTextCentered str
      C.restore