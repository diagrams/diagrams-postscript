{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , DeriveDataTypeable
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

import Control.Monad (when)
import Data.Maybe (catMaybes)

import Data.VectorSpace

import Data.Monoid
import qualified Data.Foldable as F
import Data.Typeable

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Postscript = Postscript
    deriving Typeable

-- | Postscript could output to several file formats, which each have their own associated properties that affect the output.
data OutputFormat =
  EPS { epsSize :: (Int, Int) -- ^ the size of the output is given in points
      }

instance Monoid (Render Postscript R2) where
  mempty  = C $ return ()
  (C r1) `mappend` (C r2) = C (r1 >> r2)


instance Backend Postscript R2 where
  data Render  Postscript R2 = C (C.Render ())
  type Result  Postscript R2 = IO ()
  data Options Postscript R2 = PostscriptOptions
          { fileName     :: String       -- ^ the name of the file you want generated
          , outputFormat :: OutputFormat -- ^ the output format and associated options
          }

  withStyle _ s t (C r) = C $ do
    C.save
    -- postscriptMiscStyle s
    r
    postscriptTransf t
    postscriptStyle s
    C.stroke
    C.restore

  doRender _ options (C r) = 
    let surfaceF surface = C.renderWith surface r
    in  case outputFormat options of
          EPS (w,h) -> C.withEPSSurface (fileName options) w h surfaceF

  adjustDia c opts d = adjustDia2D (getSize . outputFormat) c opts d
    where getSize (EPS (w,h)) = (fromIntegral w, fromIntegral h)

renderC :: (Renderable a Postscript, V a ~ R2) => a -> C.Render ()
renderC a = case (render Postscript a) of C r -> r

postscriptStyle :: Style v -> C.Render ()
postscriptStyle s = foldr (>>) (return ())
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
  where (a1,a2) = apply t (1,0)
        (b1,b2) = apply t (0,1)
        (c1,c2) = transl t

instance Renderable Ellipse Postscript where
  render _ ell = C $ do
    let P (xc,yc) = ellipseCenter ell
        (xs,ys)   = ellipseScale ell
        Rad th    = ellipseAngle ell
    C.newPath
    C.saveMatrix
    C.translate xc yc
    C.rotate th
    C.scale xs ys
    C.arc 0 0 1 0 (2*pi)
    C.restoreMatrix

instance Renderable (Segment R2) Postscript where
  render _ (Linear v) = C $ uncurry C.relLineTo v
  render _ (Cubic (x1,y1) (x2,y2) (x3,y3)) = C $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Postscript where
  render _ (Trail segs c) = C $ do
    mapM_ renderC segs
    when c C.closePath

instance Renderable (Path R2) Postscript where
  render _ (Path trs) = C $ C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (P p, tr) = do
            uncurry C.moveTo p
            renderC tr