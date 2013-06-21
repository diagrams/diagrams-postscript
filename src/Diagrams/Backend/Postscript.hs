{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Postscript
-- Copyright   :  (c) 2013 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A Postscript rendering backend for diagrams.
--
-- To build diagrams for Postscript rendering use the @Postscript@
-- type in the diagram type construction
--
-- > d :: Diagram Postscript R2
-- > d = ...
--
-- and render giving the @Postscript@ token
--
-- > renderDia Postscript (PostscriptOptions "file.eps" (Width 400) EPS) d
--
-- This IO action will write the specified file.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Postscript

  ( -- * Backend token
    Postscript(..)

    -- * Postscript-specific options
    -- $PostscriptOptions

  , Options(..)

    -- * Postscript-supported output formats
  , OutputFormat(..)
  ) where


import qualified Graphics.Rendering.Postscript as C

import           Diagrams.Prelude

import           Diagrams.Core.Transform

import           Diagrams.TwoD.Adjust          (adjustDia2D)
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Path            (Clip (..), getFillRule)
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Size            (requiredScaleT)
import           Diagrams.TwoD.Text

import           Control.Applicative           ((<$>))
import           Control.Monad                 (when)
import           Data.Maybe                    (catMaybes, fromMaybe)

import           Data.VectorSpace

import qualified Data.Foldable                 as F
import qualified Data.List.NonEmpty            as N
import           Data.Monoid                   hiding ((<>))
import           Data.Monoid.MList
import           Data.Monoid.Split
import           Data.Typeable

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Postscript = Postscript
    deriving (Eq,Ord,Read,Show,Typeable)

-- | Postscript only supports EPS style output at the moment.  Future formats would each
--   have their own associated properties that affect the output.
data OutputFormat = EPS -- ^ Encapsulated Postscript output.
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable)

instance Monoid (Render Postscript R2) where
  mempty  = C $ return ()
  (C r1) `mappend` (C r2) = C (r1 >> r2)


instance Backend Postscript R2 where
  data Render  Postscript R2 = C (C.Render ())
  type Result  Postscript R2 = IO ()
  data Options Postscript R2 = PostscriptOptions
          { psfileName     :: String       -- ^ the name of the file you want generated
          , psSizeSpec     :: SizeSpec2D   -- ^ the requested size of the output
          , psOutputFormat :: OutputFormat -- ^ the output format and associated options
          }
    deriving Show

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
        (w,h) = sizeFromSpec size

    in  case out of
          EPS -> C.withEPSSurface file (round w) (round h) surfaceF

  adjustDia c opts d = adjustDia2D psSizeSpec setPsSize c opts d
    where setPsSize sz o = o { psSizeSpec = sz }

sizeFromSpec size = case size of
   Width w'   -> (w',w')
   Height h'  -> (h',h')
   Dims w' h' -> (w',h')
   Absolute   -> (100,100)

instance MultiBackend Postscript R2 where
   renderDias b opts ds = doRenderPages b (combineSizes (map fst rs)) (map snd rs) >> return ()
     where
       mkMax (a,b) = (Max a, Max b)
       fromMaxPair (Max a, Max b) = (a,b)

       rs = map mkRender ds
       mkRender d = (opts', mconcat . map renderOne . prims $ d')
         where
           (opts', d') = adjustDia b opts d
           renderOne (p, (M t,      s)) = withStyle b s mempty (render b (transform t p))
           renderOne (p, (t1 :| t2, s)) = withStyle b s t1 (render b (transform (t1 <> t2) p))

       combineSizes (o:os) = o { psSizeSpec = uncurry Dims . fromMaxPair . sconcat $ f o N.:| fmap f os }
         where f = mkMax . sizeFromSpec . psSizeSpec

       doRenderPages _ (PostscriptOptions file size out) rs =
        let surfaceF surface = C.renderPagesWith surface (map (\(C r) -> r) rs)
            (w,h) = sizeFromSpec size
        in case out of
           EPS -> C.withEPSSurface file (round w) (round h) surfaceF

renderC :: (Renderable a Postscript, V a ~ R2) => a -> C.Render ()
renderC a = case render Postscript a of C r -> r

-- | Handle \"miscellaneous\" style attributes (clip, font stuff, fill
--   color and fill rule).
postscriptMiscStyle :: Style v -> C.Render ()
postscriptMiscStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle fFace
                , handle fSlant
                , handle fWeight
                , handle fSize
                , handle fColor
                , handle lFillRule
                ]
  where handle :: AttributeClass a => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        clip     = mapM_ (\p -> renderC p >> C.clip) . getClip
        fSize    = C.setFontSize <$> getFontSize
        fFace    = C.setFontFace <$> getFont
        fSlant   = C.setFontSlant . fromFontSlant <$> getFontSlant
        fWeight  = C.setFontWeight . fromFontWeight <$> getFontWeight
        fColor c = C.fillColor (getFillColor c)
        lFillRule = C.setFillRule . getFillRule

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
                            , handle lDashing
                            ]
  where handle :: (AttributeClass a) => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        lColor = C.strokeColor . getLineColor
        fColor c = C.fillColor (getFillColor c) >> C.fillPreserve
        lWidth = C.lineWidth . getLineWidth
        lCap = C.lineCap . getLineCap
        lJoin = C.lineJoin . getLineJoin
        lDashing (getDashing -> Dashing ds offs) =
          C.setDash ds offs

postscriptTransf :: Transformation R2 -> C.Render ()
postscriptTransf t = C.transform a1 a2 b1 b2 c1 c2
  where (a1,a2) = unr2 $ apply t unitX
        (b1,b2) = unr2 $ apply t unitY
        (c1,c2) = unr2 $ transl t

instance Renderable (Segment Closed R2) Postscript where
  render _ (Linear (OffsetClosed (unr2 -> v))) = C $ uncurry C.relLineTo v
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (OffsetClosed (unr2 -> (x3,y3))))
    = C $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Postscript where
  render _ t = flip withLine t $ renderT . lineSegments
    where
      renderT segs =
        C $ do
          mapM_ renderC segs
          when (isLoop t) C.closePath

instance Renderable (Path R2) Postscript where
  render _ (Path trs) = C $ C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (viewLoc -> (unp2 -> p, tr)) = do
            uncurry C.moveTo p
            renderC tr

instance Renderable Text Postscript where
  render _ (Text tr al str) = C $ do
      C.save
      postscriptTransf tr
      case al of
        BoxAlignedText xt yt -> C.showTextAlign xt yt str
        BaselineText         -> C.moveTo 0 0 >> C.showText str
      C.restore

-- $PostscriptOptions
--
-- Unfortunately, Haddock does not yet support documentation for
-- associated data families, so we must just provide it manually.
-- This module defines
--
-- > data family Options Postscript R2 = PostscriptOptions
-- >           { psfileName     :: String       -- ^ the name of the file you want generated
-- >           , psSizeSpec     :: SizeSpec2D   -- ^ the requested size of the output
-- >           , psOutputFormat :: OutputFormat -- ^ the output format and associated options
-- >           }
