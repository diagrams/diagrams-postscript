{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
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
-- > d :: Diagram Postscript V2
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
  , B

    -- * Postscript-specific options
    -- $PostscriptOptions

  , Options(..), psfileName, psSizeSpec, psOutputFormat

    -- * Postscript-supported output formats
  , OutputFormat(..)

  , renderDias
  ) where

import           Diagrams.Core.Compile
import qualified Graphics.Rendering.Postscript as C
import           Diagrams.Backend.Postscript.CMYK

import           Diagrams.Prelude              hiding (view, fillColor)

import           Diagrams.TwoD.Adjust          (adjustDia2D)
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Path            (Clip (Clip), getFillRule)
import           Diagrams.TwoD.Text

import           Control.Lens                  hiding (transform)
import           Control.Monad                 (when)
import           Data.Maybe                    (catMaybes)

import qualified Data.Foldable                 as F
import           Data.Hashable                 (Hashable (..))
import qualified Data.List.NonEmpty            as N
import           Data.Tree
import           Data.Typeable
import           GHC.Generics                  (Generic)

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Postscript = Postscript
    deriving (Eq,Ord,Read,Show,Typeable)

type B = Postscript

-- | Postscript only supports EPS style output at the moment.  Future formats would each
--   have their own associated properties that affect the output.
data OutputFormat = EPS -- ^ Encapsulated Postscript output.
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Generic)

instance Hashable OutputFormat

instance Monoid (Render Postscript V2 Double) where
  mempty  = C $ return ()
  (C x) `mappend` (C y) = C (x >> y)


instance Backend Postscript V2 Double where
  data Render  Postscript V2 Double = C (C.Render ())
  type Result  Postscript V2 Double = IO ()
  data Options Postscript V2 Double = PostscriptOptions
          { _psfileName     :: String       -- ^ the name of the file you want generated
          , _psSizeSpec     :: SizeSpec2D Double   -- ^ the requested size of the output
          , _psOutputFormat :: OutputFormat -- ^ the output format and associated options
          }
    deriving (Show)

  renderRTree _ opts t =
    let surfaceF surface = C.renderWith surface r
        (w,h) = sizeFromSpec (opts^.psSizeSpec)
        r = runC . toRender $ t
    in case opts^.psOutputFormat of
         EPS -> C.withEPSSurface (opts^.psfileName) (round w) (round h) surfaceF

  adjustDia c opts d = adjustDia2D psSizeSpec c opts d

runC :: Render Postscript V2 Double -> C.Render ()
runC (C r) = r

toRender :: RTree Postscript V2 Double a -> Render Postscript V2 Double
toRender (Node (RPrim p) _) = render Postscript p
toRender (Node (RStyle sty) rs) = C $ do
  C.save
  postscriptMiscStyle sty
  runC $ F.foldMap toRender rs
  postscriptStyle sty
  C.stroke
  C.restore
toRender (Node _ rs) = F.foldMap toRender rs

instance Hashable (Options Postscript V2 Double) where
  hashWithSalt s (PostscriptOptions fn sz out) =
    s `hashWithSalt` fn
      `hashWithSalt` sz
      `hashWithSalt` out

sizeFromSpec :: SizeSpec2D Double -> (Double, Double)
sizeFromSpec sz = case sz of
   Width w'   -> (w',w')
   Height h'  -> (h',h')
   Dims w' h' -> (w',h')
   Absolute   -> (100,100)

psfileName :: Lens' (Options Postscript V2 Double) String
psfileName = lens (\(PostscriptOptions {_psfileName = f}) -> f)
                     (\o f -> o {_psfileName = f})

psSizeSpec :: Lens' (Options Postscript V2 Double) (SizeSpec2D Double)
psSizeSpec = lens (\(PostscriptOptions {_psSizeSpec = s}) -> s)
                     (\o s -> o {_psSizeSpec = s})

psOutputFormat :: Lens' (Options Postscript V2 Double) OutputFormat
psOutputFormat = lens (\(PostscriptOptions {_psOutputFormat = t}) -> t)
                     (\o t -> o {_psOutputFormat = t})

renderDias :: (Semigroup m, Monoid m) =>
               Options Postscript V2 Double -> [QDiagram Postscript V2 Double m] -> IO [()]
renderDias opts ds = case opts^.psOutputFormat of
  EPS -> C.withEPSSurface (opts^.psfileName) (round w) (round h) surfaceF
    where
      surfaceF surface = C.renderPagesWith surface (map (\(C r) -> r) rs)
      (w,h) = sizeFromSpec (cSize^.psSizeSpec)

      dropMid (x, _, z) = (x,z)

      optsdss = map (dropMid . adjustDia Postscript opts) ds
      cSize = (combineSizes $ map fst optsdss)
      g2o = scaling (sqrt (w * h))
      rs = map (toRender . toRTree g2o . snd) optsdss

      combineSizes [] = PostscriptOptions "" (Dims 100 100) EPS
      combineSizes (o:os) =
        o { _psSizeSpec = uncurry Dims . fromMaxPair . sconcat $ f o N.:| fmap f os }
        where
          f = mkMax . sizeFromSpec . _psSizeSpec
          fromMaxPair (Max x, Max y) = (x,y)
          mkMax (x,y) = (Max x, Max y)

renderC :: (Renderable a Postscript, V a ~ V2, N a ~ Double) => a -> C.Render ()
renderC a = case render Postscript a of C r -> r

-- | Handle \"miscellaneous\" style attributes (clip, font stuff, fill
--   color and fill rule).
postscriptMiscStyle :: Style v Double -> C.Render ()
postscriptMiscStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle fFace
                , handle fSlant
                , handle fWeight
                , handle fSize
                , handle fLocal
                , handle fColor
                , handle fColorCMYK
                , handle lFillRule
                ]
  where
    handle :: AttributeClass a => (a -> C.Render ()) -> Maybe (C.Render ())
    handle f = f `fmap` getAttr s
    clip     = mapM_ (\p -> renderC p >> C.clip) . op Clip
    fSize    = assign (C.drawState . C.font . C.size) <$> (fromOutput . getFontSize)
    fLocal :: FontSize Double -> C.Render ()
    fLocal = assign (C.drawState . C.font . C.isLocal) <$> getFontSizeIsLocal
    fFace    = assign (C.drawState . C.font . C.face) <$> getFont
    fSlant   = assign (C.drawState . C.font . C.slant) .fromFontSlant <$> getFontSlant
    fWeight  = assign (C.drawState . C.font . C.weight) . fromFontWeight <$> getFontWeight
    fColor :: FillTexture Double -> C.Render ()
    fColor = C.fillColor . getFillTexture
    fColorCMYK c = C.fillColorCMYK (getFillColorCMYK c)
    lFillRule = assign (C.drawState . C.fillRule) . getFillRule

fromFontSlant :: FontSlant -> C.FontSlant
fromFontSlant FontSlantNormal   = C.FontSlantNormal
fromFontSlant FontSlantItalic   = C.FontSlantItalic
fromFontSlant FontSlantOblique  = C.FontSlantOblique

fromFontWeight :: FontWeight -> C.FontWeight
fromFontWeight FontWeightNormal = C.FontWeightNormal
fromFontWeight FontWeightBold   = C.FontWeightBold

postscriptStyle :: Style v Double -> C.Render ()
postscriptStyle s = sequence_ -- foldr (>>) (return ())
              . catMaybes $ [ handle fColor
                            , handle fColorCMYK
                            , handle lColor
                            , handle lColorCMYK
                            , handle lWidth
                            , handle lJoin
                            , handle lMiter
                            , handle lCap
                            , handle lDashing
                            ]
  where handle :: (AttributeClass a) => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        lColor :: LineTexture Double -> C.Render ()
        lColor = C.strokeColor . getLineTexture
        lColorCMYK = C.strokeColorCMYK . getLineColorCMYK
        fColor :: FillTexture Double -> C.Render ()
        fColor c = C.fillColor (getFillTexture c) >> C.fillPreserve
        fColorCMYK c = C.fillColorCMYK (getFillColorCMYK c) >> C.fillPreserve
        lWidth = C.lineWidth . fromOutput . getLineWidth
        lCap = C.lineCap . getLineCap
        lJoin = C.lineJoin . getLineJoin
        lMiter = C.miterLimit . getLineMiterLimit
        lDashing (getDashing -> Dashing ds offs) =
          C.setDash (map fromOutput ds) (fromOutput offs)

postscriptTransf :: Transformation V2 Double -> C.Render ()
postscriptTransf t = C.transform a1 a2 b1 b2 c1 c2
  where (V2 a1 a2) = apply t unitX
        (V2 b1 b2) = apply t unitY
        (V2 c1 c2) = transl t

instance Renderable (Segment Closed V2 Double) Postscript where
  render _ (Linear (OffsetClosed (V2 x y))) = C $ C.relLineTo x y
  render _ (Cubic (V2 x1 y1)
                  (V2 x2 y2)
                  (OffsetClosed (V2 x3 y3)))
    = C $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail V2 Double) Postscript where
  render _ t = flip withLine t $ renderT . lineSegments
    where
      renderT segs =
        C $ do
          mapM_ renderC segs
          when (isLoop t) C.closePath

          -- We need to ignore the fill if we see a line.
          -- Ignore fill is part of the drawing state, so
          -- it will be cleared by the `restore` after this
          -- primitive.
          when (isLine t) $ (C.drawState . C.ignoreFill) .= True

instance Renderable (Path V2 Double) Postscript where
  render _ p = C $ C.newPath >> F.mapM_ renderTrail (op Path p)
    where renderTrail (viewLoc -> (unp2 -> pt, tr)) = do
            uncurry C.moveTo pt
            renderC tr

instance Renderable (Text Double) Postscript where
  render _ (Text tt tn al str) = C $ do
      isLocal <- use (C.drawState . C.font . C.isLocal)
      let tr = if isLocal then tt else tn
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
-- > data family Options Postscript V2 Double = PostscriptOptions
-- >           { psfileName     :: String       -- ^ the name of the file you want generated
-- >           , psSizeSpec     :: SizeSpec2D   -- ^ the requested size of the output
-- >           , psOutputFormat :: OutputFormat -- ^ the output format and associated options
-- >           }
