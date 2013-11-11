{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Postscript.CmdLine
-- Copyright   :  (c) 2013 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Postscript backend.
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'pagesMain' is like 'defaultMain' but renders a list of
--   diagrams as pages in a single file.
--
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * A simple but somewhat inflexible approach is to wrap up
--   'defaultMain' (or 'multiMain', or 'pagesMain') in a call to
--   'System.Environment.withArgs'.
--
-- * A more flexible approach is to directly call 'renderDia'; see
--   "Diagrams.Backend.Postscript" for more information.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Postscript.CmdLine
       ( defaultMain
       , multiMain
       , pagesMain
       , animMain
       , Postscript
       ) where

import Diagrams.Prelude hiding (width, height, interval, option, value, (<>))
import Diagrams.Backend.Postscript
import Diagrams.Backend.CmdLine

import Control.Lens hiding (argument)

import Options.Applicative hiding ((&))

import Prelude

import Data.Maybe          (fromMaybe)
import Control.Monad       (forM_)
import Control.Applicative ((<$>))
import Data.List.Split
import Data.List           (intercalate)

-- | This is the simplest way to render diagrams, and is intended to
--   be used like so:
--
-- > ... other definitions ...
-- > myDiagram = ...
-- >
-- > main = defaultMain myDiagram
--
--   Compiling a source file like the above example will result in an
--   executable which takes command-line options for setting the size,
--   output file, and so on, and renders @myDiagram@ with the
--   specified options.
--
--   Pass @--help@ to the generated executable to see all available
--   options.  Currently it looks something like
--
-- @
-- Command-line diagram generation.
--
-- Foo [OPTIONS]
--
-- Common flags:
--   -w --width=INT         Desired width of the output image
--   -h --height=INT        Desired height of the output image
--   -o --output=FILE       Output file
--   -f --fpu=FLOAT         Frames per unit time (for animations)
--   -? --help              Display help message
--   -V --version           Print version information
-- @
--
--   For example, a couple common scenarios include
--
-- @
-- $ ghc --make MyDiagram
--
--   # output image.eps with a width of 400pt (and auto-determined height)
-- $ ./MyDiagram -o image.eps -w 400
-- @

defaultMain :: Diagram Postscript R2 -> IO ()
defaultMain = mainWith

instance Mainable (Diagram Postscript R2) where
    type MainOpts (Diagram Postscript R2) = DiagramOpts

    mainRender opts d = chooseRender opts (renderDia' d)

chooseRender :: DiagramOpts -> (Options Postscript R2 -> IO ()) -> IO ()
chooseRender opts render =
  case splitOn "." (opts^.output) of
    [""] -> putStrLn "No output file given."
    ps |  last ps `elem` ["eps"] 
       || last ps `elem` ["ps"] -> do
           let outfmt = case last ps of
                          _     -> EPS
               sizeSpec = case (opts^.width, opts^.height) of
                            (Nothing, Nothing) -> Absolute
                            (Just w, Nothing)  -> Width (fromIntegral w)
                            (Nothing, Just h)  -> Height (fromIntegral h)
                            (Just w, Just h)   -> Dims (fromIntegral w)
                                                       (fromIntegral h)

           render (PostscriptOptions (opts^.output) sizeSpec outfmt)
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps
       
renderDias' :: [Diagram Postscript R2] -> Options Postscript R2 -> IO ()
renderDias' ds o = renderDias Postscript o ds >> return ()

renderDia' :: Diagram Postscript R2 -> Options Postscript R2 -> IO ()
renderDia' d o = renderDia Postscript o d >> return ()
       

-- | @multiMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams paired with names as input.
--   The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The
--   list of available diagrams may also be printed by passing the
--   option @--list@.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiTest
-- [1 of 1] Compiling Main             ( MultiTest.hs, MultiTest.o )
-- Linking MultiTest ...
-- $ ./MultiTest --list
-- Available diagrams:
--   foo bar
-- $ ./MultiTest --selection bar -o Bar.eps -w 200
-- @

multiMain :: [(String, Diagram Postscript R2)] -> IO ()
multiMain = mainWith

instance Mainable [(String,Diagram Postscript R2)] where
    type MainOpts [(String,Diagram Postscript R2)] = (DiagramOpts, DiagramMultiOpts)

    mainRender = defaultMultiMainRender

-- | @pagesMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams and each wil be rendered as a page
--   in the Postscript file.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiPage
-- [1 of 1] Compiling Main             ( MultiPage.hs, MultiPage.o )
-- Linking MultiPage ...
-- $ ./MultiPage -o Pages.ps -w 200
-- @

pagesMain :: [Diagram Postscript R2] -> IO ()
pagesMain = mainWith

instance Mainable [Diagram Postscript R2] where
    type MainOpts [Diagram Postscript R2] = DiagramOpts

    mainRender opts ds = chooseRender opts (renderDias' ds)

-- | @animMain@ is like 'defaultMain', but renders an animation
-- instead of a diagram.  It takes as input an animation and produces
-- a command-line program which will crudely \"render\" the animation
-- by rendering one image for each frame, named by extending the given
-- output file name by consecutive integers.  For example if the given
-- output file name is @foo\/blah.eps@, the frames will be saved in
-- @foo\/blah001.eps@, @foo\/blah002.eps@, and so on (the number of
-- padding digits used depends on the total number of frames).  It is
-- up to the user to take these images and stitch them together into
-- an actual animation format (using, /e.g./ @ffmpeg@).
--
--   Of course, this is a rather crude method of rendering animations;
--   more sophisticated methods will likely be added in the future.
--
-- The @--fpu@ option can be used to control how many frames will be
-- output for each second (unit time) of animation.
animMain :: Animation Postscript R2 -> IO ()
animMain = mainWith

instance Mainable (Animation Postscript R2) where
    type MainOpts (Animation Postscript R2) = (DiagramOpts, DiagramAnimOpts)

    mainRender = defaultAnimMainRender
