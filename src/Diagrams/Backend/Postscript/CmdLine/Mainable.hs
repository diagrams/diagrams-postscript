{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Postscript.CmdLine.Mainable
-- Copyright   :  (c) 2013 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Postscript.CmdLine.Mainable
       ( DiagramOpts(..)
       , diagramOpts
       , width
       , height
       , output

       , DiagramMultiOpts(..)
       , diagramMultiOpts
       , selection
       , list

       , DiagramAnimOpts(..)
       , diagramAnimOpts
       , fpu

       , Parseable(..)
       , ToDiagram(..)
       , Mainable(..)
       ) where

import Diagrams.Prelude hiding (width, height, interval, option, value, (<>))

import Control.Lens hiding (argument)

import Options.Applicative hiding ((&))

import Prelude

import Data.Maybe          (fromMaybe)
import Control.Monad       (forM_)
import Control.Applicative ((<$>))
import Control.Monad       (when)
import Data.List.Split
import Data.List           (intercalate)
import Data.Typeable
import Data.Data

import Text.Printf

import System.Environment  (getArgs, getProgName)
import System.FilePath     (addExtension, splitExtension)


-- | Standard options most diagrams are likely to have.
data DiagramOpts = DiagramOpts
    { _width     :: Maybe Int
    , _height    :: Maybe Int
    , _output    :: FilePath
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramOpts

-- | Extra options for a program that can offer a choice
--   between multiple diagrams.
data DiagramMultiOpts = DiagramMultiOpts
    { _selection :: Maybe String
    , _list      :: Bool
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramMultiOpts

-- | Extra options for animations.
data DiagramAnimOpts = DiagramAnimOpts
    { _fpu :: Double
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramAnimOpts

-- | Parseable instances give a command line parser for a type.
--   If a custom parser for a common type is wanted a newtype
--   wrapper could be used to make a new 'Parseable' instance.
--   Notice that there are instances that we do /not/ want as
--   many instances as 'Read' because we want to limit ourselves
--   to things that make sense to parse from the command line.
class Parseable a where
    parser :: Parser a

-- The following instance would overlap with the product instance for
-- Parseable.  We can't tell if one wants to parse (a,b) as one argument or a
-- as one argument and b as another.  Since this is the command line we almost
-- certainly want the latter.  So we need to have less Read instances.
--
-- instance Read a => Parseable a where
--    parser = argument readMaybe mempty
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing

instance Parseable Int where
    parser = argument readMaybe mempty
   
instance Parseable Double where
    parser = argument readMaybe mempty

instance Parseable String where
    parser = argument Just mempty

instance Parseable DiagramOpts where
    parser = diagramOpts

instance Parseable DiagramMultiOpts where
    parser = diagramMultiOpts

instance Parseable DiagramAnimOpts where
    parser = diagramAnimOpts


-- This instance is needed to signal the end of a chain of
-- nested tuples.
instance Parseable () where
    parser = pure ()

-- Allow 'Parseable' things to be combined.
instance (Parseable a, Parseable b) => Parseable (a,b) where
    parser = (,) <$> parser <*> parser

-- Allow lists of 'Parseable'.
instance Parseable a => Parseable [a] where
    parser = many parser


-- | This class allows us to collect up arguments as well as things that can
--   produce a diagram and in turn produce a diagram.  This will let us write
--   something akin to curry and uncurry.
class ToDiagram d where
    type Args d :: *
    type DiagramResult d :: *

    toDiagram :: d -> Args d -> DiagramResult d

-- | A diagram can always produce a diagram when given '()' as an argument.
--   This is our base case.
instance ToDiagram (Diagram b v) where
    type Args (Diagram b v) = ()
    type DiagramResult (Diagram b v) = Diagram b v

    toDiagram d _ = d

-- | A function that, given some 'a', can produce a diagram producer 'd' is also
--   a diagram producer.  For this to work we need both the argument 'a' and
--   all the arguments that the diagram producer 'd' will need.  Producing the
--   diagram is simply applying the argument to the producer and passing the 
--   remaining arguments to the produced producer.
--
--   The previous paragraph stands as a witness to the fact that Haskell code
--   is clearer and easier to understand then paragraphs in English written by
--   me.
instance ToDiagram d => ToDiagram (a -> d) where
    type Args (a -> d) = (a, Args d)
    type DiagramResult (a -> d) = DiagramResult d

    toDiagram f (a,args) = toDiagram (f a) args


-- | This class represents the various ways we want to support diagram creation
--   from the command line.  It has the right instances to select between creating
--   single static diagrams, multiple static diagrams, static animations, and 
--   functions that produce diagrams as long as the arguments are 'Parseable'.
class Mainable d where
    type MainOpts d :: *

    -- TODO: can we get rid of the d argument here?
    mainArgs :: (Parseable a, Parseable (MainOpts d)) => d -> IO (MainOpts d, a)
    mainArgs _ = defaultOpts ((,) <$> parser <*> parser)

    mainRender :: MainOpts d -> d -> IO ()

    mainWith :: Parseable (MainOpts d) => d -> IO ()
    mainWith d = do
        (opts,()) <- mainArgs d
        mainRender opts d

instance (Parseable a, Parseable (Args d), ToDiagram d, Mainable (DiagramResult d)) => Mainable (a -> d) where
    type MainOpts (a -> d) = (MainOpts (DiagramResult (a -> d)), Args (a -> d))

    mainRender (opts, a) f  = mainRender opts (toDiagram f a)


diagramOpts :: Parser DiagramOpts
diagramOpts = DiagramOpts
    <$> (optional . option)
        ( long "width" <> short 'w'
       <> value 400
       <> metavar "WIDTH"
       <> help "Desired WIDTH of the output image (default 400)")
    <*> (optional . option)
        ( long "height" <> short 'h'
       <> value 400
       <> metavar "HEIGHT"
       <> help "Desired HEIGHT of the output image (default 400)")
    <*> strOption
        ( long "output" <> short 'o'
       <> value ""
       <> metavar "OUTPUT"
       <> help "OUTPUT file")

diagramMultiOpts :: Parser DiagramMultiOpts
diagramMultiOpts = DiagramMultiOpts
    <$> (optional . strOption)
        ( long "selection" <> short 's'
       <> metavar "NAME"
       <> help "NAME of the diagram to render")
    <*> switch
        ( long "list" <> short 'l'
       <> help "List all available diagrams")

diagramAnimOpts :: Parser DiagramAnimOpts
diagramAnimOpts = DiagramAnimOpts
    <$> option
        ( long "fpu" <> short 'f'
       <> value 30.0
       <> help "Frames per unit time (for animations)")

-- | A hidden \"helper\" option which always fails.
--   Taken from Options.Applicative.Extra but without the
--   short option 'h'.  We want the 'h' for Height.
helper' :: Parser (a -> a)
helper' = abortOption ShowHelpText $ mconcat
  [ long "help"
  , short '?'
  , help "Show this help text" 
  ]

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
    prog <- getProgName
    let p = info (helper' <*> optsParser)
                ( fullDesc
               <> progDesc "Command-line diagram generation."
               <> header prog)
    execParser p
