{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}
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
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * A simple but somewhat inflexible approach is to wrap up
--   'defaultMain' (or 'multiMain', or 'animMain') in a call to
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
       , Postscript
       ) where

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.Postscript

import System.Console.CmdArgs.Implicit hiding (args)

import Prelude

import Data.Maybe          (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad       (when)
import Data.List.Split
import Data.List           (intercalate)

import System.Environment  (getArgs, getProgName)

data DiagramOpts = DiagramOpts
                   { width     :: Maybe Int
                   , height    :: Maybe Int
                   , output    :: FilePath
                   , selection :: Maybe String
                   , list      :: Bool
                   }
  deriving (Show, Data, Typeable)

diagramOpts :: String -> Bool -> DiagramOpts
diagramOpts prog sel = DiagramOpts
  { width =  def
             &= typ "INT"
             &= help "Desired width of the output image (default 400)"

  , height = def
             &= typ "INT"
             &= help "Desired height of the output image (default 400)"

  , output = def
           &= typFile
           &= help "Output file"

  , selection = def
              &= help "Name of the diagram to render"
              &= (if sel then typ "NAME" else ignore)

  , list = def
         &= (if sel then help "List all available diagrams" else ignore)
  }
  &= summary "Command-line diagram generation."
  &= program prog

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
--   -? --help              Display help message
--   -V --version           Print version information
-- @
--
--   For example, a couple common scenarios include
--
-- @
-- $ ghc --make MyDiagram
--
--   # output image.png with a width of 400pt (and auto-determined height)
-- $ ./MyDiagram -o image.eps -w 400
-- @

defaultMain :: Diagram Postscript R2 -> IO ()
defaultMain d = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog False)
  chooseRender opts (renderDia' d)

chooseRender :: DiagramOpts -> (Options Postscript R2 -> IO ()) -> IO ()
chooseRender opts render =
  case splitOn "." (output opts) of
    [""] -> putStrLn "No output file given."
    ps |  last ps `elem` ["eps"] 
       || last ps `elem` ["ps"] -> do
           let outfmt = case last ps of
                          _     -> EPS
               sizeSpec = case (width opts, height opts) of
                            (Nothing, Nothing) -> Absolute
                            (Just w, Nothing)  -> Width (fromIntegral w)
                            (Nothing, Just h)  -> Height (fromIntegral h)
                            (Just w, Just h)   -> Dims (fromIntegral w)
                                                       (fromIntegral h)

           render (PostscriptOptions (output opts) sizeSpec outfmt)
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
multiMain ds = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog True)
  if list opts
    then showDiaList (map fst ds)
    else
      case selection opts of
        Nothing  -> putStrLn "No diagram selected." >> showDiaList (map fst ds)
        Just sel -> case lookup sel ds of
          Nothing -> putStrLn $ "Unknown diagram: " ++ sel
          Just d  -> chooseRender opts (renderDia' d)

-- | Display the list of diagrams available for rendering.
showDiaList :: [String] -> IO ()
showDiaList ds = do
  putStrLn "Available diagrams:"
  putStrLn $ "  " ++ intercalate " " ds 

-- | @pagesMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams and each wil be rendered as a page
--   in the Postscript file.
--
--   Example usage:
-- @
-- $ ghc --make MultiPage
-- [1 of 1] Compiling Main             ( MultiPage.hs, MultiPage.o )
-- Linking MultiPage ...
-- $ ./MultiPage -o Pages.ps -w 200
-- @

pagesMain :: [Diagram Postscript R2] -> IO ()
pagesMain ds = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog False)
  chooseRender opts (renderDias' ds)

