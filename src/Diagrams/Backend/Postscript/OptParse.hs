{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Postscript.OptParse
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

module Diagrams.Backend.Postscript.OptParse
       ( defaultMain
       , multiMain
       , pagesMain
       , animMain
       , Postscript
       ) where

import Diagrams.Prelude hiding (width, height, interval, option, value, (<>))
import Diagrams.Backend.Postscript

import Options.Applicative

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

data DiagramOpts = DiagramOpts
                   { width     :: Maybe Int
                   , height    :: Maybe Int
                   , output    :: FilePath
                   , selection :: Maybe String
                   , list      :: Bool
                   , fpu       :: Double
                   }
  deriving (Show, Data, Typeable)

diagramOpts :: Bool -> Parser DiagramOpts
diagramOpts sel = DiagramOpts
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
       <> metavar "OUTPUT"
       <> help "OUTPUT file")
    <*> (if sel
          then (optional . strOption)
                ( long "selection" <> short 's'
               <> metavar "NAME"
               <> help "NAME of the diagram to render")
          else pure Nothing)
    <*> (if sel
          then switch
                ( long "list" <> short 'l'
               <> help "List all available diagrams")
          else pure False)
    <*> option
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

defaultOpts :: Bool -> IO DiagramOpts
defaultOpts sel = do
    prog <- getProgName
    let p = info (helper' <*> diagramOpts sel)
                ( fullDesc
               <> progDesc "Command-line diagram generation."
               <> header prog)
    execParser p

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
defaultMain d = do
    opts <- defaultOpts False
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
  opts <- defaultOpts True
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
--
-- @
-- $ ghc --make MultiPage
-- [1 of 1] Compiling Main             ( MultiPage.hs, MultiPage.o )
-- Linking MultiPage ...
-- $ ./MultiPage -o Pages.ps -w 200
-- @

pagesMain :: [Diagram Postscript R2] -> IO ()
pagesMain ds = do
  opts <- defaultOpts False
  chooseRender opts (renderDias' ds)

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
animMain anim = do
  opts <- defaultOpts False
  let frames  = simulate (toRational $ fpu opts) anim
      nDigits = length . show . length $ frames
  forM_ (zip [1..] frames) $ \(i,d) ->
    chooseRender (indexize nDigits i opts) (renderDia' d)

-- | @indexize d n@ adds the integer index @n@ to the end of the
--   output file name, padding with zeros if necessary so that it uses
--   at least @d@ digits.
indexize :: Int -> Integer -> DiagramOpts -> DiagramOpts
indexize nDigits i opts = opts { output = output' }
  where fmt         = "%0" ++ show nDigits ++ "d"
        output'     = addExtension (base ++ printf fmt (i::Integer)) ext
        (base, ext) = splitExtension (output opts)
