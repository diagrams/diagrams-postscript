{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Postscript.CmdLine
-- Copyright   :  (c) 2011 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Postscript backend.
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

import System.Environment  (getArgs, getProgName)

data DiagramOpts = DiagramOpts
                   { width     :: Maybe Int
                   , height    :: Maybe Int
                   , output    :: FilePath
                   , selection :: Maybe String
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
  }
  &= summary "Command-line diagram generation."
  &= program prog

defaultMain :: Diagram Postscript R2 -> IO ()
defaultMain d = do
  prog <- getProgName
  args <- getArgs
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
       
pagesMain :: [Diagram Postscript R2] -> IO ()
pagesMain ds = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog True)
  chooseRender opts (renderDias' ds)

multiMain :: [(String, Diagram Postscript R2)] -> IO ()
multiMain ds = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog True)
  case selection opts of
    Nothing  -> chooseRender opts (renderDias' (map snd ds)) -- putStrLn "No diagram selected."
    Just sel -> case lookup sel ds of
      Nothing -> putStrLn $ "Unknown diagram: " ++ sel
      Just d  -> chooseRender opts (renderDia' d)


