{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad
import Data.Char
import Data.Game
import Data.List
import Data.Maybe
import Data.Pokeapi
import Data.String.Interpolate (i)
import Network.Pokeapi
import Options.Applicative
import System.Directory
import System.Process

games = [yellow, crystal, emerald, fireRedLeafGreen, diamondPearl, platinum, heartGoldSoulSilver, blackWhite]

data ProgramOptions
  = ProgramOptions
  { refresh :: Bool
  , directory :: Maybe String
  }

optionParser = do
  refresh   <- switch (long "refresh" <> short 'r' <> help "Refresh pokeapi sprites")
  directory <- optional $ strOption (long "directory" <> short 'd' <> help "Destination directory")
  return ProgramOptions { .. }

whenDirectoryExists :: (String -> IO ()) -> String -> IO ()
whenDirectoryExists f dir = do
  e <- doesDirectoryExist dir
  when e $ f dir

whenDirectoryDoesNotExist :: (String -> IO ()) -> String -> IO ()
whenDirectoryDoesNotExist f dir = do
  e <- doesDirectoryExist dir
  unless e $ f dir

clonePokeapiSprites :: FilePath -> IO ()
clonePokeapiSprites path = callCommand [i|git clone git@github.com:pokeapi/sprites.git #{path}|]

main :: IO ()
main = do
  opts <- execParser $ info (optionParser <**> helper) (fullDesc <> header "dex-assets")
  let spritesDir     = fromMaybe "sprites" (directory opts)
  let pokeapiSprites = "pokeapi-sprites"
  when (refresh opts) $ removeDirectoryRecursive `whenDirectoryExists` pokeapiSprites
  removeDirectoryRecursive `whenDirectoryExists` spritesDir
  clonePokeapiSprites `whenDirectoryDoesNotExist` pokeapiSprites
  createDirectory spritesDir
  pokedex <- entries <$> fetchNationalPokedex
  forM_ games $ \g@Game {..} -> do
    createDirectory [i|sprites/#{key}|]
    let pokeapiDirectory = [i|pokeapi-sprites/sprites/pokemon/versions/generation-#{romanNumber}/#{folder}|] :: String
    files <- filter (applicable g) <$> listDirectory pokeapiDirectory
    forM_ files $ \file -> do
      let number                             = read . takeWhile isDigit $ file
      let Entries { species = Species {..} } = fromJust . find (\Entries {..} -> entryNumber == number) $ pokedex
      copyFile [i|#{pokeapiDirectory}/#{file}|] [i|sprites/#{key}/#{name}.#{extension}|]
 where
  applicable Game {..} file =
    let trim = takeWhile isDigit file in not $ null trim || ((> cutoff) . (read :: String -> Int) $ trim)
