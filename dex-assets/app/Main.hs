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
  { refreshSprites :: Bool
  , refreshIcons :: Bool
  , directory :: Maybe String
  , source :: Maybe String
  }

optionParser = do
  refreshSprites <- switch (long "refresh-sprites" <> help "Refresh pokeapi sprites sprites")
  refreshIcons   <- switch (long "refresh-icons" <> help "Refresh pokencyclopedia icons")
  directory      <- optional
    $ strOption (long "directory" <> short 'd' <> help "Destination directory for the sprites/ and icons/ directories")
  source <- optional $ strOption
    (long "source" <> short 's' <> help "Source directory containing pokencyclopedia-icons/ and pokeapi-sprites/ directories")
  return ProgramOptions { .. }

whenDirectoryExists :: (String -> IO ()) -> String -> IO ()
whenDirectoryExists f dir = do
  e <- doesDirectoryExist dir
  when e $ f dir

whenDirectoryDoesNotExist :: (String -> IO ()) -> String -> IO ()
whenDirectoryDoesNotExist f dir = do
  e <- doesDirectoryExist dir
  unless e $ f dir

ifDirectoryDoesNotExist :: String -> (FilePath -> IO ()) -> IO ()
ifDirectoryDoesNotExist dir f = do
  e <- doesDirectoryExist dir
  unless e $ f dir

clonePokeapiSprites :: FilePath -> IO ()
clonePokeapiSprites path = callCommand [i|git clone git@github.com:pokeapi/sprites.git #{path}|]

generateSprites :: ProgramOptions -> IO ()
generateSprites opts = do
  let spritesDir     = maybe "sprites" (++ "/sprites") (directory opts)
  let pokeapiSprites = maybe "pokeapi-sprites" (++ "/pokeapi-sprites") (source opts)
  when (refreshSprites opts) $ removeDirectoryRecursive `whenDirectoryExists` pokeapiSprites
  removeDirectoryRecursive `whenDirectoryExists` spritesDir
  clonePokeapiSprites `whenDirectoryDoesNotExist` pokeapiSprites
  createDirectory spritesDir
  pokedex <- entries <$> fetchNationalPokedex
  forM_ games $ \g@Game {..} -> do
    putStrLn [i|Generating #{key} sprites|]
    createDirectory [i|#{spritesDir}/#{key}|]
    let pokeapiDirectory = [i|#{pokeapiSprites}/sprites/pokemon/versions/generation-#{romanNumber}/#{folder}|] :: String
    files <- filter (applicable g) <$> listDirectory pokeapiDirectory
    forM_ files $ \file -> do
      let number                             = read . takeWhile isDigit $ file
      let Entries { species = Species {..} } = fromJust . find (\Entries {..} -> entryNumber == number) $ pokedex
      copyFile [i|#{pokeapiDirectory}/#{file}|] [i|#{spritesDir}/#{key}/#{name}.#{extension}|]
 where
  applicable Game {..} file =
    let trim = takeWhile isDigit file in not $ null trim || ((> cutoff) . (read :: String -> Int) $ trim)

generateIcons :: ProgramOptions -> IO ()
generateIcons ProgramOptions {..} = do
  let icons                = maybe "icons" (++ "/icons") directory
  let pokencyclopediaIcons = maybe "pokencyclopedia-icons" (++ "/pokencyclopedia-icons") source
  removeDirectoryRecursive `whenDirectoryExists` icons
  createDirectory `whenDirectoryDoesNotExist` icons
  when refreshIcons $ removeDirectoryRecursive pokencyclopediaIcons
  downloadPokencyclopediaIcons pokencyclopediaIcons
  forM_ [1 .. 5 :: Int] $ \gen -> do
    putStrLn [i|Generating gen #{gen} icons|]
    files <-
      map ([i|#{pokencyclopediaIcons}/gen#{gen}/|] ++) . sortOn (\xs -> (read $ takeWhile isDigit xs) :: Int) <$> listDirectory
        [i|#{pokencyclopediaIcons}/gen#{gen}|]
    callCommand [i|montage #{unwords files} -background none -geometry +0+0 #{icons}/gen#{gen}.png|]

downloadPokencyclopediaIcons :: FilePath -> IO ()
downloadPokencyclopediaIcons source = do
  createDirectory `whenDirectoryDoesNotExist` source
  createDirectory `whenDirectoryDoesNotExist` gifs
  createDirectory `whenDirectoryDoesNotExist` pngs
  pokedex <- entries <$> fetchNationalPokedex
  let pokemonPerGen = perGen pokedex
  forM_ pokemonPerGen $ \(gen, ps) -> ifDirectoryDoesNotExist [i|#{source}/gen#{gen}|] $ \dir -> do
    createDirectory dir
    forM_ ps $ \(number, name) -> do
      putStrLn [i|Downloading #{name} icons for gen #{gen}...|]
      let alternateForms = formNames <$> find (\WithAlternateForms { number = n, ..} -> gen `elem` gens && n == number) forms
      case alternateForms of
        Nothing -> doDownload dir gen (if gen `elem` [1, 2] then [i|#{pad number}|] else [i|#{pad number}_1.png|]) (show number)
        Just fs -> forM_ fs $ \(formName, key) -> doDownload dir gen key [i|#{number}-#{formName}|]
  removeDirectoryRecursive gifs
  removeDirectoryRecursive pngs
 where
  doDownload :: FilePath -> Int -> String -> FilePath -> IO ()
  doDownload dir gen key fileName = do
    let url        = iconUrl gen key
    let pathToFile = [i|#{folder gen}/#{fileName}.#{extension gen}|] :: String
    callCommand [i|wget -q #{url} -O #{pathToFile}|]
    if gen == 1 || gen == 2
      then do
        callCommand [i|convert -coalesce #{pathToFile} #{gifs}/#{fileName}.png|]
        renameFile [i|#{gifs}/#{fileName}-0.png|] [i|#{dir}/#{fileName}.png|]
      else renameFile [i|#{pngs}/#{fileName}.png|] [i|#{dir}/#{fileName}.png|]
    callCommand
      [i|convert #{dir}/#{fileName}.png -background none -gravity center -extent 32x32 -scale 64x64 #{dir}/#{fileName}.png|]

  gifs = [i|#{source}/gifs|]
  pngs = [i|#{source}/pngs|]

  perGen pokedex = do
    (cut, g) <- [(151, 1), (251, 2), (386, 3), (493, 4), (649, 5)]
    let ps = map (\Entries { entryNumber = n, species = Species {..} } -> (n, name)) . take cut $ pokedex
    return (g, ps)

  folder :: Int -> String
  folder 1 = gifs
  folder 2 = gifs
  folder _ = pngs

  extension :: Int -> String
  extension 1 = "gif"
  extension 2 = "gif"
  extension _ = "png"

  iconUrl :: Int -> String -> String
  iconUrl 1 name = [i|https://www.pokencyclopedia.info/sprites/menu-icons/ico-a_gb/ico-a_1_#{name}.gif|]
  iconUrl 2 name = [i|https://www.pokencyclopedia.info/sprites/menu-icons/ico-a_gbc/ico-a_2_#{name}.gif|]
  iconUrl 3 name = old name
  iconUrl 4 name = old name
  iconUrl 5 name = old name

  old :: String -> String
  old name = [i|https://www.pokencyclopedia.info/sprites/menu-icons/ico_old/ico_old_#{name}|]

pad :: Int -> String
pad n
  | n < 10    = [i|00#{n}|]
  | n < 100   = [i|0#{n}|]
  | otherwise = show n

main :: IO ()
main = do
  opts <- execParser $ info (optionParser <**> helper) (fullDesc <> header "dex-assets")
  generateSprites opts
  generateIcons opts
