{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad
import Data.Char
import Data.Functor.Identity
import Data.Game
import Data.List
import Data.Maybe
import Data.Pokeapi
import Data.String.Interpolate (i)
import Network.Pokeapi
import Options.Applicative
import System.Directory
import System.Process
import Text.Regex.Posix

games = [yellow, crystal, emerald, fireRedLeafGreen, diamondPearl, platinum, heartGoldSoulSilver, blackWhite]

data ProgramOptions
  = ProgramOptions
  { refreshSprites :: Bool
  , refreshIcons :: Bool
  , directory :: Maybe String
  , source :: Maybe String
  , onlyIcons :: Bool
  , onlySprites :: Bool
  }

optionParser = do
  refreshSprites <- switch (long "refresh-sprites" <> help "Refresh pokeapi sprites sprites")
  refreshIcons   <- switch (long "refresh-icons" <> help "Refresh pokencyclopedia icons")
  directory      <- optional
    $ strOption (long "directory" <> short 'd' <> help "Destination directory for the sprites/ and icons/ directories")
  source <- optional $ strOption
    (long "source" <> short 's' <> help "Source directory containing pokencyclopedia-icons/ and pokeapi-sprites/ directories")
  onlyIcons   <- switch (long "only-icons" <> help "Generate only the icons folder")
  onlySprites <- switch (long "only-sprites" <> help "Generate only the sprites folder")
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
  pokedex <- entries <$> fetchNationalPokedex
  forM_ [1 .. 5 :: Int] $ \gen -> do
    putStrLn [i|Generating gen #{gen} icons|]
    (nonForms, forms) <- partition isRegularForm . sortByNumber <$> listDirectory [i|#{pokencyclopediaIcons}/gen#{gen}|]
    unless (null nonForms) $ doGenerateIcons pokedex pokencyclopediaIcons gen icons [i|gen#{gen}|] nonForms
    unless (null forms) $ doGenerateIcons pokedex pokencyclopediaIcons gen icons [i|gen#{gen}-forms|] forms

 where
  mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
  mapWithIndex f xs = let idxs = [0 .. length xs] in zipWith f idxs xs

  generateCss :: [Entries] -> Int -> FilePath -> FilePath -> [FilePath] -> IO ()
  generateCss pokedex gen icons fileName files = do
    let
      css = flip mapWithIndex files $ \idx f ->
        let
          number                             = numericalPart f
          Entries { species = Species {..} } = pokedex !! (number - 1)
          className                          = if isRegularForm f
            then name
            else let formName = dropRight (length ".png") . drop (length $ show number) $ f in [i|#{name}#{formName}|]
          x = (idx `rem` 30) * 64
          y = (idx `quot` 30) * 64
        in [i|.#{className}-gen#{gen}{background-position-x:-#{x}px;background-position-y:-#{y}px;}|] :: String
    let backgroundFile = reverse . takeWhile (/= '/') . reverse $ fileName
    let backgroundCss  = [i|.icon-#{fileName}{background: url(#{fileName}.png);}|] :: String
    writeFile [i|#{icons}/#{fileName}.css|] $ unlines (backgroundCss : css)

  doGenerateIcons :: [Entries] -> FilePath -> Int -> FilePath -> FilePath -> [FilePath] -> IO ()
  doGenerateIcons pokedex pokencyclopediaIcons gen icons fileName files = do
    let pokencyclopediaFiles = map ([i|#{pokencyclopediaIcons}/gen#{gen}/|] ++) files :: [String]
    callCommand [i|montage #{unwords pokencyclopediaFiles} -background none -tile 30x -geometry +0+0 #{icons}/#{fileName}.png|]
    generateCss pokedex gen icons fileName files

  sortByNumber = sortOn numericalPart

  numericalPart :: String -> Int
  numericalPart = read . takeWhile isDigit

isRegularForm :: String -> Bool
isRegularForm = (=~ "[0-9]+.png")

downloadPokencyclopediaIcons :: [Entries] -> FilePath -> IO ()
downloadPokencyclopediaIcons pokedex source = do
  createDirectory `whenDirectoryDoesNotExist` source
  createDirectory `whenDirectoryDoesNotExist` gifs
  createDirectory `whenDirectoryDoesNotExist` pngs
  let pokemonPerGen = perGen pokedex
  forM_ pokemonPerGen $ \(gen, ps) -> ifDirectoryDoesNotExist [i|#{source}/gen#{gen}|] $ \dir -> do
    createDirectory dir
    forM_ ps $ \(number, name) -> do
      putStrLn [i|Downloading #{name} icons for gen #{gen}...|]
      let alternateForms = formNames <$> find (\WithAlternateForms { number = n, ..} -> gen `elem` gens && n == number) forms
      case alternateForms of
        Nothing -> doDownload dir gen (if gen `elem` [1, 2] then [i|#{pad number}|] else [i|#{pad number}_1.png|]) (show number)
        Just fs -> do
          (\(_, key) -> doDownload dir gen key $ show number) $ head fs
          forM_ fs $ \(formName, key) -> doDownload dir gen key [i|#{number}-#{formName}|]
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
  opts@ProgramOptions {..} <- execParser $ info (optionParser <**> helper) (fullDesc <> header "dex-assets")
  unless onlyIcons $ generateSprites opts
  unless onlySprites $ generateIcons opts

dropRight :: Int -> [a] -> [a]
dropRight n = reverse . drop n . reverse
