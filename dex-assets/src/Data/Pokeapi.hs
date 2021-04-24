{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Pokeapi where

import Data.Aeson
import GHC.Generics (Generic)

newtype PokeapiPokedex
  = PokeapiPokedex
  { entries :: [Entries]
  } deriving (Show, Eq, Generic)

data Entries
  = Entries
  { entryNumber :: Int
  , species :: Species
  }
  deriving (Show, Eq, Generic)

instance FromJSON PokeapiPokedex where
  parseJSON = withObject "pokeapi-pokedex" $ \o -> do
    entries <- o .: "pokemon_entries"
    return PokeapiPokedex { .. }

instance FromJSON Entries where
  parseJSON = withObject "entries" $ \o -> do
    entryNumber <- o .: "entry_number"
    species     <- o .: "pokemon_species"
    return Entries { .. }

newtype Species
  = Species
  { name :: String
  } deriving (Show, Eq, Generic)

instance FromJSON Species
