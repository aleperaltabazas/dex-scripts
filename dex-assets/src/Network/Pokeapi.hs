{-# LANGUAGE OverloadedStrings #-}

module Network.Pokeapi
  ( fetchNationalPokedex
  )
where

import Data.Pokeapi (PokeapiPokedex)
import Network.HTTP.Req

fetchNationalPokedex :: IO PokeapiPokedex
fetchNationalPokedex = runReq defaultHttpConfig $ do
  res <- req GET (https "pokeapi.co" /: "/api" /: "v2" /: "pokedex" /: "national") NoReqBody jsonResponse mempty
  return $ responseBody res
