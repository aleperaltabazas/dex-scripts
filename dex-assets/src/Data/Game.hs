module Data.Game
  ( Game(..)
  , yellow
  , crystal
  , emerald
  , fireRedLeafGreen
  , platinum
  , diamondPearl
  , heartGoldSoulSilver
  , blackWhite
  )
where

data Game
  = Game
  { romanNumber :: String
  , gen :: Int
  , folder :: String
  , cutoff :: Int
  , extension :: String
  , key :: String
  } deriving (Show, Eq)

yellow = Game { romanNumber = "i", gen = 1, folder = "yellow/transparent", cutoff = 151, extension = "png", key = "rby" }
crystal = Game { romanNumber = "ii", gen = 2, folder = "crystal/transparent", cutoff = 251, extension = "png", key = "gsc" }
emerald = Game { romanNumber = "iii", gen = 3, folder = "emerald", cutoff = 386, extension = "png", key = "rse" }
fireRedLeafGreen =
  Game { romanNumber = "iii", gen = 3, folder = "firered-leafgreen", cutoff = 386, extension = "png", key = "frlg" }
platinum = Game { romanNumber = "iv", gen = 4, folder = "platinum", cutoff = 493, extension = "png", key = "pt" }
diamondPearl = Game { romanNumber = "iv", gen = 4, folder = "diamond-pearl", cutoff = 493, extension = "png", key = "dp" }
heartGoldSoulSilver =
  Game { romanNumber = "iv", gen = 4, folder = "heartgold-soulsilver", cutoff = 493, extension = "png", key = "hgss" }
blackWhite = Game { romanNumber = "v", gen = 5, folder = "black-white/animated", cutoff = 649, extension = "gif", key = "bw" }
