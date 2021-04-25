
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
  , WithAlternateForms(..)
  , forms
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
  , extent :: Int
  } deriving (Show, Eq)

data WithAlternateForms
  = WithAlternateForms
  { number :: Int
  , gens :: [Int]
  , formNames :: [(String, String)]
  }

yellow =
  Game { romanNumber = "i", gen = 1, folder = "yellow/transparent", cutoff = 151, extension = "png", key = "rby", extent = 64 }
crystal =
  Game { romanNumber = "ii", gen = 2, folder = "crystal/transparent", cutoff = 251, extension = "png", key = "gsc", extent = 64 }
emerald = Game { romanNumber = "iii", gen = 3, folder = "emerald", cutoff = 386, extension = "png", key = "rse", extent = 32 }
fireRedLeafGreen =
  Game { romanNumber = "iii", gen = 3, folder = "firered-leafgreen", cutoff = 386, extension = "png", key = "frlg", extent = 32 }
platinum = Game { romanNumber = "iv", gen = 4, folder = "platinum", cutoff = 493, extension = "png", key = "pt", extent = 32 }
diamondPearl =
  Game { romanNumber = "iv", gen = 4, folder = "diamond-pearl", cutoff = 493, extension = "png", key = "dp", extent = 32 }
heartGoldSoulSilver = Game
  { romanNumber = "iv"
  , gen         = 4
  , folder      = "heartgold-soulsilver"
  , cutoff      = 493
  , extension   = "png"
  , key         = "hgss"
  , extent      = 32
  }
blackWhite =
  Game { romanNumber = "v", gen = 5, folder = "black-white/animated", cutoff = 649, extension = "gif", key = "bw", extent = 32 }

unownForms =
  map (tupled . (: [])) ['a' .. 'z'] ++ [("exclamation", "201--exclamation_1.png"), ("question", "201--question_1.png")]
  where tupled name = (name, "201-" ++ name ++ "_1.png")

forms =
  [ WithAlternateForms { number = 201, gens = [3, 4, 5], formNames = unownForms }
  , WithAlternateForms
    { number    = 386
    , gens      = [3, 4, 5]
    , formNames =
      [("normal", "386_1.png"), ("attack", "386-attack_1.png"), ("defense", "386-defense_1.png"), ("speed", "386-speed_1.png")]
    }
  , WithAlternateForms
    { number    = 412
    , gens      = [4, 5]
    , formNames = [("plant", "412-plant_1.png"), ("sandy", "412-sandy_1.png"), ("trash", "412-trash_1.png")]
    }
  , WithAlternateForms
    { number    = 413
    , gens      = [4, 5]
    , formNames = [("plant", "413-plant_1.png"), ("sandy", "413-sandy_1.png"), ("trash", "413-trash_1.png")]
    }
  , WithAlternateForms
    { number    = 421
    , gens      = [4, 5]
    , formNames = [("overcast", "421-overcast_1.png"), ("sunshine", "421-sunshine_1.png")]
    }
  , WithAlternateForms { number = 422, gens = [4, 5], formNames = [("west", "422-west_1.png"), ("east", "422-east_1.png")] }
  , WithAlternateForms { number = 423, gens = [4, 5], formNames = [("west", "423-west_1.png"), ("east", "423-east_1.png")] }
  , WithAlternateForms
    { number    = 479
    , gens      = [4, 5]
    , formNames =
      [ ("normal", "479_1.png")
      , ("heat"  , "479-heat_1.png")
      , ("wash"  , "479-wash_1.png")
      , ("frost" , "479-frost_1.png")
      , ("fan"   , "479-fan_1.png")
      , ("mow"   , "479-mow_1.png")
      ]
    }
  , WithAlternateForms
    { number    = 487
    , gens      = [4, 5]
    , formNames = [("altered", "487-altered_1.png"), ("origin", "487-origin_1.png")]
    }
  , WithAlternateForms { number = 492, gens = [4, 5], formNames = [("land", "492-land_1.png"), ("sky", "492-sky_1.png")] }
  , WithAlternateForms { number = 521, gens = [5], formNames = [("male", "521_m-1.png"), ("female", "521_f-1.png")] }
  , WithAlternateForms
    { number    = 550
    , gens      = [5]
    , formNames = [("red-striped", "550-red-striped_1.png"), ("blue-striped", "550-blue-striped_1.png")]
    }
  , WithAlternateForms { number = 555, gens = [5], formNames = [("standard", "555-standard_1.png"), ("zen", "555-zen_1.png")] }
  , WithAlternateForms
    { number    = 585
    , gens      = [5]
    , formNames =
      [ ("spring", "585-spring_1.png")
      , ("summer", "585-summer_1.png")
      , ("autumn", "585-autumn_1.png")
      , ("winter", "585-winter_1.png")
      ]
    }
  , WithAlternateForms
    { number    = 586
    , gens      = [5]
    , formNames =
      [ ("spring", "586-spring_1.png")
      , ("summer", "586-summer_1.png")
      , ("autumn", "586-autumn_1.png")
      , ("winter", "586-winter_1.png")
      ]
    }
  , WithAlternateForms { number = 592, gens = [5], formNames = [("male", "592_m-1.png"), ("female", "592_f-1.png")] }
  , WithAlternateForms { number = 593, gens = [5], formNames = [("male", "593_m-1.png"), ("female", "593_f-1.png")] }
  , WithAlternateForms
    { number    = 641
    , gens      = [5]
    , formNames = [("incarnate", "641-incarnate_1.png"), ("therian", "641-therian_1.png")]
    }
  , WithAlternateForms
    { number    = 642
    , gens      = [5]
    , formNames = [("incarnate", "642-incarnate_1.png"), ("therian", "642-therian_1.png")]
    }
  , WithAlternateForms
    { number    = 645
    , gens      = [5]
    , formNames = [("incarnate", "645-incarnate_1.png"), ("therian", "645-therian_1.png")]
    }
  , WithAlternateForms
    { number    = 646
    , gens      = [5]
    , formNames = [("normal", "646_1.png"), ("black", "646-black_1.png"), ("white", "646-white_1.png")]
    }
  , WithAlternateForms
    { number    = 647
    , gens      = [5]
    , formNames = [("ordinary", "647-ordinary_1.png"), ("resolute", "647-resolute_1.png")]
    }
  , WithAlternateForms
    { number    = 648
    , gens      = [5]
    , formNames = [("aria", "648-aria_1.png"), ("pirouette", "648-pirouette_1.png")]
    }
  ]
