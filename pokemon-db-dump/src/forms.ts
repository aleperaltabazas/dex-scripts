import { Form } from "./types";

function unowns(): Form[] {
  const alphabet = "abcdefghijklmnopqrstuvwxyz".split("");

  return alphabet
    .map((c) => ({
      name: `unown-${c}`,
      number: 201,
      primaryAbility: "levitate",
    }))
    .concat({
      name: "unown-exclamation",
      number: 201,
      primaryAbility: "levitate",
    })
    .concat({
      name: "unown-question",
      number: 201,
      primaryAbility: "levitate",
    });
}

function arceus(): Form[] {
  const types = [
    "bug",
    "dark",
    "dragon",
    "electric",
    "fairy",
    "fighting",
    "fire",
    "flying",
    "ghost",
    "grass",
    "ground",
    "ice",
    "normal",
    "poison",
    "psychic",
    "rock",
    "steel",
    "water",
  ];

  return types.map((t) => ({
    name: `arceus-${t}`,
    number: 493,
    primaryAbility: "multitype",
  }));
}

const forms: Form[] = [
  {
    name: "spiky-eared-pichu",
    number: 172,
    games: ["hgss"],
    primaryAbility: "static",
  },
  ...unowns(),
  {
    name: "castform-normal",
    number: 351,
    primaryAbility: "forecast",
  },
  {
    name: "castform-sunny",
    number: 351,
    primaryAbility: "forecast",
  },
  {
    name: "castform-rainy",
    number: 351,
    primaryAbility: "forecast",
  },
  {
    name: "castform-snowy",
    number: 351,
    primaryAbility: "forecast",
  },
  {
    name: "deoxys-normal",
    number: 386,
    primaryAbility: "pressure",
  },
  {
    name: "deoxys-attack",
    number: 386,
    primaryAbility: "pressure",
  },
  {
    name: "deoxys-defense",
    number: 386,
    primaryAbility: "pressure",
  },
  {
    name: "deoxys-speed",
    number: 386,
    primaryAbility: "pressure",
  },
  {
    name: "burmy-plant",
    number: 412,
    primaryAbility: "shed-skin",
    hiddenAbility: "overcoat",
  },
  {
    name: "burmy-sandy",
    number: 412,
    primaryAbility: "shed-skin",
    hiddenAbility: "overcoat",
  },
  {
    name: "burmy-trash",
    number: 412,
    primaryAbility: "shed-skin",
    hiddenAbility: "overcoat",
  },
  {
    name: "wormadam-plant",
    number: 413,
    primaryAbility: "anticipation",
    hiddenAbility: "overcoat",
  },
  {
    name: "wormadam-sandy",
    number: 413,
    primaryAbility: "anticipation",
    hiddenAbility: "overcoat",
  },
  {
    name: "wormadam-trash",
    number: 413,
    primaryAbility: "anticipation",
    hiddenAbility: "overcoat",
  },
  {
    name: "cherrim-overcast",
    number: 421,
    primaryAbility: "flower-gift",
  },
  {
    name: "cherrim-sunshine",
    number: 421,
    primaryAbility: "flower-gift",
  },
  {
    name: "shellos-west",
    number: 422,
    primaryAbility: "sticky-hold",
    secondaryAbility: "storm-drain",
    hiddenAbility: "sand-force",
  },
  {
    name: "shellos-east",
    number: 422,
    primaryAbility: "sticky-hold",
    secondaryAbility: "storm-drain",
    hiddenAbility: "sand-force",
  },
  {
    name: "gastrodon-west",
    number: 423,
    primaryAbility: "sticky-hold",
    secondaryAbility: "storm-drain",
    hiddenAbility: "sand-force",
  },
  {
    name: "gastrodon-east",
    number: 423,
    primaryAbility: "sticky-hold",
    secondaryAbility: "storm-drain",
    hiddenAbility: "sand-force",
  },
  {
    name: "rotom-normal",
    number: 479,
    primaryAbility: "levitate",
  },
  {
    name: "rotom-heat",
    number: 479,
    primaryAbility: "levitate",
  },
  {
    name: "rotom-wash",
    number: 479,
    primaryAbility: "levitate",
  },
  {
    name: "rotom-frost",
    number: 479,
    primaryAbility: "levitate",
  },
  {
    name: "rotom-fan",
    number: 479,
    primaryAbility: "levitate",
  },
  {
    name: "rotom-mow",
    number: 479,
    primaryAbility: "levitate",
  },
  {
    name: "giratina-altered",
    number: 487,
    primaryAbility: "pressure",
    hiddenAbility: "telepathy",
  },
  {
    name: "giratina-origin",
    number: 487,
    primaryAbility: "levitate",
  },
  {
    name: "shaymin-land",
    number: 492,
    primaryAbility: "natural-cure",
  },
  {
    name: "shaymin-sky",
    number: 492,
    primaryAbility: "serene-grace",
  },
  ...arceus(),
  {
    name: "basculin-red-striped",
    number: 550,
    primaryAbility: "reckless",
    secondaryAbility: "adaptability",
    hiddenAbility: "mold-breaker",
  },
  {
    name: "basculin-blue-striped",
    number: 550,
    primaryAbility: "rock-head",
    secondaryAbility: "adaptability",
    hiddenAbility: "mold-breaker",
  },
  {
    name: "darmanitan-standard",
    number: 555,
    primaryAbility: "sheer-force",
    hiddenAbility: "zen-mode",
  },
  {
    name: "darmanitan-zen",
    number: 555,
    primaryAbility: "sheer-force",
    hiddenAbility: "zen-mode",
  },
  {
    name: "deerling-spring",
    number: 585,
    primaryAbility: "chlorophyll",
    secondaryAbility: "sap-sipper",
    hiddenAbility: "serene-grace",
  },
  {
    name: "deerling-summer",
    number: 585,
    primaryAbility: "chlorophyll",
    secondaryAbility: "sap-sipper",
    hiddenAbility: "serene-grace",
  },
  {
    name: "deerling-autumun",
    number: 585,
    primaryAbility: "chlorophyll",
    secondaryAbility: "sap-sipper",
    hiddenAbility: "serene-grace",
  },
  {
    name: "deerling-winter",
    number: 585,
    primaryAbility: "chlorophyll",
    secondaryAbility: "sap-sipper",
    hiddenAbility: "serene-grace",
  },
  {
    name: "sawsbuck-spring",
    number: 586,
    primaryAbility: "chlorophyll",
    secondaryAbility: "sap-sipper",
    hiddenAbility: "serene-grace",
  },
  {
    name: "sawsbuck-summer",
    number: 586,
    primaryAbility: "chlorophyll",
    secondaryAbility: "sap-sipper",
    hiddenAbility: "serene-grace",
  },
  {
    name: "sawsbuck-autumun",
    number: 586,
    primaryAbility: "chlorophyll",
    secondaryAbility: "sap-sipper",
    hiddenAbility: "serene-grace",
  },
  {
    name: "sawsbuck-winter",
    number: 586,
    primaryAbility: "chlorophyll",
    secondaryAbility: "sap-sipper",
    hiddenAbility: "serene-grace",
  },
  {
    name: "tornadus-incarnate",
    number: 641,
    primaryAbility: "prankster",
    hiddenAbility: "defiant",
  },
  {
    name: "tornadus-therian",
    number: 641,
    primaryAbility: "regenerator",
  },
  {
    name: "thundurus-incarnate",
    number: 642,
    primaryAbility: "prankster",
    hiddenAbility: "defiant",
  },
  {
    name: "thundurus-therian",
    number: 642,
    primaryAbility: "volt-absorb",
  },
  {
    name: "landorus-incarnate",
    number: 641,
    primaryAbility: "sand-force",
    hiddenAbility: "sheer-force",
  },
  {
    name: "landorus-therian",
    number: 641,
    primaryAbility: "intimidate",
  },
  {
    name: "kyurem-normal",
    number: 646,
    primaryAbility: "pressure",
  },
  {
    name: "kyurem-white",
    number: 646,
    primaryAbility: "turboblaze",
  },
  {
    name: "kyurem-black",
    number: 646,
    primaryAbility: "teravolt",
  },
  {
    name: "keldeo-ordinary",
    number: 647,
    primaryAbility: "justified",
  },
  {
    name: "keldeo-resolute",
    number: 647,
    primaryAbility: "justified",
  },
  {
    name: "meloetta-aria",
    number: 648,
    primaryAbility: "serene-grace",
  },
  {
    name: "meloetta-pirouette",
    number: 648,
    primaryAbility: "serene-grace",
  },
  {
    name: "genesect-shock",
    number: 649,
    primaryAbility: "download",
  },
  {
    name: "genesect-burn",
    number: 649,
    primaryAbility: "download",
  },
  {
    name: "genesect-chill",
    number: 649,
    primaryAbility: "download",
  },
  {
    name: "genesect-douse",
    number: 649,
    primaryAbility: "download",
  },
];

export default forms;
