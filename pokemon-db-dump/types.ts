type Pokemon = {
  name: string;
  abilities: Ability[];
  types: Type[];
  stats: Stat[];
};

type Ability = {
  ability: { name: string };
  slot: number;
  is_hidden: boolean;
};

type Type = {
  slot: number;
  type: { name: string };
};

type Stat = {
  base_stat: number;
};

type Species = {
  evolution_chain: { url: string };
  gender_rate: number;
};

type EvolutionChain = {
  chain: Chain;
};

type Chain = {
  evolution_details: EvolutionDetails[];
  evolves_to: Chain[];
  species: { name: string };
};

type EvolutionDetails = {
  gender?: string;
  held_item?: string;
  item?: string;
  known_move_type?: string;
  location?: string;
  min_affection?: number;
  min_beauty?: number;
  min_happinnes?: number;
  min_level?: number;
  needs_overworld_rain?: false;
  party_species?: string;
  party_type?: string;
  relative_physical_stats?: number;
  time_of_day?: string;
  trade_species?: string;
  turn_upside_down?: boolean;
  trigger: { name: "level-up" | "trade" | "use-item" | "shed" | "other" };
};

type PokemonInsert = {
  name: string;
  dexNumber: number;
  primaryAbility: string;
  secondaryAbility?: string;
  hiddenAbility?: string;
  primaryType: string;
  secondaryType?: string;
  maleProbability?: number;
  femaleProbability?: number;
  hp: number;
  attack: number;
  defense: number;
  specialAttack: number;
  specialDefense: number;
  speed: number;
  evolutions: EvolutionInsert[];
};

type EvolutionInsert = {
  name: string;
  method: EvolutionMethod;
};

type LevelUp = {
  level?: number;
  friendship?: number;
  move?: string;
  location?: string;
  time?: string;
  item?: string;
  gender?: string;
  upsideDown?: boolean;
  region?: string;
  type: "LEVEL_UP";
};

type UseItem = {
  item: String;
  gender?: String;
  region?: String;
  type: "USE_ITEM";
};

type Trade = {
  item?: String;
  pokemon?: String;
  type: "TRADE";
};

type EvolutionMethod = LevelUp | UseItem | Trade;
