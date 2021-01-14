export type NamedAPIResource = {
  name: string;
  url: string;
};

export type Pokemon = {
  name: string;
  abilities: Ability[];
  types: Type[];
  stats: Stat[];
};

export type Ability = {
  ability: NamedAPIResource;
  slot: number;
  is_hidden: boolean;
};

export type Type = {
  slot: number;
  type: NamedAPIResource;
};

export type Stat = {
  base_stat: number;
};

export type Species = {
  evolution_chain: NamedAPIResource;
  gender_rate: number;
  generation: {
    name:
      | "generation-i"
      | "generation-ii"
      | "generation-iii"
      | "generation-iv"
      | "generation-v";
  };
};

export type EvolutionChain = {
  chain: Chain;
};

export type Chain = {
  evolution_details: EvolutionDetails[];
  evolves_to: Chain[];
  species: NamedAPIResource;
};

export type EvolutionDetails = {
  gender?: 1 | 2 | 3;
  held_item?: NamedAPIResource;
  item?: { name: string };
  known_move?: NamedAPIResource;
  known_move_type?: NamedAPIResource;
  location?: { name: string };
  min_affection?: number;
  min_beauty?: number;
  min_happiness?: number;
  min_level?: number;
  needs_overworld_rain?: false;
  party_species?: NamedAPIResource;
  party_type?: NamedAPIResource;
  relative_physical_stats?: number;
  time_of_day?: "day" | "night";
  trade_species?: NamedAPIResource;
  turn_upside_down?: boolean;
  trigger: { name: "level-up" | "trade" | "use-item" | "shed" | "other" };
};

export type PokemonInsert = {
  name: string;
  dexNumber: number;
  primaryAbility: string;
  secondaryAbility?: string;
  hiddenAbility?: string;
  evolutions: EvolutionInsert[];
  forms: Form[];
  gen: number;
};

export type EvolutionInsert = {
  name: string;
  method: EvolutionMethod;
};

export type LevelUp = {
  level?: number;
  friendship?: number;
  move?: string;
  moveType?: string;
  location?: string;
  time?: string;
  item?: string;
  gender?: string;
  upsideDown?: boolean;
  region?: string;
  type: "LEVEL_UP";
};

export type UseItem = {
  item: string;
  gender?: string;
  region?: string;
  type: "USE_ITEM";
};

export type Trade = {
  item?: string;
  pokemon?: string;
  type: "TRADE";
};

export type EvolutionMethod = LevelUp | UseItem | Trade;

export type GameTitle =
  | "rby"
  | "gsc"
  | "rse"
  | "frlg"
  | "dppt"
  | "hgss"
  | "bw"
  | "b2w2";

export type Form = {
  name: string;
  number: number;
  games?: GameTitle[];
  primaryAbility: string;
  secondaryAbility?: string;
  hiddenAbility?: string;
};
