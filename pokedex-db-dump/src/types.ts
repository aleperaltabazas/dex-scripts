export type BasePokedex =
  | {
      name: string;
      type: "NATIONAL";
      displayName: string;
      region: string;
      gen: number;
      cutoff: number;
    }
  | {
      name: string;
      type: "REGIONAL";
      displayName: string;
      region: string;
      gen: number;
      pokeapiName: string;
    };

export type PokeapiPokedex = {
  pokemon_entries: Array<PokeapiEntry>;
};

export type PokeapiEntry = {
  entry_number: number;
  pokemon_species: {
    name: string;
  };
};

export type Entry = { name: string; number: number };

export type Pokedex = {
  name: string;
  display_name: string;
  region: string;
  gen: number;
  type: "NATIONAL" | "REGIONAL";
  entries: Array<Entry>;
};

export type DbOptions = {
  name: string;
  user: string;
  password: string;
  host: string;
};
