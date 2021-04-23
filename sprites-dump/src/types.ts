export type Pokemon = {
  name: string;
  number: number;
};

export type Pokedex = {
  pokemon_entries: Array<{
    entry_number: number;
    pokemon_species: { name: string };
  }>;
};
