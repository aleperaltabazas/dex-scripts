import axios, { AxiosRequestConfig } from "axios";
import {
  Chain,
  EvolutionChain,
  EvolutionMethod,
  GameTitle,
  Pokemon,
  PokemonInsert,
  Species,
} from "./types";
import formList from "./forms";

if (process.argv.length < 4) {
  console.error("Error: missing arguments 'first' and 'last'");
}

async function fetchPokemon(dexNumber: number) {
  let config: AxiosRequestConfig = {
    method: "GET",
    url: `https://pokeapi.co/api/v2/pokemon/${dexNumber}`,
  };

  return axios.request<Pokemon>(config).then((res) => res.data);
}

async function fetchSpecies(dexNumber: number) {
  let config: AxiosRequestConfig = {
    method: "GET",
    url: `https://pokeapi.co/api/v2/pokemon-species/${dexNumber}`,
  };

  return axios.request<Species>(config).then((res) => res.data);
}

async function fetchEvolutionChain(species: Species) {
  let config: AxiosRequestConfig = {
    method: "GET",
    url: species.evolution_chain.url,
  };

  return axios.request<EvolutionChain>(config).then((res) => res.data);
}

function fromGenerationString(
  gen:
    | "generation-i"
    | "generation-ii"
    | "generation-iii"
    | "generation-iv"
    | "generation-v"
) {
  switch (gen) {
    case "generation-i":
      return 1;
    case "generation-ii":
      return 2;
    case "generation-iii":
      return 3;
    case "generation-iv":
      return 4;
    case "generation-v":
      return 5;
  }
}

function generationFromGame(s: GameTitle) {
  switch (s) {
    case "rby":
      return 1;
    case "gsc":
      return 2;
    case "rse":
    case "frlg":
      return 3;
    case "dppt":
    case "hgss":
      return 4;
    case "bw":
    case "b2w2":
      return 5;
  }
}

function fromGenderNumber(n: 1 | 2 | 3 | undefined) {
  switch (n) {
    case 1:
      return "female";
    case 2:
      return "male";
    case 3:
      return "genderless";
    case undefined:
      return undefined;
  }
}

async function buildEvolutions(pokemon: string, chain: Chain, gen: number) {
  if (pokemon == "phione" || pokemon == "manaphy") {
    return [];
  }

  if (chain.species.name != pokemon) {
    const next = chain.evolves_to.find((e) => e.species.name == pokemon);
    return next ? buildEvolutions(pokemon, next, gen) : [];
  }

  const supportedEvolutionTypes = chain.evolves_to.filter((e) =>
    e.evolution_details.every(
      (d) =>
        d.trigger.name == "level-up" ||
        d.trigger.name == "use-item" ||
        d.trigger.name == "trade"
      // until the rest of the evolution methods are incorporated
    )
  );

  const evosSpecies = await Promise.all(
    supportedEvolutionTypes.map((e) =>
      axios
        .get<Species>(e.species.url)
        .then((res) => res.data)
        .then((s) => {
          let tuple: [Species, Chain] = [s, e];
          return tuple;
        })
    )
  );

  return evosSpecies
    .filter(([s, _]) => fromGenerationString(s.generation.name) <= gen)
    .map(([_, e]) => e)
    .map((e) => {
      const detail = e.evolution_details[0];
      let method: EvolutionMethod;
      switch (detail.trigger.name) {
        case "level-up": {
          method = {
            type: "LEVEL_UP",
            level: detail.min_level,
            friendship: detail.min_happiness,
            move: detail.known_move?.name,
            move_type: detail.known_move_type?.name,
            location: detail.location?.name,
            time: detail.time_of_day || undefined,
            item: detail.held_item?.name,
            gender: fromGenderNumber(detail.gender),
            upside_down: detail.turn_upside_down,
          };
          break;
        }
        case "use-item": {
          method = {
            type: "USE_ITEM",
            item: detail.item?.name!,
            gender: fromGenderNumber(detail.gender),
          };
          break;
        }
        case "trade": {
          method = {
            type: "TRADE",
            item: detail.held_item?.name,
            pokemon: detail.trade_species?.name,
          };
          break;
        }
        default: {
          throw new Error(`Unexpected default`);
        }
      }

      return {
        name: e.species.name,
        method: method,
      };
    });
}

export async function fetchPokedex(first: number, last: number, gen: number) {
  const forms = formList;
  const pokemon: PokemonInsert[] = [];

  for (let dexNumber = first; dexNumber <= last; dexNumber++) {
    console.log(`Fetching #${dexNumber}`);

    const poke = await fetchPokemon(dexNumber);
    const species = await fetchSpecies(dexNumber);
    const evolutionChain = await fetchEvolutionChain(species);

    const evolutions = await buildEvolutions(
      poke.name,
      evolutionChain.chain,
      gen
    );

    const insert: PokemonInsert = {
      name: poke.name,
      national_pokedex_number: dexNumber,
      primary_ability: poke.abilities[0].ability.name,
      secondary_ability: poke.abilities.find((a) => a.slot == 2)?.ability?.name,
      hidden_ability: poke.abilities.find((a) => a.is_hidden)?.ability?.name,
      evolutions: evolutions,
      forms: forms.filter(
        (f) =>
          f.number == dexNumber &&
          (f.games ? f.games.some((g) => generationFromGame(g) <= gen) : true)
      ),
      gen: gen,
    };

    pokemon.push(insert);
  }

  return pokemon;
}
