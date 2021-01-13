import axios, { AxiosRequestConfig } from "axios";
import * as fs from "fs";
import {
  Chain,
  EvolutionChain,
  EvolutionInsert,
  EvolutionMethod,
  Pokemon,
  PokemonInsert,
  Species,
} from "./types";

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

function buildEvolutions(pokemon: string, chain: Chain): EvolutionInsert[] {
  if (chain.species.name != pokemon) {
    const next = chain.evolves_to.find((e) => e.species.name == pokemon);
    return next ? buildEvolutions(pokemon, next) : [];
  }

  return chain.evolves_to
    .filter((e) =>
      e.evolution_details.every(
        (d) =>
          d.trigger.name == "level-up" ||
          d.trigger.name == "use-item" ||
          d.trigger.name == "trade"
        // until the rest of the evolution methods are incorporated
      )
    )
    .map((e) => {
      const detail = e.evolution_details[0];
      let method: EvolutionMethod;
      switch (detail.trigger.name) {
        case "level-up": {
          method = {
            type: "LEVEL_UP",
            level: detail.min_level,
            friendship: detail.min_happinnes,
            move: detail.known_move_type,
            location: detail.location,
            time: detail.time_of_day || undefined,
            item: detail.held_item,
            gender: detail.gender,
            upsideDown: detail.turn_upside_down,
          };
          break;
        }
        case "use-item": {
          method = {
            type: "USE_ITEM",
            item: detail.item!,
          };
          break;
        }
        case "trade": {
          method = {
            type: "TRADE",
            item: detail.held_item,
            pokemon: detail.trade_species,
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

export async function fetchPokedex(first: number, last: number) {
  const pokemon: PokemonInsert[] = [];

  for (let dexNumber = first; dexNumber <= last; dexNumber++) {
    console.log(`Fetching #${dexNumber}`);

    const poke = await fetchPokemon(dexNumber);
    const species = await fetchSpecies(dexNumber);
    const evolutionChain = await fetchEvolutionChain(species);

    const insert: PokemonInsert = {
      name: poke.name,
      dexNumber: dexNumber,
      primaryAbility: poke.abilities[0].ability.name,
      secondaryAbility: poke.abilities.find((a) => a.slot == 2)?.ability?.name,
      hiddenAbility: poke.abilities.find((a) => a.is_hidden)?.ability?.name,
      evolutions: buildEvolutions(poke.name, evolutionChain.chain),
    };

    pokemon.push(insert);
  }

  return pokemon;
}
