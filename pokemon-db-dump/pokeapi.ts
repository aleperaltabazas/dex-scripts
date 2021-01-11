import axios, { AxiosRequestConfig } from "axios";
import * as fs from "fs";

if (process.argv.length < 4) {
  console.error("Error: missing arguments 'first' and 'last'");
}

let first = Number.parseInt(process.argv[2]);
let last = Number.parseInt(process.argv[3]);

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

function sql(pokemon: PokemonInsert) {
  return `
insert into pokemons (name, national_dex_number, primary_ability, secondary_ability, hidden_ability, primary_type, secondary_type, male_prob, female_prob, hp, attack, defense, special_attack, special_defense, speed)
values (
  '${pokemon.name}', 
  ${pokemon.dexNumber}, 
  '${pokemon.primaryAbility}', 
  ${orNullString(pokemon.secondaryAbility)}, 
  ${orNullString(pokemon.hiddenAbility)},
  '${pokemon.primaryType}',
  ${orNullString(pokemon.secondaryType)},
  ${orNullNumber(pokemon.maleProbability)},
  ${orNullNumber(pokemon.femaleProbability)},
  ${pokemon.hp},
  ${pokemon.attack},
  ${pokemon.defense},
  ${pokemon.specialAttack},
  ${pokemon.specialDefense},
  ${pokemon.speed}
);

set @pokemon_id = LAST_INSERT_ID();
${pokemon.evolutions.map(
  (e) => `
insert into evolutions (name, pokemon_id, method)
values ('${e.name}', @pokemon_id, '${JSON.stringify(e.method)}');`
)}`;
}

function orNullString(s?: string) {
  return s ? "'" + s + "'" : "null";
}

function orNullNumber(n?: number) {
  return n ? n : "null";
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

async function run() {
  for (let dexNumber = first; dexNumber <= last; dexNumber++) {
    console.log(`Fetching #${dexNumber}`);

    const poke = await fetchPokemon(dexNumber);
    const species = await fetchSpecies(dexNumber);
    const evolutionChain = await fetchEvolutionChain(species);

    const femaleProbability =
      species.gender_rate == -1 ? undefined : (species.gender_rate * 100) / 8;
    const maleProbability =
      femaleProbability == undefined ? undefined : 100 - femaleProbability;
    const insert: PokemonInsert = {
      name: poke.name,
      dexNumber: dexNumber,
      primaryAbility: poke.abilities[0].ability.name,
      secondaryAbility: poke.abilities.find((a) => a.slot == 2)?.ability?.name,
      hiddenAbility: poke.abilities.find((a) => a.is_hidden)?.ability?.name,
      primaryType: poke.types[0].type.name.toUpperCase(),
      secondaryType: poke.types
        .find((a) => a.slot == 2)
        ?.type?.name?.toUpperCase(),
      maleProbability: maleProbability,
      femaleProbability: femaleProbability,
      hp: poke.stats[0].base_stat,
      attack: poke.stats[1].base_stat,
      specialAttack: poke.stats[3].base_stat,
      defense: poke.stats[2].base_stat,
      specialDefense: poke.stats[4].base_stat,
      speed: poke.stats[5].base_stat,

      evolutions: buildEvolutions(poke.name, evolutionChain.chain),
    };

    fs.appendFile("pokemons.sql", sql(insert), () =>
      console.log(`Done with #${dexNumber}`)
    );
  }
}

run();
