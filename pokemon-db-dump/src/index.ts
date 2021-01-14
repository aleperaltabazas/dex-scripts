import { fetchPokedex } from "./pokeapi";
import fs from "fs";
import { EvolutionInsert, Form, PokemonInsert } from "./types";

function parseArgs(): Args {
  const args = process.argv.slice(2);
  if (args.length < 1) {
    throw new Error("Error: missing gen parameter");
  } else if (args.length == 1) {
    return { gen: parseInt(args[args.indexOf("--gen") + 1]) };
  } else if (args.length == 2) {
    throw new Error("Error: missing finish parameter");
  } else {
    let [gen, start, finish] = args;

    return {
      gen: parseInt(gen),
      start: parseInt(start),
      finish: parseInt(finish),
    };
  }
}

interface Args {
  gen: number;
  start?: number;
  finish?: number;
}

function insertPokemon(pokemon: PokemonInsert) {
  return `
insert into pokemon (name, national_dex_number, primary_ability, secondary_ability, hidden_ability, gen)
values (
  '${pokemon.name}', 
  ${pokemon.dexNumber}, 
  '${pokemon.primaryAbility}', 
  ${orNullString(pokemon.secondaryAbility)}, 
  ${orNullString(pokemon.hiddenAbility)},
  ${pokemon.gen}
);

set @pokemon_id = LAST_INSERT_ID();
${insertEvolutions(pokemon.evolutions)}
${insertForms(pokemon.forms)}`;
}

function insertEvolutions(evolutions: EvolutionInsert[]) {
  if (evolutions.length == 0) {
    return "";
  } else {
    return `
insert into evolutions (name, pokemon_id, method)
values
${evolutions
  .map((e) => `('${e.name}', @pokemon_id, '${JSON.stringify(e.method)}')`)
  .join(",\n")};
`;
  }
}

function insertForms(forms: Form[]) {
  if (forms.length == 0) {
    return "";
  } else {
    return `
insert into forms (name, pokemon_id)
values
${forms.map((f) => `('${f.name}', @pokemon_id)`).join(",\n")};
    `;
  }
}

function orNullString(s?: string) {
  return s ? "'" + s + "'" : "null";
}

function requireGeneration(number: number): 1 | 2 | 3 | 4 | 5 {
  switch (number) {
    case 1:
      return 1;
    case 2:
      return 2;
    case 3:
      return 3;
    case 4:
      return 4;
    case 5:
      return 5;
    default:
      throw new Error(`Unknown generation ${number}`);
  }
}

async function run() {
  if (fs.existsSync("pokemons.sql")) {
    fs.unlinkSync("pokemons.sql");
  }

  const args = parseArgs();

  const gen = requireGeneration(args.gen);
  let start = args.start || 1;
  let finish: number;

  switch (gen) {
    case 1:
      finish = args.finish || 151;
    case 2:
      finish = args.finish || 251;
    case 3:
      finish = args.finish || 386;
    case 4:
      finish = args.finish || 493;
    case 5:
      finish = args.finish || 649;
  }

  await fetchPokedex(start, finish, gen, (p) => {
    fs.appendFile("pokemons.sql", insertPokemon(p), () =>
      console.log(`Wrote #${p.dexNumber}`)
    );
  });
}

run();
