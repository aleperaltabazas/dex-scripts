import { fetchPokedex } from "./pokeapi";
import fs from "fs";

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
  if (fs.existsSync("pokemon.json")) {
    fs.unlinkSync("pokemon.json");
  }

  const args = parseArgs();

  const gen = requireGeneration(args.gen);
  let start = args.start || 1;
  let finish: number;

  function minOrDefault(n: number) {
    return args.finish ? Math.min(args.finish, n) : n;
  }

  switch (gen) {
    case 1:
      finish = minOrDefault(151);
      break;
    case 2:
      finish = minOrDefault(251);
      break;
    case 3:
      finish = minOrDefault(386);
      break;
    case 4:
      finish = minOrDefault(493);
      break;
    case 5:
      finish = minOrDefault(649);
      break;
  }

  const pokemon = await fetchPokedex(start, finish, gen);
  fs.appendFileSync(
    "pokemon.json",
    `[${pokemon.map((p) => JSON.stringify(p)).join(",")}]`
  );
  // fs.appendFile("pokemons.sql", insertPokemon(p), () =>
  //   console.log(`Wrote #${p.national_pokedex_number}`)
  // );
}

run();
