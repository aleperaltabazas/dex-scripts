import { fetchPokedex } from "./pokeapi";
import fs from "fs";
import { PokemonInsert } from "./types";

let first = Number.parseInt(process.argv[2]);
let last = Number.parseInt(process.argv[3]);

function sql(pokemon: PokemonInsert) {
  return `
insert into pokemons (name, national_dex_number, primary_ability, secondary_ability, hidden_ability)
values (
  '${pokemon.name}', 
  ${pokemon.dexNumber}, 
  '${pokemon.primaryAbility}', 
  ${orNullString(pokemon.secondaryAbility)}, 
  ${orNullString(pokemon.hiddenAbility)}
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

async function run() {
  fs.unlinkSync("pokemons.sql");
  const pokemon = await fetchPokedex(first, last);
  pokemon.forEach((p, idx) => {
    fs.appendFile("pokemons.sql", sql(p), () => console.log(`Wrote ${idx}`));
  });
}

run();
