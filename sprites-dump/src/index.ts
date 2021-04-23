import { Pokedex, Pokemon } from "./types";
import fs from "fs";
import axios from "axios";
import Path from "path";
import rimraf from "rimraf";

const gen1Cuttof = 151;
const gen2Cuttof = 251;
const gen3Cuttof = 386;
const gen4Cuttof = 493;
const gen5Cuttof = 649;

const args = process.argv.slice(2);

if (args.length < 1 || args.length > 2) {
  console.error(
    `Illegal number of arguments: expected at least 1 argument, maximum 2, got ${args.length}`
  );
  console.error("Run like sprites-dump <gen> <directory>");
}

const gen = Number.parseInt(args[0]);
const dir = args[1] || __dirname;

function cutoff(gen: number): number {
  switch (gen) {
    case 1:
      return gen1Cuttof;
    case 2:
      return gen2Cuttof;
    case 3:
      return gen3Cuttof;
    case 4:
      return gen4Cuttof;
    case 5:
      return gen5Cuttof;
    default:
      throw new Error(`Unsupported generation ${gen}`);
  }
}

async function downloadSprite(gen: number, pokemon: Pokemon) {
  function pad(n: number) {
    if (n < 10) return `00${n}`;
    else if (n < 100) return `0${n}`;
    else return n.toString();
  }

  function spritePath(): string {
    switch (gen) {
      case 1:
        return `spr_yellow_gbc/spr_y-gbc_${pad(pokemon.number)}.png`;
      case 2:
        return `spr_crystal/spr_c_${pad(pokemon.number)}.png`;
      case 3:
        return `spr_emerald/spr_e_${pad(pokemon.number)}_1.png`;
      case 4:
        return `spr_platinum/spr_pt_${pad(pokemon.number)}_1.png`;
      case 5:
        return `ani_black-white/ani_bw_${pad(pokemon.number)}.gif`;
    }
  }

  const url = `https://www.pokencyclopedia.info/sprites/gen${gen}/${spritePath()}`;
  const fileName = url.split("/").reverse()[0];
  const extension = fileName.split(".").reverse()[0];
  const path = Path.resolve(
    dir,
    "images",
    `gen${gen}`,
    `${pokemon.name}.${extension}`
  );
  const writer = fs.createWriteStream(path);

  const response = await axios.request({
    url,
    method: "GET",
    responseType: "stream",
  });

  response.data.pipe(writer);

  return new Promise((resolve, reject) => {
    writer.on("finish", resolve), writer.on("error", reject);
  }).catch(console.error);
}

async function run() {
  rimraf.sync(Path.resolve(dir, "images"));
  fs.mkdirSync(Path.resolve(dir, "images"));
  fs.mkdirSync(Path.resolve(dir, "images", "gen1"));
  fs.mkdirSync(Path.resolve(dir, "images", "gen2"));
  fs.mkdirSync(Path.resolve(dir, "images", "gen3"));
  fs.mkdirSync(Path.resolve(dir, "images", "gen4"));
  fs.mkdirSync(Path.resolve(dir, "images", "gen5"));

  const c = cutoff(gen);
  const pokedex = await axios
    .request<Pokedex>({ url: "https://pokeapi.co/api/v2/pokedex/national" })
    .then((res) => res.data)
    .then((dex) => dex.pokemon_entries.slice(0, c));

  for await (const p of pokedex) {
    console.log(`Downloading ${p.pokemon_species.name}`);
    await downloadSprite(gen, {
      name: p.pokemon_species.name,
      number: p.entry_number,
    });
  }
}

run();
