import fs from "fs";
import { base } from "./constants";
import axios, { AxiosRequestConfig } from "axios";
import {
  BasePokedex,
  Entry,
  PokeapiEntry,
  PokeapiPokedex,
  Pokedex,
} from "./types";

function fromPokeapiEntry(p: PokeapiEntry): Entry {
  return {
    name: p.pokemon_species.name,
    number: p.entry_number,
  };
}

(async function () {
  const dexes = await Promise.all(
    base.map(async (d) => {
      let config: AxiosRequestConfig;
      switch (d.type) {
        case "NATIONAL":
          {
            config = {
              url: `https://pokeapi.co/api/v2/pokedex/national`,
            };
          }
          break;
        default: {
          config = {
            url: `https://pokeapi.co/api/v2/pokedex/${d.pokeapiName}`,
          };
          break;
        }
      }

      const res = await axios.request<PokeapiPokedex>(config);
      const p = res.data;

      return [p, d] as [PokeapiPokedex, BasePokedex];
    })
  );
  const pokedex: Array<Pokedex> = dexes.map(([p, d]) => ({
    name: d.name,
    display_name: d.displayName,
    gen: d.gen,
    type: d.type,
    region: d.region,
    entries:
      d.type == "NATIONAL"
        ? p.pokemon_entries.slice(0, d.cutoff).map(fromPokeapiEntry)
        : p.pokemon_entries.map(fromPokeapiEntry),
  }));

  fs.unlinkSync("pokedex.json");
  fs.writeFileSync("pokedex.json", JSON.stringify(pokedex));
})();
