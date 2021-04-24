# dex-assets

Tool to generate sprites and icons for [dex-tracker](https://github.com/aleperaltabazas/dex-tracker).

```
dex-assets

Usage: dex-assets-exe [-r|--refresh] [-d|--directory ARG]

Available options:
  -r,--refresh             Refresh pokeapi sprites
  -d,--directory ARG       Destination directory
  -h,--help                Show this help text
```

This tool depends [pokeapi's sprites](https://github.com/pokeapi/sprites) .

Missing stuff:
- ~~Icons for gens 1 to 5~~
- Icons for gens 6 and 7
- Icons for gen 8

Bugs:
- Pokemon with form differences (namely Unfezant, Frillish and Jelicent) will crash on download because pokencyclopedia is not quite consistent on those three and (instead of being `xyz-m_1.png` it's `xyz_m-1.png`).
