# cosmic-express-solver

Solves puzzles from [Cosmic Express](https://store.steampowered.com/app/583270/Cosmic_Express/).

## Usage

```
cosmic-solver
  --datadir path_to_output_folder
  --constellation andromeda
  --level 1
```

Results are saved and cached in `datadir`.

```
datadir/
  andromeda/
    1/
      answer.json
```

<!--

TODO:

- Save `proof.json` for each level, and have a mode that displays a simulation of the train as a verification of the puzzle solution.
- Save `debug.ndjson` for each level, for showing intermediate states.

-->
