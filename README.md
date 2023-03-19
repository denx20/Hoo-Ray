# Hoo-Ray
Meet Hoo-Ray: Ray-like distributed execution engine for Haskell, written in Haskell.

# Installation / Executing

Install the Haskell toolchain, preferably via [GHCup](https://www.haskell.org/ghcup/). Then in this directory, run

```
cabal install --only-dependencies
```

to install the dependencies.

Then, `cabal build` to build all the executables to make sure there are no critical errors.

Finally, `cabal run [program] -- [args]` builds and runs the specified `program` with `args`. See *Hoo-Ray.cabal* for a current list of programs. For example, to generate the depeendency graph, one would do

```
cabal run dependency-graph -- Tests/test.hs
```

# Developing

Add your file and its dependencies to `Hoo-Ray.cabal` just like the ones before.