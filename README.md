# Hoo-Ray
Meet Hoo-Ray: Ray-like distributed execution engine for Haskell, written in Haskell.

# Installation / Executing

- Install the Haskell toolchain, preferably via [GHCup](https://www.haskell.org/ghcup/).

- Run `cabal configure`. Optionally, If you want to build the tools for distributed computing, additionally pass in the argument `-f distributed` to this command (i.e. run `cabal configure -f distributed`).

- In this directory, run

    ```
    cabal install --only-dependencies
    ```

    to install the dependencies [^1].


- Then, `cabal build` to build all the executables to make sure there are no critical errors.

- Finally, `cabal run [program] -- [args]` builds and runs the specified `program` with `args`. See *Hoo-Ray.cabal* for a current list of programs. For example, to generate the depeendency graph, one would do

    ```
    cabal run dependency-graph -- Tests/pure.hs
    ```

# Developing

Add your file and its dependencies to `Hoo-Ray.cabal` just like the ones before.

[^1]: If you get an error about Cabal cannot resolve dependencies while installing with `-f distributed`, downgrade your GHC to 8.4.4 with `ghcup tui` and try again.
