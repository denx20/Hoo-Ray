# Hoo-Ray

Meet Hoo-Ray: Ray-like distributed execution engine for Haskell, written in Haskell. Hoo-Ray uses an abstract syntax tree parser to parsing the data dependencies in the main function of a given Haskell program, and a greedy scheduler for scheduling all computations that have their dependency ready.

This project is done as part of the course [CS 512: Distributed Systems](https://courses.cs.duke.edu/spring23/compsci512/) at Duke University in Spring 2023.

# Quickstart

## Testing UDP multicast

If you want to use the tools for distributed computing, first make sure that UDP multicast works with your configuration.

To test UDP multicast, use `listener` and `sender` in the `c-test` folder like this:

```bash
# Terminal window 1
cd Server/c-test
make
./sender 239.255.255.250 1900

# Terminal window 2
cd Server/c-test
./listener 239.255.255.250 1900
```

If UDP multicast is configured properly[^udp], you should see *Hello, World!* printed on the terminal with `listener` running.

## Installation

- Install the Haskell toolchain, preferably via [GHCup](https://www.haskell.org/ghcup/).

- Run `cabal configure`. Optionally, If you want to build the tools for distributed computing, additionally pass in the argument `-f distributed` to this command (i.e. run `cabal configure -f distributed`).

- In this directory, run

    ```
    cabal install --only-dependencies
    ```

    to install the dependencies [^deps].


- Then, `cabal build` to build all the executables to make sure there are no critical errors.

- Finally, `cabal run [program] [args]` builds and runs the specified `program` with `args`. See *Hoo-Ray.cabal* for a current list of programs. For example, to generate the dependency graph for the file *Tests/pure1.hs*, one would do
    ```
    cabal run dependency-graph Tests/pure1.hs
    ```
    If you wish to use command flags, run `cabal run [program] -- [-f1|--flag1] ...` . For example, to generate matrix multiplication benchmark files, one could do
    ```
    cabal run matmul_test_gen -- -l 50 -m 100 -n 1000 -p 100 -r 10
    ```


## Current Modules

##### dependency-graph

Generates a dependency graph (in text) from input file. e.g. `cabal run dependency-graph Tests/pure1.hs`

We also have a Python script using [NetworkX](https://networkx.org) and [PyGraphviz](https://pygraphviz.github.io) to visualize the graph. To run it, first install these two packages in your Python environment. Then redirect the output of the above command via e.g. `cabal run dependency-graph Tests/pure1.hs > Vis/graph.out`. Finally, modify the corresponding variables in `Vis/fetch_graph.py` and run `python Vis/fetch_graph.py`.

Output of `cabal run dependency-graph Tests/pure1.hs > Vis/graph.out` and `python Vis/fetch_graph.py` as an example:

![example graph](Vis/ex_graph.png)

##### distributed-compute

A simple example of a pair of master-slave server such that the master sends a job to a slave and the slave sends the job results back. All this logic is wrapped in the `Process ()` monad.

The parameters of this module are in the form `'master <host> <port>' or 'slave <host> <port>'`. To run this module, execute `cabal run distributed-compute slave localhost 8081`. Then open another terminal and execute `cabal run distributed-compute master localhost 8084`.

##### matmul_test_gen

Generates single-threaded (*Tests/matmul_ss_test.hs*) and multi-threaded (*Tests/matmul_ms_test.hs*) test files with matrix multiplication operations. For a detailed look at the configuration options, run

```
cabal run matmul_test_gen -- -h
```
To run the resulting multi-threaded test file, pass in the runtime flags like so:
```
cabal run matmul_ms_test -- +RTS -N
```

We have deprecated the `-t` flag, so the way to time this function would be to use the `time` command. e.g. `time cabal run matmul_ms_test -- +RTS -N`.

## Other modules

These are modules that did not withstand the test of time. Their service is no longer required, because their functionalities either got replaced or did not prove to be useful in the grand scheme of our projects. But we provide a description for what they do.

For instructions on how to run each module, try `cabal run module_name` first. If the module requires inputs, it should show a message about it.

##### arithmetic_test_gen

##### list_test_gen

The above two tests are too fast (usually takes <1 millisecond to execute the entire file) and the network overhead from distributing does not pay off.

# Developing

Add your file and its dependencies to `Hoo-Ray.cabal` just like the ones before.

[^udp]: In the particular case of misconfiguration that happened to me while running a Linux machine on Duke's network, I had to execute `sudo vim /etc/resolv.conf` and change the nameserver line to `nameserver 152.3.72.100`. Might not apply to you, but could be of interest to consult.
[^deps]: If you get an error about Cabal cannot resolve dependencies while installing with `-f distributed`, downgrade your GHC to 8.4.4 with `ghcup tui` and try again.


<!-- There is also a flag (-O) for GHC to compile everything aggressively optimized,  but that requires recompiling of all the existing libraries via `cabal install -p package --reinstall`-->


# Current benchmarking results

#### Dennis' computer (2022 MacBook Pro, 10-core M1 Pro, Rosetta emulation (??))

*@Dennis: fill in here and also figure out whether you had Rosetta while running these tests*

#### Jaden's computer (2019 MacBook Pro, 8-core i9)

100 lines, 100x1000x100 matrices.

```
cabal run matmul_test_gen -- -l 100 -m 100 -n 1000 -p 100 -r 1

cabal run matmul_ms_test -- +RTS -N  120.58s user 10.18s system 1254% cpu 10.425 total

cabal run matmul_ss_test  33.87s user 1.42s system 97% cpu 36.296 total
```
