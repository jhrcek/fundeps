# fundeps

**Fun**ction **dep**endencie**s**.

An experimental CLI tool for visualization of elm declaration dependencies.

# TODOs
* Make the autocompletion work for queries of the form `Module:function` even for modules from external packages
* Ask for confirmation when displaying too large graphs
* Find better way how to integrate hacked elm compiler into the tool

## How to install

:warning: This tool is still very much work in progress and will change a lot.
The following installation steps will likely be simplified in the future :warning:

1. Make sure the [graphviz](https://www.graphviz.org/) toolset is installed on your system.

2. Install `hacked-elm` binary (NOTE: This is modified version of elm compiler which writes out function usage data into file.)
   ```bash
   git clone git@github.com:jhrcek/compiler.git
   cd compiler
   git checkout hackCanonicalAST
   stack install
   ```
   That should make the `hacked-elm` available on your path. You won't use this binary directly, it's used automatically by the `fundeps` tool installed next.

3. Install `fundeps` binary
   ```bash
   git clone git@github.com:jhrcek/fundeps.git
   cd fundeps
   stack install
   ```
   This should make `fundeps` binary available on your PATH.

## How to run it

Move to a folder with elm project (where your elm.json is) and run
```bash
fundeps path/to/Main.elm
```
