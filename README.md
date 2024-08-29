# Lambda Library

This is a sample project to compare how side-effects can be managed and tested in Haskell using different techniques.

Each alternative has it's own `app<N>` and `test<N>` project, and they all share common library code from `src`.

You can start the tour by reading [./docs/01-Intro.md](./docs/01-Intro.md).

## Alternatives

- **app1** uses `IO` functions direcly. The tests replace & capture the STDIN/STDOUT.
- **app2** uses the handler pattern to be able to replace the functions to interact with the console in tests.
- **app3** similar to `app2` but restricts which other side-effects can happen by introducing a `Task` to replace `IO`.
- **app4** uses custom type classes to restrict other side-effects from happening.
- **app5** similar to `app4` but using `ReaderT` to keep the application state

**TBD**

- **app??** similar to `app2` and `app3` but uses `bluefin` package as a type level alternative of the handler pattern.
- **app??** similar to `app5` but uses `Capabilities` package
- **app??** effectfull
- **app??** polysemy
- **app??** free monads

## Running the code

The code was developed with ghc 9.4.8 and cabal 3.10.3.0 as they were the recommended version at time.

```
% ghcup run --ghc 9.4.8 --cabal 3.10.3.0 -- zsh
# or % ghcup run --ghc recommended --cabal recommended -- zsh

% ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.4.8

% cabal --version
cabal-install version 3.10.3.0
compiled using version 3.10.3.0 of the Cabal library
```

Each app and test can be run with `cabal` directly.

```
% cabal run app1
% cabal test app1-test
```