# Introduction

This is a Haskell driver for [Factual's public API](http://developer.factual.com/display/docs/Factual+Developer+APIs+Version+3).

# Installation

## Prerequisites

This driver was developed on [ghc 7.0.4](http://www.haskell.org/ghc/)
and [The Haskell Platform 2011.4.0.0](http://hackage.haskell.org/platform/).
Please follow the installation instructions for your specific
platform to install ghc and The Haskell Platform.

## Installation from git

    $ git clone git@github.com:rudyl313/factual-haskell-driver.git
    $ cabal install hoauth
    $ cabal install aeson

Note you may require libcurl to install hoauth. On lucid you can run:

    $ sudo apt-get install libcurl4-openssl-dev

## Installation from cabal

    $ cabal install factual-api

# Tests

Load the tests file into ghci to run the tests:

    $ ghci test/Tests.hs

To run the integration tests you'll need an API key and secret, but you
can always run the unit tests:

    *Main> runUnitTests
    Cases: 39  Tried: 39  Errors: 0  Failures: 0
    Counts {cases = 39, tried = 39, errors = 0, failures = 0}
    *Main> runIntegrationTests "mykey" "mysecret"
    Cases: 6  Tried: 6  Errors: 0  Failures: 0
    Counts {cases = 6, tried = 6, errors = 0, failures = 0}

# Documentation

You can read library documentation by opening docs/index.html in
your browser.

# Examples

See the examples directory for examples of each query type. To
run an example go to the project root and execute these commands:

    $ ghc -o example examples/ReadExample.hs --make
    $ ./example mykey mysecret

In this example replace mykey with your key and mysecret with your
secret. Note that compiling the source code generates .o and .hi
files in the source directories.
