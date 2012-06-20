# Introduction

This is a Haskell driver for [Factual's public API](http://developer.factual.com/display/docs/Factual+Developer+APIs+Version+3).

# Installation

## Prerequisites

This driver targets [ghc 7.4.1](http://www.haskell.org/ghc/)
and [The Haskell Platform 2012.2.0.0](http://hackage.haskell.org/platform/).
Please follow the installation instructions for your specific
platform to install ghc and The Haskell Platform.

## Installation from cabal

    $ cabal install factual-api

## Installation from git

    $ git clone git@github.com:rudyl313/factual-haskell-driver.git
    $ cabal install hoauth
    $ cabal install aeson

# Tests

Load the tests file into ghci to run the tests:

    $ ghci test/Tests.hs

To run the integration tests you'll need an API key and secret, but you
can always run the unit tests:

    *Main> runUnitTests
    Cases: 47  Tried: 47  Errors: 0  Failures: 0
    Counts {cases = 47, tried = 47, errors = 0, failures = 0}
    *Main> runIntegrationTests "mykey" "mysecret"
    Cases: 10  Tried: 10  Errors: 0  Failures: 0
    Counts {cases = 10, tried = 10, errors = 0, failures = 0}

# Documentation

You can read library documentation by opening docs/index.html in
your browser or by visiting the [Hackage page](http://hackage.haskell.org/package/factual-api).

# Examples

See the examples directory for examples of each query type. To
run an example go to the project root and execute these commands:

    $ ghc -o example examples/ReadExample.hs --make
    $ ./example mykey mysecret

In this example replace mykey with your key and mysecret with your
secret. Note that compiling the source code generates .o and .hi
files in the source directories.

# Writes

Although there is code in the current version to support API writes,
these features have not yet been implemented in the Factual API. Please
refrain from attempting to use these features until they have been
officially announced.
