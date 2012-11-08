# Introduction

This is a Haskell driver for [Factual's public API](http://developer.factual.com/display/docs/Factual+Developer+APIs+Version+3).

# Features

* Read Queries
  * Places US
  * Restaurants US
  * Hotels US
  * Global Places
  * Crosswalk
  * Healthcare Providers
  * World Geographies
  * Products CPG
  * Products Crosswalk
  * Monetize
  * Custom Tables
* Facets Queries
* Geocode Queries
* Geopulse Queries
* Resolve Queries
* Match Queries
* Schema Queries
* Diffs Queries
* Multi Queries
* Raw Read Queries
* Parametric Search
* Geo Filters
* Query Debugging

# Installation

## Prerequisites

This driver targets [ghc 7.4.1](http://www.haskell.org/ghc/)
and [The Haskell Platform 2012.2.0.0](http://hackage.haskell.org/platform/).
Please follow the installation instructions for your specific
platform to install ghc and The Haskell Platform.

You'll need a Factual account to use this driver. If you don't have one yet, [it's free and easy to get one](https://www.factual.com/api-keys/request).

## Installation from cabal

    $ cabal install factual-api

## Installation from git

    $ git clone git@github.com:rudyl313/factual-haskell-driver.git
    $ cabal install hoauth
    $ cabal install aeson
    $ cabal install MissingH

# Tests

To run the integration tests you'll need an to create a yaml file
at ~/.factual/factual-auth.yaml to store your Factual credentials:

    ---
    key: MYKEY
    secret: MYSECRET

Load the tests file into ghci to run the tests:

    $ ghci test/Tests.hs

Next you use the following functions to run the unit and integration
tests respectively:

    *Main> runUnitTests
    Cases: 53  Tried: 53  Errors: 0  Failures: 0
    Counts {cases = 53, tried = 53, errors = 0, failures = 0}
    *Main> runIntegrationTests
    Cases: 14  Tried: 14  Errors: 0  Failures: 0
    Counts {cases = 14, tried = 14, errors = 0, failures = 0}

# Documentation

You can read library documentation by visiting the [Hackage page](http://hackage.haskell.org/package/factual-api).

The [github wiki](https://github.com/rudyl313/factual-haskell-driver/wiki) also
provides thorough documentation and examples for each feature.

# Examples

See the examples directory for examples of each query type. To
run an example go to the project root and execute these commands:

    $ ghc -o example examples/ReadExample.hs --make
    $ ./example mykey mysecret

In this example replace mykey with your key and mysecret with your
secret. Note that compiling the source code generates .o and .hi
files in the source directories.

# Locale issues

If you're using Linux and experience character encoding issues add the
following line to your bashrc before using the code:

  export LC_ALL="en_US.UTF-8"
