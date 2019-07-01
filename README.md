![Logo](https://holmusk.dev/images/projects/three_layer.png)
[![CircleCI](https://circleci.com/gh/Holmusk/three-layer.svg?style=svg)](https://circleci.com/gh/Holmusk/three-layer)

# three-layer

This package is aimed at being a modern, production-level, batteries-included starting template for
writing web servers with Haskell on backend and Elm on frontend. It follows the
[Three Layer Cake](http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html).
architecture pattern.

Haskell ibraries used in here:
* [`relude`](https://github.com/kowainik/relude): alternative prelude; here
  `base-noprelude` trick is used.
* [`co-log`](https://github.com/kowainik/co-log):
  [composable contravariant comonadic logging library](https://kowainik.github.io/posts/2018-09-25-co-log).
* [`postgresql-simple`](http://hackage.haskell.org/package/postgresql-simple):
  mid-level PostgreSQL client library for database interaction.
* [`servant`](http://hackage.haskell.org/package/servant): family of libraries
  for defining webservices Rest API on type-level.
* [`elm-street`](https://github.com/Holmusk/elm-street): bridge between Elm and
  Haskell - generating Elm data types, JSON encoders and decoders automatically
  from Haskell types.
* [`proto-lens`](http://hackage.haskell.org/package/proto-lens): Protobuf
  messages for integration with the mobile application.
* [`ekg`](http://hackage.haskell.org/package/ekg): application performance monitoring.
* [`bcrypt`](http://hackage.haskell.org/package/bcrypt): password hashing functions.
* [`jwt`](http://hackage.haskell.org/package/jwt): user authentication via JWT.
* [`hspec`](http://hackage.haskell.org/package/hspec) and [`hedgehog`](http://hackage.haskell.org/package/hedgehog): testing libraries.

## Detailed approach description

This section contains more detailed description of the chosen architecture and
our particular implementation of it.

### Application environment

Data type for the runtime environment for the whole application is defined in
the [`Lib/App/Env.hs`](src/Lib/App/Env.hs) module. It contains various fields
required for the application processing, like database pool, JWT secret, logger,
etc. It also has instance of custom `Has` typeclass which tells how to extract
different parts of the application. This is done to achieve the following purposes:

1. Specify in the constraints what parts of the environment you need.
2. Introduce more modularity when multiple different environments are implemented.

Environment initialisation is happening in the [`Lib.hs`](src/Lib.hs) module.

### Application errors

Module [`Lib/App/Error.hs`](src/Lib/App/Error.hs) contains exhaustive list of
all errors that application can throw. This module provides convenient layer
between human-readable error names and HTTP error codes. It also contains useful
utilities for throwing errors and for formatting `CallStack` of errors.

### Application monad

Main application monad can be found in the
[`Lib/App/Monad.hs`](src/Lib/App/Monad.hs) module.

### Database

This template uses PostgreSQL database and contains helper wrappers around
functions from the `postgresql-simple` library to integrate smoother with our
own monad. See [`Lib/Db/Functions.hs`](src/Lib/Db/Functions.hs) for more details.

### Effects

All new effects (like sending an email. storing the file, etc.) should be added
to the [`Lib/Effects/`](src/Lib/Effects) directory.
