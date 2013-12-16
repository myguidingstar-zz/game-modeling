# Game modeling

This program models how a randomly mixed population of two races will
evolve over time if people just do not want to be surround by too much
of the other race.

# Requirements
You need [Racket](http://racket-lang.org/) installed to run/develop this.

# Development

Run unit tests

[![Build Status](https://travis-ci.org/myguidingstar/game-modeling.png?branch=master)](https://travis-ci.org/myguidingstar/game-modeling)

```sh
racket -t core-test.rkt
```

It is recommended to have a watcher automatically rerun the tests
while developing. For example, I use [nodemon](https://github.com/remy/nodemon)

```sh
nodemon -e scm --exec "racket -t core-test.rkt"
```

# Usage

```sh
racket -t gui.rkt
```
then press "Next n turns" button to see the "world" changing.

# License
Copyright (C) 2013 Hoang Minh Thang

Distributed under the GNU GENERAL PUBLIC LICENSE Version 3.

See the file COPYING for more details.
