# PnP Orc

_**Disclaimer:** This Clojure based software is currently usable through the REPL only. Furthermore,
it's poorly documented and I'm reconsidering some of the basic functionality. Use with care._

PnP Orc is a tool for Print and Play files.

PnP Orc currently have the following features:

- Take a PDF meant to be printed double sided and turn it into a PDF
  with folding lines instead given that there's a recipe for that game.
- Create boxes for cards based on measures and pictures.

PnP Orc has a list of recipes for PnP games or PnP versions of physical
games. If you are not afraid of a little Clojure programming there's
an API for creating new recipes.

## Recipes (in alphabetic order)

- [Palm Island](https://boardgamegeek.com/boardgame/239464/palm-island)
- [Sprawlopolis](https://boardgamegeek.com/boardgame/251658/sprawlopolis)
- [Supertall](https://boardgamegeek.com/boardgame/252684/supertall)

## Installation

Download from http://example.com/FIXME.

## Usage

    $ java -jar PnP-orc-0.1.0-standalone.jar [args]


## Making a recipe for at game

See [Creating recpies](doc/recipes.md).

Why can't I just give the Orc a PDF and it will chew on it and spit out
the right file? At least with a few coordinates? Well, in short things
are not that simple when working with PnP files. You can read more in
the documentation.


## And I also want to point out that ...

PnP Orc is not a wizard. It's an orc. Because the word "orc" is shorter
and is therefore a better name in code. And it doesn't guide you through
the processes (at least not in the current version) and is thus not a
wizard. Okay, so it's a tool then, but "PnP tool" is already a thing and
a boring name.


## License

Copyright © 2018 Daniel Lundsgaard Skovenborg

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
