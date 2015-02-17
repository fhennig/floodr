# floodr

![screenshot](floodr_screenshot01.png)

floodr is a game for the unix terminal. The goal is to make your part
of the board as large as possible. It is possible to play alone or
with up to four players, and there is also an AI.

This was mostly a project for myself to learn some clojure, but now
it's out there for anyone who is interested!

## Installation

Download the standalone jar from the
[releases page](https://github.com/greenkeeper/floodr/releases/tag/v0.2). 

Or clone the repo and use [leiningen](http://leiningen.org/) to run it.

## Starting

    $ java -jar floodr-0.2.0-standalone.jar [args]

However I recommend to create an alias in `~/.bash_aliases`

    alias floodr='java -jar /path/to/floodr-0.2.0-standalone.jar'

Add `--swing` as an argument if you want to use a GUI terminal. This
should only be used if you don't have `/bin/sh`.

## Controls

Type `?` to get an overview of the controls.

`s` `d` `f` and `j` `k` `l` are used to colorize your block.

`n` to start a new game and `q` to quit.

## License

Copyright © 2015

Distributed under the Eclipse Public License version 1.0, the same as clojure.
