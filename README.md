![]("results/examples/logo.jpg")

# clojure2d.charts

THIS IS WIP/POC

A "no web-based" pure JVM Clojure library for 2d charts, inspired by D3, Vega and R (ggplot2/lattice/facet).

## Usage

Currently there are no easy to use functions nor data DSL (help needed here). Still everything is done manually.

See `sketches` folder for examples

## Internals

[How does it work?](INTERNALS.md)

## Short term plan

* More chart types (`+` - easier; `-` - harder):
    - heatmaps (various grids) (`+`)
    - point clouds (log and linear rendering) (`+`)
    - contours (`-`), flow fields (`+`)
    - categorical x categorical plots (scatter plot matrix  (`+`)
    - stacked area/stream (`+`)
    - parallel / hive (`+`/`-`)
    - radial charts, like pie (`+`), sunburst (`-`), radial tree (`-`)
    - arc(?) (`+`)
* Labels / legends
* Higher level API
    - single function for each chart type (easier)
    - faceting/lattice (harder)
* cleaning configuration mess

## Long term plan / wishlist

* Data DSL(?)
* geo maps
* elements of UpSet
* signal processing / time series
* annotations
* grid of charts / subplots (with independent axes)
* kind of interactivity
* chart styles (now everything is blueish)

## Known issues

* grid, axes sometimes don't match (1-2px shift). This is due to rounding errors and axes rotations
* still no labels, no legends
* overlapping ticks (add some heuristics to use smaller fonts and smaller number of ticks)
* chart configuration need to be reviewed, still not consistent
* no higher level functions/macros
* no DSL for data manipulation, and no plan to make it (task for others!)
* deep nesting is really nasty to configure and for data preparation :/ (lattice of stacked horizontally stacked bars...)
* merging nesting extents is not working correctly (in lattice)
* true is, there are some labels for lattice, quick and dirty
* do not layer over histograms...
* still something is wrong with time scale

## Why?

So, why another chart library, when you can use _insert any name here_
In my case, the main obstacle was the ability to save hundreds of charts without using any display (web or java frame).
One can say: jfreechart! I can say: try to generate heatmaps... Every library I've found had some issues or produces ugly result or was web based or was notebook based or...
So I fell into the lisp curse and I'm writing my own.

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
