# cljplot internals

## Dependencies

cljplot depends on following

* clojure2d - rendering backend
* fastmath - statistics, distributions, interpolators, math
* clojure-java-time - for date/time operations

## Some terms used

* scale map- set of scale information like scale function, domain, ticks and tick formatting function
* scale - custom record which acts as a function which converts domain value into defined range
* lattice - faceted view
* extent - domain range
* build phase - chart data processing phase
* render phase - image creation
* orientation - axes orientation, by default [0,0] is at bottom-left(!) corner. This is opposite to default java canvas (which is top-left).

## Chart

Chart definition is a sequence of three values:

* chart name as a keyword
* input data (when needed)
* optional configuration / styling

For example valid chart definitions are:

```
[:grid]
[:histogram [1 2 3 1 2 3 1 2 3]]
[:rug (map second seattle-weather) {:size 2}]
[:scatter data {:shape (fn [[_ _ v] _] ...) :size 18 :stroke {:size 2} :color (fn [[_ _ v] _] ...)}]
[:stack-horizontal [:extent-stat barley-variety-yield {:extent-type :bci :shape \O :size 5 :stroke {:size 1} :color :black}]]
```

The first example has no data and no configuration. The last one has nested chart definition as data.

### Input data

Every chart type expects some specific data structure for input, usually it's a sequence of values or sequence of sequences. For chart with categorical scales maps or key-value pairs are expected. Sometimes chart rendering depends on data. For example histograms. When provided map of sequences (instead of just sequence of values) multihistogram will be created.

### Configuration

Every chart type has it's own configuration for styling and behaviour. Default one can be find in `config.clj` file. Configuration provided by user is (deeply) merged with default one.

#### Function parameters

Most of the configuration parameters are just pure values but for some types you can provide function instead. Such function is called during rendering. This can be used to vary color/shape. Such function accepts current data point and fully merged configuration.

#### Injected parameters

During chart preparation additional parameters are injected to the configuration. These are:

- `:position` - position in the lattice (more on this later)
- `:series-id` - when charts are layered over it's consecutive number (starting `0`)
- `:id` - for categorical charts (vertical/horizontal) it's category id (more on this later)
- `:chart-type` - chart name
- `:extent` - calculated extent for each dimension
- `:label` - position label

#### Functions

Each chart is built using three functions called in different steps of process. All are multimethods with chart name as a dispatch.

* `prepare-data` - convert data into internal format. Accepts data and merged config, returns data
* `data-extent` - calculate extents of domains. Accepts internal data and config, returns map of extents
* `render-graph` - (name to be changed) actual render. Accepts data, config and rendering config (size and scales), returns BufferedImage.

## Lattice / layering

By default all charts are created in faceted view. When position is not given, default is [0,0].
When you have defined more than one chart at given position they will be layered over (empty chart is fully transparent).
Each layer gets its own `series-id` for given position (it is consecutive integer). 

## Sides

Every side of the lattice (left, right, top and bottom) has a placeholders for axes and small subcharts like rug or strip, maybe legends. About this later.

## Build phase data flow

Data flow goes like that:

* first list of series is created
* chart definitions are grouped by position
* chart configuration is merged, additional parameters are injected
* data preparation is called
* extents are calculated
* for every lattice row and column, extents are merged
* based on extent types automatic scales are calculated (for each row and column separately)

Final map contains:

* `:series` - map of series (keys are positions)
* `:cols` - number of columns
* `:rows` - number of rows
* `:extents` - list of extents for every row and column
* `:scales` - list of scale maps for every row and column
* `:bottom`, `:left`, `:right`, `:top` - map of list of series which will be rendered at sides of given row/column (for example axes)

Such map can be manipulated by some helper functions. You can add another series to the sides, you can change scales.

### Series

Series is a vector (it's crucial) of chart definitions. There are some helpers for creating such lists:

* `series` - to wrap list into the vector
* `add-serie` - to add serie at given lattice position (default [0,0])
* `add-series` - to concat to lists
* `add-multi` - helper for layered chart creation (you can create different configurations from a lists)
* `lattice` - to create faceted view (lattices can be layered over)

### Extents

Extent is an information about domain used later to create scales. For continuous data (numerical, temporal)
it's a vector with minimal and maximal value. For discrete data it's a sequence of values.

Extents can be merged.

#### Auto extend

During the process extents can be extended by using chart configuration parameter `:margins`.
Parameter defines fraction of range to be added to each side of extent.

```
(let [margins {:x [0.01 0.5]}
      extent {:x [:numerical [1 10]]}]
  (extend-domains extent margins))
;; => {:x [:numerical [0.91 14.5]], :y nil}
```

Note: currently this works only for `numerical` values

#### Example

Extent lattice 2x3:

```
{:x {0 [:numerical [-0.4 8.4]], 1 [:numerical [1.7 8.3]], 2 [:numerical [0.6499999999999999 8.35]]}
 :y {0 [:numerical [0.0 0.4196124702316519]], 1 [:numerical [0.0 0.42448510193413486]]}}
```

### Scales

Scales concept is slightly based on a D3 scales package.

There are three main types of scales used for chart generation:

* continuous, numerical - these are `:linear`, `:log`, `:pow`, `:log1p`, `:spline`
* continuous, temporal - `:time`, values have to be type of `java.time.temporal.Temporal`
* categorical - `:bands` - which creates evenly spaced set of ranges for discrete domain

There are also other scales, not used here (threshold, quantile).

Each scale is a `record` containing scale information like domain, forward/inverse functions, additional info used for example to create tick values.

Each scale works as a forward function.

#### Continuous

Each continuous scale by default maps domain to `[0.0 1.0]` range. Scale can be called as a function with:

* one parameter - to map value to default range
* three parameter - to map value to desired range

```
(s/log [1 10])
;; => #charts.scale.ContinuousRange{:start 1, :end 10, :type :log, :forward #function[charts.scale/log-forward/fn--23448], :inverse #function[charts.scale/log-inverse/fn--23453], :info {:base 10.0}}
((s/log [1 10]) 5)
;; => 0.6989700043360186
((s/log [1 10]) 111 123 5)
;; => 119.38764005203222
(s/inverse (s/log [1 10]) 0.5)
;; => 3.1622776601683804

(s/time-interval [(dt/local-date) (dt/local-date 2019 03 30)])
;; => #charts.scale.ContinuousRange{:start #object[java.time.LocalDate 0x6025234c "2019-03-28"], :end #object[java.time.LocalDate 0x60e60a32 "2019-03-30"], :type :time, :forward #function[charts.scale/time-forward/fn--23480], :inverse #function[charts.scale/time-inverse/fn--23483], :info {:time-diff-millis 1.728E8}}
((s/time-interval [(dt/local-date-time "2019-03-01T00:00:00") (dt/local-date-time "2019-03-03T00:00:00")]) (dt/local-date "2019-03-02"))
;; => 0.5
(s/inverse (s/time-interval [(dt/local-date-time "2019-03-01T00:00:00") (dt/local-date-time "2019-03-03T00:00:00")]) 0.1)
;; => #object[java.time.LocalDateTime 0x5e3ff815 "2019-03-01T04:48"]
```

#### Bands

Bands are used to divide continuous range into even ranges. They are used to create placeholders for stacked charts, find lattice positions etc.

Parameters are:

* bands - number of the bands (default: 1) or sequence of values
* padding-in - padding between bands (default: 0.0)
* padding-out - border padding (default: 0.0)
* align - position of the selected point (0.0 - left, 1.0 - right, 0.5 - midpoint, default)

```
(s/bands [:a :b])
;; => #charts.scale.OrdinalRange{:domain [:a :b], :range ({:start 0.0, :end 0.5, :point 0.25} {:start 0.5, :end 1.0, :point 0.75}), :type :bands, :forward {:a {:start 0.0, :end 0.5, :point 0.25}, :b {:start 0.5, :end 1.0, :point 0.75}}, :inverse #function[charts.scale/bands-inverse-fn/fn--23540], :info {:bandwidth 0.5, :step 0.5, :value :point}}
((s/bands [:a :b]) :a)
;; => {:start 0.0, :end 0.5, :point 0.25}
((s/bands {:padding-in 0.2 :padding-out 0.2 :align 0.25} [:a :b]) :a)
;; => {:start 0.09090909090909091, :end 0.4545454545454546, :point 0.18181818181818182}
```

#### Scale map

Scale map is reacher set of information for given domain. It contains:

* `:domain` - as domain (without a type)
* `:fmt` - formatting function
* `:ticks` - list of tick values (infered)
* `:scale` - scale itself
* `:scale-def` - scale definition

```
{:domain [-0.4 8.4], :fmt #function[clojure.core/str], :ticks (0.0 2.0 4.0 6.0 8.0), :scale #charts.scale.ContinuousRange{:start -0.4, :end 8.4, :type :linear, :forward #function[fastmath.core/make-norm/fn--352], :inverse #function[clojure.core/partial/fn--5826], :info nil}, :scale-def [:linear]}
```

To create scale map call `scale-map` function and provide:

* scale definition as a vector of scale name and scale parameters, like `[:linear]`, `[:pow 0.5]` or `[:bands {:padding-in 0.1}]`
* map with domain, fmt and ticks (they will be infered if not provided)
    * `:fmt` can be string (it's then is a formatting definition) or any function
    * `:ticks` this can be sequence of values or just requested number of ticks

To update scale map call `update-scale` and provide: scale map, key and value. This function has some logic to recreate ticks and other stuff when for example domain is changed.

To manipulate chart scales use functions: `update-scale`, `update-scales` and `tie-domains`

### Sides

To add some subchart or axis for given column or row use `add-side` and `add-axes` function. They place given series (or axes) at given position and side.

## Stacking

To display bar chart or set of violin plots stacking meta chart is used. Stacked chart definition follows general rule. It should containt: data and config. In this case data contains definition of inner chart.

// TODO: describe more

## Rendering phase data flow

When chart data are processed renderer (the only one `render-lattice`) creates image.

Input parameters are:

* `:padding-in`, padding-out of lattice (default: padding-in 0.05, padding-out 0.0)
* `:width` and height of target image (default: 600x600)
* `:border`, number of pixels left empty (default: 15)
* `:background`, background color or image

Process steps:

* calculate sizes
* calculate lattice positions
* render every chart grouped by positions (bottom->up, left->right)
* render every side
* return image

### Rendering charts

Charts are rendered on empty and fully transparent and properly oriented canvas (see below). Chart rendering function gets all information about desired size and scale maps that should be used.
Actual canvas is 50px bigger to enable drawing bigger shapes which shouldn't be cropped.

### Orientations

There are four orientations from the screen perspective:

* `:top` - default, where y axis is oriented up, [0,0] - bottom-left
* `:bottom` - y axis is oriented down, x axis to the right, [0,0] - top-left
* `:left` - y axis oriented to the left, x axis up, [0,0] - bottom-right
* `:right` - y axis oriented left, x axis up, [0,0] - bottom-left 

Orientation is used to properly display charts on sides (they are rotated)

### Colors

For colors, palettes and gradients use `clojure2d.color` namespace

### Strokes

Every (soon!) stroke definition follows [this configuration](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.core.html#var-set-stroke-custom)

### Shapes

* \O - filled circle
* \o - empty circle
* \S - filled square
* \s - empty square
* \A \V \{ \} - filled triangles
* \^ \v \< \> - empty triangles
* \- \| \\ \/ - strokes
* \+ - plus
* \x - x
* any other char - a char itself

Due to orientation on some charts shapes can be rotated (scatter plot has this fixed).

## Display / save

Just call `show` to display rendered chart and `save` to save to the file. Type of the file is taken from extension.
Number of possible formats may vary and depend on jvm/host. List of available formats is stored in the `clojure2d.core/img-writer-formats` var.
