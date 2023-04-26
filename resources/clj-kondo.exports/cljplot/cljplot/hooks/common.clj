(ns hooks.common)

(defmacro do-graph
  [graph-conf highest-render? & body]
  (let [c (symbol "c")]
    `(let [canv# (cljplot.common/graph-canvas ~graph-conf ~highest-render?)
           orient# (cljplot.common/canvas-orientation (:orientation ~graph-conf))]
       (cljplot.common/apply-body canv# orient# (fn [~c] ~@body))
       canv#)))

(defmacro xy-chart
  [conf series & mods]
  `(-> ~series
       ~@mods
       (cljplot.render/render-lattice ~conf)))
