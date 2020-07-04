(defproject cljplot "0.0.3-SNAPSHOT"
  :description "JVM chart library"
  :url "https://github.com/generateme/cljplot"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [generateme/fastmath "1.5.3-SNAPSHOT"]
                 [clojure2d "1.3.1"] 
                 [clojure.java-time "0.3.2"]
                 [techascent/tech.ml.dataset "2.0-beta-25"]
                 [techascent/tech.datatype "5.0-beta-10"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/data.json "1.0.0"]]
  :repl-options {:timeout 120000}
  :java-source-paths ["src"]
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :target-path "target/%s")
