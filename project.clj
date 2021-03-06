(defproject cljplot "0.0.2a-SNAPSHOT"
  :description "JVM chart library"
  :url "https://github.com/generateme/cljplot"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [generateme/fastmath "2.0.3"]
                 [clojure2d "1.4.0"] 
                 [clojure.java-time "0.3.2"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/data.json "1.0.0"]]
  :repl-options {:timeout 120000}
  :java-source-paths ["src"]
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :target-path "target/%s")
