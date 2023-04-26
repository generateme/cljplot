(defproject cljplot "0.0.3"
  :description "JVM chart library"
  :url "https://github.com/generateme/cljplot"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clojure2d "1.4.5"]
                 [clojure.java-time "1.2.0"]
                 [org.clojure/data.csv "1.0.1"]
                 [org.clojure/data.json "2.4.0"]]
  :repl-options {:timeout 120000}
  :java-source-paths ["src"]
  :resource-path "resources/"
  :javac-options ["--release" "8"]
  :target-path "target/%s")
