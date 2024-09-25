(defproject cljplot "0.0.4-SNAPSHOT"
  :description "JVM chart library"
  :url "https://github.com/generateme/cljplot"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [clojure2d "1.5.0-SNAPSHOT"]
                 [clojure.java-time "1.4.2"]
                 [org.clojure/data.csv "1.1.0"]
                 [org.clojure/data.json "2.5.0"]]
  :repl-options {:timeout 120000}
  :java-source-paths ["src"]
  :resource-path "resources/"
  :javac-options ["--release" "8"]
  :target-path "target/%s")
