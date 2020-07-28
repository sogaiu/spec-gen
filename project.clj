(defproject spec-gen "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot spec-gen.core
  :target-path "target/%s"
  :profiles {:socket-repl
             {:jvm-opts
              ["-Dclojure.server.repl={:port 7650 :accept clojure.core.server/repl}"]}
             :uberjar {:aot :all}})
