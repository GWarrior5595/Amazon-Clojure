(defproject isbn_finder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                [org.clojure/data.zip "0.1.1"]
                [pradpi "0.2.1"]
                ; [amazonica "0.3.77"]
                ]
  :main ^:skip-aot isbn-finder.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
