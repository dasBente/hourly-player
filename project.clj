(defproject hourly-player "0.1.0-SNAPSHOT"
  :description "Tray icon and acompanying UI to the hourly player script"
  :url "https://github.com/dasbente/hourly-player"
  :license {:name "GPL"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [factual/timely "0.0.3"]]
  :main ^:skip-aot hourly-player.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
