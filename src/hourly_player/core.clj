(ns hourly-player.core
  (:require [timely.core :as timely]
            [hourly-player.play-hourly :refer :all])
  (:gen-class))

(defn run
  "Run the hourly player once an hour and update it every time"
  []
  (timely/start-scheduler)
  (let [item (timely/scheduled-item
              (timely/hourly) 
              (fn [] (hourly-player.utils/config-> (clojure.java.io/resource "config")
                               (run-hourly-player))))]
    (timely/start-schedule item)))

(defn -main
  "Start hourly player schedule."
  [& args]
  (run)
  ()
  (loop []
    (Thread/sleep (* 1000 60 2))
    (recur)))
