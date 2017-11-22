(ns hourly-player.tray-icon 
  (:require [hourly-player.play-hourly :refer :all]
            [clj-systemtray.core :refer :all]))

(defn callback 
  "Wraps a function call into a function which ignores a callback argument"
  [function & args] (fn [_] (apply function args)))

(defn build-popup
  "Generate a new popup consisting of a bunch of menu items"
  [config-map]
  (popup-menu
   (menu-item (:current config-map) (callback play-hourly config-map))))

(defn dispatch-tray
  [config-map]
  (make-tray-icon! (clojure.java.io/resource "placeholder.png") 
                   (build-popup config-map)))
