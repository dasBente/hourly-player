(ns hourly-player.tray-icon 
  (:require [hourly-player.play-hourly :refer :all]
            [hourly-player.utils :refer :all]
            [clj-systemtray.core :refer :all])
  (:import [java.awt.event ActionListener]))

(defn callback 
  "Wraps a function call into a function which ignores a callback argument"
  [function & args] (fn [_] (apply function args)))

(defn build-popup
  "Generate a new popup consisting of a bunch of menu items."
  [config-map]
  (popup-menu
   (menu-item (:current config-map) (callback play-hourly config-map))))

(defn dispatch-tray
  [callback]
  (let [tray-icon (make-tray-icon! (clojure.java.io/resource "placeholder.png") nil)]
    (.addActionListener tray-icon 
                        (proxy [ActionListener] []
                          (actionPerformed [event] (callback))))))

(defn run-tray
  []
  (dispatch-tray (fn [] (build-popup 
                         (read-config clojure.java.io/resource "config")))))
