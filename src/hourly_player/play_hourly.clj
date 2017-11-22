(ns hourly-player.play-hourly
  (:require [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hourly-player.utils :refer :all]))

(def hourlies-path (.getFile (io/resource "hourlies")))
(def config-path (.getFile (io/resource "config")))
(def lists-path (.getFile (io/resource "lists")))

(defn hourly-path
  "Checks if a given hourly/clip combo exists and returns it's string path or nil if it does not."
  [hourly clip]
  (let [path (io/file (str hourlies-path "/" hourly "/" clip ".wav"))]
    (when (.exists path) (str path))))

(defn play-hourly
  "Plays the given clip for a given hourly"
  ([hourly clip]
   (sh "aplay" (hourly-path hourly clip)) ;; TODO: Move away from aplay, make asynchronous
   nil)
  ([config-map]
   (play-hourly (:current config-map) (hour))
   config-map))
  
(defn needs-update?
  "Check whether the date saved in config is the same as the current one"
  [config-map]
  (not= (:last-update config-map) (date)))

(defn hourlies
  "Gets a list of all hourlies. Returns a lazy seq"
  []
  (into (sorted-set) (.list (io/file hourlies-path))))

(defn lists
  "Gets a list of all known hourlies as a seq"
  []
  (into (sorted-set) (.list (io/file lists-path))))

(defn hourlies-from-list
  "Get all hourlies from a given list"
  ([] (hourlies))
  ([hourly-list]
   (if (str/blank? hourly-list)
     (hourlies)
     (let [complement (= "-" (subs hourly-list 0 1))]
       (if complement
         (clojure.set/difference (hourlies) (hourlies-from-list (subs hourly-list 1)))
         (into (sorted-set) (file-lines (str lists-path "/" hourly-list))))))))

(defn random-hourly
  "Generates a new hourly from all hourlies or given list"
  [& args] (rand-item (seq (apply hourlies-from-list args))))

(defn change-hourly
  "Updates the current hourly and changes date in a given config map"
  ([config-map]
   (change-hourly config-map (random-hourly (:list config-map))))
  ([config-map new-hourly]
   (-> config-map
       (assoc :current change-hourly)
       (assoc :last-update (date)))))

(defn refresh-hourly
  "Updates the hourly player config if necesary"
  [config-map]
  (if (needs-update? config-map)
    (change-hourly)
    config-map))

(defn run-hourly-player
  "Checks for a date change and updates the current hourly"
  [config-map]
  (-> config-map
      (refresh-hourly)
      (play-hourly)))

(defn init-hourly-config
  "Create a initial config map and updates it"
  []
  (refresh-hourly
   {:list "" :mute "0"}))
