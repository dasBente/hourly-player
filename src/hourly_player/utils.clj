(ns hourly-player.utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn file-lines
  "Returns a list of all of a files lines"
  [file-path]
  (with-open [rdr (io/reader file-path)]
            (doall (line-seq rdr))))

(defn read-config
  "Reads the config file and generates a map with all relevant informations"
  [config-path]
  (reduce (fn [res line] 
            (let [[attr value] (str/split line #"=")]
              (assoc res (keyword attr) value)))
          {}
          (file-lines config-path)))

(defn write-config
  "Writes a map to the config file"
  [config-map config-path]
  (spit config-path 
        (str/join "\n" (map (fn [[k v]] (str (name k) "=" v))
                                       config-map))))

(defmacro config->
  "Threads the body between reading from and writing to a config file"
  [config-path & body]
  `(-> (read-config ~config-path)
       ~@body
       (write-config ~config-path)))

(defn rand-item
  "Returns a random item from a list of items"
  [items]
  (nth items (rand-int (count items))))

(defn hour
  "Returns the current hour"
  []
  (first (str/split (str (java.time.LocalTime/now)) #":")))

(defn date
  "Returns the current date"
  []
  (str (java.time.LocalDate/now)))
