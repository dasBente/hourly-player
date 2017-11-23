(ns hourly-player.tray-icon 
  (:require [hourly-player.play-hourly :refer :all]
            [hourly-player.utils :refer :all])
  (:import [java.awt.event ActionListener MouseListener]
           [java.awt SystemTray TrayIcon PopupMenu MenuItem Toolkit]
           [javax.swing JPopupMenu JMenuItem]))

(defn maybe-show-popup
  "Takes a mouse event and a popup callback that is called when the popup was truly triggered"
  [mouse-event popup-callback icon]
  (when (.isPopupTrigger mouse-event)
    (let [popup (popup-callback)]
      (.setPopupMenu icon popup))))

(defn updating-icon
  "Icon that uses a builder function to create a new popup menu each time it's activated"
  [image popup-builder]
  (let [icon (proxy [TrayIcon] [image]
               (getPopupMenu [] (popup-builder)))]
    (.setImageAutoSize icon true)
    (.setPopupMenu icon (popup-builder))
    icon))

(defn spawn-popup-menu
  "Create a popup menu containing nothing but a single button"
  []
  (let [popup (PopupMenu. "Test")
        config (read-config config-path)]
    (.add popup (MenuItem. (:current config)))
    popup))

(defn register-tray-icon!
  "Get the system tray and register a given icon there"
  [icon]
  (let [tray (SystemTray/getSystemTray)]
    (.add tray icon)
    icon))

(defn dispatch-tray-icon!
  "Creates a tray icon with a given icon and optional popup building 0-ary function to build a 
  popup on click"
  [image popup-builder]
  (-> image
      (updating-icon popup-builder)
      (register-tray-icon!)))

(defn image-from-path
  "Create a icon-ready image object from a given path"
  [path]
  (.getImage (Toolkit/getDefaultToolkit) path)) ;Maybe use Buffered Image?

(defn run-tray-icon!
  "Dispatches a tray icon"
  []
  (dispatch-tray-icon! (image-from-path (clojure.java.io/resource "placeholder.png"))
                       spawn-popup-menu))

(defn test-tray-icon!
  "Dispatches a tray icon and removes it again after user input in the REPL"
  []
  (let [tray (SystemTray/getSystemTray)
        icon (run-tray-icon!)]
    (read)
    (.remove tray icon)))
