(ns hourly-player.tray-icon 
  (:require [hourly-player.play-hourly :refer :all]
            [hourly-player.utils :refer :all])
  (:import [java.awt.event ActionListener ItemListener]
           [java.awt SystemTray TrayIcon PopupMenu MenuItem CheckboxMenuItem Toolkit]
           [java.awt.image BufferedImage]))

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

(defn menu-item
  "Creates a menu item which will call a callback function upon being clicked (provided one was supplied to the item)"
  ([title] (menu-item title nil))
  ([title callback]
   (let [item (MenuItem. title)
         listener (proxy [ActionListener] []
                                  (actionPerformed [event] (callback)))]
     (when callback
       (.addActionListener item listener))
     item)))

(defn check-menu-item
  "Creates a check item with a given title and optional state (defaults to false) which calls a 
  optional callback function which is given the new state"
  ([title] (check-menu-item title nil false))
  ([title callback] (check-menu-item title callback false))
  ([title callback state] 
   (let [item (CheckboxMenuItem. title state)
         listener (proxy [ItemListener] []
                                  (itemStateChanged [event] 
                                    ()
                                    (let [new-state (not (.getState item))]
                                      (.setState item new-state)
                                      (callback (.getState new-state)))))]
     (when callback
       (.addItemListener item listener))
     item)))

(defmacro state->num
  [bool]
  `(if ~bool "1" "0"))

(defn spawn-popup-menu
  "Create a popup menu containing nothing but a single button"
  []
  (let [popup (PopupMenu.)
        config (read-config config-path)
        mute (= "1" (:mute config))]
    (.add popup (menu-item (:current config) (fn [] (play-hourly config))))
    (.add popup (menu-item (if mute "Unmute" "Mute")
                           (fn [] (write-config 
                                   (assoc config :mute (state->num (not mute)))
                                   config-path))))
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
