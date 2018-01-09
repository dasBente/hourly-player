(defpackage :hourly-player/config
  (:use :cl)
  (:export :lines-from-stream
           :lines-from-file
           :lines
           :read-config
           :write-config))

(defpackage :hourly-player/play-hourly
  (:use :cl :hourly-player/config)
  (:export :play-hourly
           :current-list
           :new-hourly
           :current-default
           :is-mute
           :toggle-mute
	   :current-hourly
           :random-hourly
           :all-lists))

(defpackage :hourly-player/core
  (:use :gtk
        :gdk
        :gdk-pixbuf
        :gobject
        :glib
        :gio
        :pango
        :cairo
        :hourly-player/play-hourly
	:common-lisp)
  (:export :run))
