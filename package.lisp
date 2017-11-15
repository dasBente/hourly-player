(defpackage :hourly-player.play-hourly
  (:use :cl)
  (:export :play-hourly :current-list :new-hourly :current-default :is-mute :toggle-mute
	   :current-hourly :random-hourly))

(defpackage :hourly-player
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo :hourly-player.play-hourly
	:common-lisp)
  (:export :run))

