;;;; -*- Lisp -*-

(defsystem "hourly-player"
    :serial t
    :depends-on (:cl-cffi-gtk
		 :cl-cffi-gtk-glib
		 :cl-cffi-gtk-gobject
		 :cl-cffi-gtk-gdk
		 :cl-cffi-gtk-gdk-pixbuf
		 :cl-cffi-gtk-gio
		 :cl-cffi-gtk-pango
		 :cl-cffi-gtk-cairo)
    :components ((:file "package")
		 (:file "play-hourly")
		 (:file "tray-icon")))
