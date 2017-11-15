;;;; -*- Lisp -*-

(defsystem "hourly-player"
    :depends-on (:cl-cffi-gtk
		 :cl-cffi-gtk-glib
		 :cl-cffi-gtk-gobject
		 :cl-cffi-gtk-gdk
		 :cl-cffi-gtk-gdk-pixbuf
		 :cl-cffi-gtk-gio
		 :cl-cffi-gtk-pango
		 :cl-cffi-gtk-cairo)
    :components ((:file "package")
		 (:file "play-hourly" :depends-on ("package"))
		 (:file "tray-icon" :depends-on ("package" "play-hourly"))))
