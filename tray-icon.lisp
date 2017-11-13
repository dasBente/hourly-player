(defpackage :hourly-player
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo :common-lisp)
  (:export :run))

(in-package :hourly-player)


(defparameter *current-hourly-path* "~/.hourlyplayer/CURRENT_HOURLY")


(defparameter *mute-hourly-path* "~/.hourlyplayer/MUTE")


(defun is-mute ()
  "Checks whether the player is currently muted"
  (probe-file *mute-hourly-path*))


(defun toggle-mute ()
  "Toggles the current mute state of the player"
  (let ((mute (is-mute)))
    (if mute
	(delete-file mute)
	(with-open-file (str *mute-hourly-path* :direction :output)))))


(defun get-current-hourly ()
  "Retrieves the name of the current hourly"
  (let* ((res 'nil)
	 (curr (open *current-hourly-path*)))
    (setf res (read-line curr))
    (close curr)
    res))


(let ((curr-hourly 'nil))
  (defun update-curr-hourly (&optional register)
    "Register a curr-hourly element or update it's label"
    (if register
	(setf curr-hourly register)
	(setf (gtk-menu-item-label curr-hourly) (get-current-hourly)))))


(defun build-menu ()
  "Builds a pop-up menu for the icon. Currently rebuilds the menu every time!"
  (let ((menu (gtk-menu-new)) 
	(current-hourly (gtk-menu-item-new))
	(mute (make-instance 'gtk-check-menu-item
			     :label "Mute"
			     :active (is-mute))))

    ;; Process toggle signals
    (g-signal-connect mute "toggled"
		      (lambda (widget)
			(declare (ignore widget))
			(toggle-mute)))

    ;; Register current hourly item to updater
    (update-curr-hourly current-hourly)
    
    ;; Process pressing the current hourly button
    (g-signal-connect current-hourly "activate"
		      (lambda (widget)
			(declare (ignore widget))
			(format t "I work! ~%")
			(asdf:run-shell-command "./play_hourly")))
    
    ;; Attach items to menu
    (gtk-menu-shell-append menu current-hourly)
    (gtk-menu-shell-append menu mute)

    (gtk-widget-show-all menu)

    ;; Return the completed menu
    menu))
			     

(defun run ()
  (within-main-loop
   (let* ((icon (make-instance 'gtk-status-icon
			      :pixbuf (gdk-pixbuf-new-from-file "/home/dasbente/Bilder/128.png")
			      :tooltip-text "Configure hourly player"))
	  (menu (build-menu)))

     ;; Register a user clicking the icon
     (g-signal-connect icon "popup-menu"
		       (lambda (widget button activate-time)
			 (declare (ignore widget button activate-time))
			 (format t "Icon clicked!~%")
			 (update-curr-hourly)
			 (gtk-menu-popup menu))))))
