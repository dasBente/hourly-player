(defpackage :hourly-player
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo :common-lisp)
  (:export :run))

(in-package :hourly-player)


(defparameter *current-hourly-path* "~/.hourlyplayer/CURRENT_HOURLY")
(defparameter *mute-hourly-path* "~/.hourlyplayer/MUTE")
(defparameter *path-to-icon* "/home/dasbente/.hourlyplayer/placeholder.png")


(let ((path-to-play-hourly "/home/dasbente/bin/play_hourly"))
  (defun play-hourly (&optional args)
    "Run play_hourly from shell with the argument list args (as string)"
    (uiop:run-program (concatenate 'string
				   path-to-play-hourly " " args)
		      :output '(:string :stripped t))))


(defun current-list (&optional new-list)
  "Get the current hourly list or change it to new-list"
  (play-hourly (if new-list
		   (concatenate 'string "-L " new-list)
		   "-C")))


(defun new-hourly ()
  "Change the hourly to a new random one"
  (play-hourly "-n"))


(defun current-default (&optional new-default)
  "Retrieve the current default hourly or set it to the given argument"
  (play-hourly (if new-default
		   (concatenate 'string "-D " new-default)
		   "-d")))

  
(defun is-mute ()
  "Checks whether the player is currently muted"
  (probe-file *mute-hourly-path*))


(defun toggle-mute ()
  "Toggles the current mute state of the player"
  (let ((mute (is-mute)))
    (if mute
	(delete-file mute)
	(with-open-file (str *mute-hourly-path* :direction :output)))))


(defun current-hourly (&optional new-hourly)
  "Get the current hourly or set it to the value of the optional new-hourly"
  (play-hourly (if new-hourly
		   (concatenate 'string "-N " new-hourly)
		   "-c")))
  

(defun random-hourly ()
  "Change to a new randomly selected hourly"
  (play-hourly "-n"))


(let ((curr-hourly 'nil))
  (defun update-curr-hourly (&optional register)
    "Register a curr-hourly element or update it's label"
    (if register
	(setf curr-hourly register)
	(setf (gtk-menu-item-label curr-hourly) (current-hourly)))))


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
		        (play-hourly)))
    
    ;; Attach items to menu
    (gtk-menu-shell-append menu current-hourly)
    (gtk-menu-shell-append menu mute)

    (gtk-widget-show-all menu)

    ;; Return the completed menu
    menu))
			     

(defun run ()
  (within-main-loop
   (let* ((icon (make-instance 'gtk-status-icon
			       :pixbuf (gdk-pixbuf-new-from-file
					*path-to-icon*)
			      :tooltip-text "Configure hourly player"))
	  (menu (build-menu)))

     ;; Register a user clicking the icon
     (g-signal-connect icon "popup-menu"
		       (lambda (widget button activate-time)
			 (declare (ignore widget button activate-time))
			 (format t "Icon clicked!~%")
			 (update-curr-hourly)
			 (gtk-menu-popup menu)))))
  (loop (sleep 60)))

