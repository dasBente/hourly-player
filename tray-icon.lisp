(in-package :hourly-player)


(defparameter *path-to-icon* "/home/dasbente/.hourlyplayer/placeholder.png")


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
	;; next-hourly as sub menu?
	(next-hourly (gtk-menu-item-new-with-label "New hourly"))
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
			(play-hourly)))

    ;; Process pressing the next hourly button
    (g-signal-connect next-hourly "activate"
		      (lambda (widget)
			(declare (ignore widget))
			(random-hourly)))
    
    ;; Attach items to menu
    (gtk-menu-shell-append menu current-hourly)
    (gtk-menu-shell-append menu next-hourly)
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
			 (update-curr-hourly)
			 (gtk-menu-popup menu)))))
  (loop (sleep 60)))

