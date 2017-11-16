(in-package :hourly-player)


(defparameter *path-to-icon* "/home/dasbente/.hourlyplayer/placeholder.png")


(defun build-list-chooser (current-list)
  (let ((window (make-instance 'gtk-window
			       :type :toplevel
			       :title "Choose a hourly list"
			       :default-width 250))
	(content-box (gtk-box-new :vertical 3))
	(combo-box (make-instance 'gtk-combo-box-text))
	(lists (all-lists))
	(button-box (gtk-box-new :horizontal 3))
	(button-conf (gtk-button-new-with-label "Set as list"))
	(button-comp (gtk-button-new-with-label "Set as complement list")))

    ;; Append available lists to combo box
    (mapcar (lambda (str) (gtk-combo-box-text-append-text combo-box str)) lists)

    ;; Set the default selection and add the combo box to the content box
    (gtk-combo-box-set-active combo-box (position
					 (if (equal "-" (subseq current-list 0 1))
					     (subseq current-list 1)
					     current-list)
					 lists :test #'equal))
    (gtk-box-pack-start content-box combo-box)


    ;; Confirm button (button-conf) was clicked
    (g-signal-connect button-conf "clicked"
		      (lambda (widget)
			(declare (ignore widget))
			(current-list (gtk-combo-box-text-get-active-text combo-box))
			(gtk-widget-destroy window)))

    ;; Complement button (button-comp) was clicked
    (g-signal-connect button-comp "clicked"
		      (lambda (widget)
			(declare (ignore widget))
			(current-list (concatenate 'string
						   "-"
						   (gtk-combo-box-text-get-active-text combo-box)))
			(gtk-widget-destroy window)))
    
    ;; Add buttons to box and add box to content box
    (gtk-box-pack-start button-box button-comp)
    (gtk-box-pack-end button-box button-conf)
    (gtk-box-pack-end content-box button-box)

    ;; Add content box to window
    (gtk-container-add window content-box)
    
    ;; Make the window visible
    (gtk-widget-show-all window)))


(defun build-menu ()
  "Builds a pop-up menu for the icon. Currently rebuilds the menu every time!"
  (let* ((menu (gtk-menu-new)) 
	 (current-hourly (gtk-menu-item-new-with-label (current-hourly)))
	 (next-hourly (gtk-menu-item-new-with-label "New hourly"))
	 (mute (make-instance 'gtk-check-menu-item
			      :label "Mute"
			      :active (is-mute)))
	 (current-list (current-list))
	 (item-current-list (gtk-menu-item-new-with-label
			(concatenate 'string "Current List: " current-list))))
    
    ;; Process toggle signals
    (g-signal-connect mute "toggled"
		      (lambda (widget)
			(declare (ignore widget))
			(toggle-mute)))
    
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

    ;; Process pressing the current list button
    (g-signal-connect item-current-list "activate"
		      (lambda (widget)
			(declare (ignore widget))
			(build-list-chooser current-list)))
    
    ;; Attach items to menu
    (gtk-menu-shell-append menu current-hourly)
    (gtk-menu-shell-append menu next-hourly)
    (gtk-menu-shell-append menu item-current-list)
    (gtk-menu-shell-append menu mute)

    (gtk-widget-show-all menu)

    ;; Return the completed menu
    menu))
			     

(defun run ()
  (within-main-loop
   (let* ((icon (make-instance 'gtk-status-icon
			       :pixbuf (gdk-pixbuf-new-from-file
					*path-to-icon*)
			      :tooltip-text "Configure hourly player")))

     ;; Register a user clicking the icon
     (g-signal-connect icon "popup-menu"
		       (lambda (widget button activate-time)
			 (declare (ignore widget button activate-time))
			 (gtk-menu-popup (build-menu))))))
  (loop (sleep 60)))

