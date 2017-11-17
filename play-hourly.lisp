(in-package :hourly-player.play-hourly)


(defparameter *mute-hourly-path* "~/.hourlyplayer/MUTE")


(let ((path-to-play-hourly "/home/dasbente/bin/play_hourly"))
  (defun play-hourly (&optional flag arg)
    "Run play_hourly from shell with the argument list args (as string)"
    (uiop:run-program (concatenate 'string
				   path-to-play-hourly " " flag " " arg)
		      :output '(:string :stripped t))))


(defun current-list (&optional new-list)
  "Get the current hourly list or change it to new-list"
  (play-hourly "-l" new-list))


(defun lines (str)
  (with-input-from-string (s str)
    (loop for line = (read-line s nil) until (null line)
	 collect line)))

     
(defun all-lists ()
  "Get all available hourly lists"
  (lines (play-hourly "-A")))


(defun all-hourlies (&optional from-list)
  "Get all hourlies listed in a given list"
  (lines (play-hourly "-a" from-list)))


(defun random-hourly (&optional from-list)
  "Change the hourly to a new random one"
  (play-hourly "-r" from-list))


(defun current-default (&optional new-default)
  "Retrieve the current default hourly or set it to the given argument"
  (play-hourly "-d" new-default))

  
(defun is-mute ()
  "Checks whether the player is currently muted"
  (probe-file *mute-hourly-path*))


(defun toggle-mute ()
  "Toggles the current mute state of the player"
  (play-hourly "-m"))


(defun current-hourly (&optional new-hourly)
  "Get the current hourly or set it to the value of the optional new-hourly"
  (play-hourly "-c" new-hourly))
