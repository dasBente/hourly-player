(in-package :hourly-player.play-hourly)


(defparameter *mute-hourly-path* "~/.hourlyplayer/MUTE")


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


(defun lines (str)
  (with-input-from-string (s str)
    (loop for line = (read-line s nil) until (null line)
	 collect line)))

     
(defun all-lists ()
  "Get all available hourly lists"
  (lines (play-hourly "-l")))


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
