(in-package :hourly-player/config)

(defun lines-from-stream (str)
  "Iterates over all lines in a stream and returns the result as a list"
  (loop for line = (read-line str nil) until (null line)
       collect line))

(defun lines-from-file (path-to-file)
  "Make a list of all lines contained in a given file"
  (with-open-file (str path-to-file)
    (lines-from-stream str)))

(defun lines (str)
  (with-input-from-string (s str)
    (lines-from-stream s)))

(defun read-config (path-to-config)
  "Parses a given simple config file and returns it as a associative list"
  (let ((lines (lines-from-file path-to-config)))
    (reduce (lambda (res kv-pair) 
              (destructuring-bind (k v) kv-pair
                (cons (cons k v) res)))
            (mapcar (lambda (l) (cl-ppcre:split "=" l)) lines)
            :initial-value nil)))

(defun write-config (config-alist path-to-config )
  "Writes a associative list into a config file"
  (with-open-file (str path-to-config :direction :output :if-exists :supersede)
    (loop for kv-pair in config-alist do
         (destructuring-bind (k . v) kv-pair
           (format str "~&~a=~a" k v)))))
