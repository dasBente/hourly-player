(ns hourly-player.utils-test
  (:require [clojure.test :refer :all]
            [hourly-player.utils :refer :all]))

(def test-file "test_file")

(defmacro deftest-with-file
  "Creates a file, executes the body and removes the file again"
  [test-name [file-sym file-name] & body]
  `(deftest ~test-name
     (let [~file-sym ~file-name]
       ~@body
       (clojure.java.io/delete-file ~file-sym))))

(deftest-with-file file-lines-test 
  [test-file "test_file"]
  (spit test-file "This\nis\na\ntest")
  (is (= ["This" "is" "a" "test"] (file-lines test-file))))

(deftest-with-file read-config-test
  [test-file "test_file"]
  (spit test-file "hello=World\nthis=is\na=test")
  (is (= {:hello "World" :this "is" :a "test"} (read-config test-file))))

(deftest-with-file write-config-test
  [test-file "test_file"]
  (write-config {:config "should" :be "written" :into "file"} test-file)
  (is (= "config=should\nbe=written\ninto=file" (slurp test-file))))

(deftest-with-file config->-test
  [test-file "test-file"]
  (spit test-file "our=initial\nconfig=file")
  (config-> test-file
            (assoc :our "new")
            (assoc :with "additional args"))
  (is (= "our=new\nconfig=file\nwith=additional args" (slurp test-file))))

