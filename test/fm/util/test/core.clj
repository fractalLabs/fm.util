(ns fm.util.test.core 
  (:use fm.namespaces 
        fm.util 
        clojure.test)) 

(deftest carga-todos
  (is (false-on-exception load-ns)))