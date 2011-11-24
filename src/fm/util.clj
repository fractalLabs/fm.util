(ns fm.util
  (:require [clojure.contrib.string :as st]))

(defn safe-nil 
  "si o es nil, regresa 0 o x"
  ([o]
   (if (nil? o) 0 o))
  ([o x]
   (if (nil? o) x o)))

(defn pre-subcoll
  "toma desde x hasta y-1 elementos de coll
  con posible IndexOutOfBoundsException"
  [x y coll]
  (for [i (range  (- y x))]
    (nth coll (+ i x))))

(defn subcoll
  "toma desde x hasta y-1 elementos de coll o hasta donde pueda"
  [x y coll]
  (if-let [x1 (> x -1)]
    (if-let [y1 (< y (count coll))]
      (pre-subcoll x y coll)
      (pre-subcoll x (count coll) coll))
    (subcoll 0 y coll)))

(defn keep-meta
  "si un objeto tiene meta, al pasar por una funcion preserva esa meta"
  [f o]
  (let [met (meta o)]
    (with-meta (f o) met)))

(defn re-num
  "obtiene la lista de todos los numeros en una string"
  [s]
  (re-seq #"[0-9]+" s))

(defn re-trim
  "elimina las coicidencias de rege en string"
  [rege string] (st/replace-re rege "" string))

(defmacro false-on-exception
  "si durante la ejecucion de body surge una excepcion regresa false"
  [& body] `(try ~@body (catch Exception ~'e (do (print ~'e) false))))

(defmacro tryy
  "cacha cualquier excepcion causada por body"
  [& body] `(try ~@body (catch Exception ~'e)))

(defmacro tryce
  "cacha la excepcion ce al ejecutar body"
  [ce & body] `(try ~@body (catch ce ~'e)))

(defn and*
  "aplica and a una lista de elementos, si encuentra un nil o false regresa falso, de otro modo true"
  [& vals]
  (let [bool-vals (map boolean vals)]
    (not (some #(= false %) bool-vals))))

(defn or*
  "aplica or a una lista de elementos, si encuentra un elemento distinto de false o nil, es verdadero de otro modo false"
  [& vals]
  (let [bool-vals (map boolean vals)]
    (some #(= true %) bool-vals)))
