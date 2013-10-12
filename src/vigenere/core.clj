(ns vigenere.core
  (:require [clojure.set :as set]))

;;; Source code for the article http://jaxenter.com/clojure-a-whistle-stop-tour-48410.html
;;;

(def ALPHABET (into (hash-set) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defprotocol Encrypter
  (encrypt [this text])
  (decrypt [this text]))

(deftype SubstitutionCipher [efn dfn]
  Encrypter
  (encrypt [this text]
    (apply str (map efn (keep ALPHABET text))))
  (decrypt [this text]
    (apply str (map dfn (keep ALPHABET text)))))

(defn- to-int [x]
  (when x
   (- (int x) (int \A))))

(defn- to-char [x]
  (when x
   (char (+ (int x) (int \A)))))

(defn char-offset [n x]
    (-> (to-int x)
        (+ n)
        (mod 26)
        Math/abs
        to-char))

(defn rot13-cipher []
  (let [fn (partial char-offset 13)]
    (->SubstitutionCipher fn fn)))

(defn caesar-cipher [n]
  (let [efn (partial char-offset n)
        dfn (partial char-offset (- n))]
    (->SubstitutionCipher efn dfn)))

(defn deranged-cipher []
  (let [m  (zipmap ALPHABET (shuffle ALPHABET))
        m' (set/map-invert m)]
    (->SubstitutionCipher (partial get m) (partial get m'))))

(defn negative [x]
  (when x (- x)))

(deftype PolySubstitutionCipher [key]
  Encrypter
  (encrypt [this text]
    (apply str (map char-offset (keep (comp to-int ALPHABET) (cycle key)) text)))
  (decrypt [this text]
    (apply str (map char-offset (keep (comp negative to-int ALPHABET) (cycle key)) text))))

(defn vigenere-cipher [key]
  (->PolySubstitutionCipher key))

(comment
  (def rot13 (rot13-cipher))
  (assert (= "PLAINTEXT" (decrypt rot13 (encrypt rot13 "PLAINTEXT"))))

  (def caesar (caesar-cipher 5))
  (assert (= "PLAINTEXT" (decrypt caesar (encrypt caesar "PLAINTEXT"))))

  (def vigenere (vigenere-cipher "PETER PIPER PICKED A PECK OF PICKLED PEPPER"))
  (assert (= "PLAINTEXT" (decrypt vigenere (encrypt vigenere "PLAINTEXT"))))
)
