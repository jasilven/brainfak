(ns brainfak2 ^{:doc "Brainfuck (https://en.wikipedia.org/wiki/Brainfuck) interpreter
(except input/',' is not implemented in this experiment) using instaparse."}
  (:require [instaparse.core :as insta]))

(def grammar "
  Program = Op* | Epsilon
  <Op> = Inc | Dec | IncP | DecP | Out | Loop | <';'> | <#'\\s+'>
  Inc = '+'; Dec = '-'; IncP = '>'; DecP = '<'; In = ','; Out = '.'
  Loop = <'['> Op* <']'>")

(defn runner []
  (let [mptr (atom 0)]
    (fn execute [node mem]
      (let [op (first node)]
        (condp = op
          :Program (doseq [n (rest node)] (execute n mem))
          :Inc (aset-byte mem @mptr (inc (aget mem @mptr)))
          :Dec (aset-byte mem @mptr (dec (aget mem @mptr)))
          :IncP (swap! mptr inc)
          :DecP (swap! mptr dec)
          :Out (-> (aget mem @mptr) char print)
          :Loop (when-not (zero? (aget mem @mptr))
                  (doseq [n (rest node)]
                    (execute n mem))
                  (when-not (zero? (aget mem @mptr))
                    (execute node mem)))

          nil)))))

(def parse (insta/parser grammar))
(def run (runner))

(comment
  ;; Hello world!
  (-> (parse ">+++++++++[<++++++++>-;]<.;>+++++++[<++++>-;]<+.+++++++..+++.[-]>++++++++[<++++>-;] <.;>+++++++++++[<++++++++>-;]<-.--------.; +++.------.--------.[-]>++++++++[<++++>-;]<+.[-]++++++++++.")
      (run (byte-array (repeat 100 0))))

  ;;
  )

