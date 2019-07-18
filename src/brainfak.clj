(ns ^{:doc "Brainfuck (https://en.wikipedia.org/wiki/Brainfuck) interpreter
Brainfuck language:
> - increment the data pointer.
< - decrement the data pointer.
+ - increment the byte at the data pointer.
- - decrement the byte at the data pointer.
. - output the byte at the data pointer.
, - input a byte and store it in the byte at the data pointer.
[ - jump forward past the matching ] if the byte at the data pointer is zero.
] - jump backward to the matching [ if the byte at the data pointer is nonzero.
Any other byte in input is considered comment."}
 brainfak)

(defn matching-index
  "return next (advancing inc-dec-fn direction) pairwise matching position of ch in
  byte vector prog or nil if not found."
  [prog pos ch inc-dec-fn open close]
  (let [prog-count (count prog)]
    (loop [curpos (inc-dec-fn pos)
           stack '()]
      (if (or (< curpos 0) (>= curpos prog-count))
        nil
        (let [curch (char (get prog curpos))]
          (cond
            (and (empty? stack) (= curch ch))
            curpos

            (and (= curch open) (= (first stack) close))
            (recur (inc-dec-fn curpos) (rest stack))

            (and (= (first stack) open) (= curch close))
            (recur (inc-dec-fn curpos) (rest stack))

            (or (= curch open) (= curch close))
            (recur (inc-dec-fn curpos) (conj stack curch))

            :else
            (recur (inc-dec-fn curpos) stack)))))))

(defn run
  "execute brainfuck bytecodes from vector prog"
  ([prog] (run prog 30000))
  ([prog mem-size]
   (let [mem (byte-array (repeat mem-size 0))
         prog-count (count prog)
         prog (mapv byte prog)]
     (loop [pc 0 mp 0]
       (if (and (>= pc 0) (< pc prog-count))
         (let [ch (char (get prog pc))]
           (condp = ch
             \< (recur (inc pc) (dec mp))
             \> (recur (inc pc) (inc mp))
             \+ (do
                  (aset-byte mem mp (inc (aget mem mp)))
                  (recur (inc pc) mp))
             \- (do
                  (aset-byte mem mp (dec (aget mem mp)))
                  (recur (inc pc) mp))
             \. (do
                  (print (char (aget mem mp)))
                  (recur (inc pc) mp))
             \, (do
                  (aset-byte mem mp (byte ch))
                  (recur (inc pc) mp))
             \[ (if (zero? (aget mem mp))
                  (recur (inc (matching-index prog pc \] inc \[ \]))
                         mp)
                  (recur (inc pc) mp))
             \] (if-not (zero? (aget mem mp))
                  (recur (inc (matching-index prog pc \[ dec \[ \]))
                         mp)
                  (recur (inc pc) mp))
             (recur (inc pc) mp))))))))

(defn usage []
  (println "Usage:\n clj -m brainfak -f <input file>")
  (println " clj -m brainfak <input>"))

(defn -main
  ([] (usage))
  ([input]
   (if (not= "-f" input) (run input) (usage)))
  ([flag fname]
   (if (and (not= "-f" flag) (nil? fname))
     (usage)
     (run (slurp fname)))))

(comment
  ;; Hello world!
  (run ">+++++++++[<++++++++>-;
]<.;>+++++++[<++++>-;
]<+.+++++++..+++.[-]>++++++++[<++++>-;
] <.;>+++++++++++[<++++++++>-;]<-.--------.;
+++.------.--------.[-]>++++++++[<++++>-;
 ]<+.[-]++++++++++."))
