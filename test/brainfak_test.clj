(ns brainfak-test
  (:require [brainfak :as bf]
            [clojure.test :as t]))

(t/deftest brainfak-test
  (t/is (= 1 (bf/matching-index (mapv byte "[]") 0 \] inc \[ \])))
  (t/is (= 6 (bf/matching-index (mapv byte "_[_[]_]_") 1 \] inc \[ \])))
  (t/is (= 1 (bf/matching-index (mapv byte "_[_[]_]_") 6 \[ dec \[ \])))
  (t/is (= 1 (bf/matching-index (mapv byte "_(_()_)_") 6 \( dec \( \))))
  (t/is (nil? (bf/matching-index (mapv byte "[_[_]_") 0 \] inc \[ \])))
  (t/is (= "1" (with-out-str (bf/run ">+++++++[<+++++++>-;]<.;")))))
