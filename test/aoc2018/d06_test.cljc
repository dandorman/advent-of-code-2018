(ns aoc2018.d06-test
  (:require [aoc2018.d06 :refer [solve-1 solve-2]]
            [clojure.test :refer [deftest is testing]]))


(def example "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")


(def input "336, 308
262, 98
352, 115
225, 205
292, 185
166, 271
251, 67
266, 274
326, 85
191, 256
62, 171
333, 123
160, 131
211, 214
287, 333
231, 288
237, 183
211, 272
116, 153
336, 70
291, 117
156, 105
261, 119
216, 171
59, 343
50, 180
251, 268
169, 258
75, 136
305, 102
154, 327
187, 297
270, 225
190, 185
339, 264
103, 301
90, 92
164, 144
108, 140
189, 211
125, 157
77, 226
177, 168
46, 188
216, 244
346, 348
272, 90
140, 176
109, 324
128, 132")


(deftest part-1
  (testing "example"
    (is (= 17 (solve-1 example))))
  (comment
    (testing "input"
      (is (= 3401 (solve-1 input))))))


(deftest part-2
  (testing "example"
    (is (= 16 (solve-2 32 example))))
  (comment
    (testing "input"
      (is (= 49327 (solve-2 10000 input))))))
