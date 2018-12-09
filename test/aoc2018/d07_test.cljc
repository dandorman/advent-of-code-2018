(ns aoc2018.d07-test
  (:require [aoc2018.d07 :as d :refer [solve-1 solve-2]]
            [clojure.test :refer [deftest is testing]]))


(def example "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")


(def input "Step B must be finished before step G can begin.
Step G must be finished before step J can begin.
Step J must be finished before step F can begin.
Step U must be finished before step Z can begin.
Step C must be finished before step M can begin.
Step Y must be finished before step I can begin.
Step Q must be finished before step A can begin.
Step N must be finished before step L can begin.
Step O must be finished before step A can begin.
Step Z must be finished before step T can begin.
Step I must be finished before step H can begin.
Step L must be finished before step W can begin.
Step F must be finished before step W can begin.
Step T must be finished before step X can begin.
Step A must be finished before step X can begin.
Step K must be finished before step X can begin.
Step S must be finished before step P can begin.
Step M must be finished before step E can begin.
Step E must be finished before step W can begin.
Step D must be finished before step P can begin.
Step P must be finished before step W can begin.
Step X must be finished before step H can begin.
Step V must be finished before step W can begin.
Step R must be finished before step H can begin.
Step H must be finished before step W can begin.
Step N must be finished before step I can begin.
Step X must be finished before step R can begin.
Step D must be finished before step V can begin.
Step V must be finished before step R can begin.
Step F must be finished before step K can begin.
Step P must be finished before step R can begin.
Step P must be finished before step V can begin.
Step S must be finished before step X can begin.
Step I must be finished before step S can begin.
Step J must be finished before step N can begin.
Step T must be finished before step S can begin.
Step T must be finished before step R can begin.
Step K must be finished before step P can begin.
Step N must be finished before step R can begin.
Step G must be finished before step T can begin.
Step I must be finished before step V can begin.
Step G must be finished before step Q can begin.
Step D must be finished before step H can begin.
Step V must be finished before step H can begin.
Step T must be finished before step K can begin.
Step T must be finished before step W can begin.
Step E must be finished before step H can begin.
Step C must be finished before step R can begin.
Step L must be finished before step K can begin.
Step G must be finished before step Y can begin.
Step Y must be finished before step O can begin.
Step O must be finished before step E can begin.
Step U must be finished before step S can begin.
Step X must be finished before step W can begin.
Step C must be finished before step D can begin.
Step E must be finished before step P can begin.
Step B must be finished before step R can begin.
Step F must be finished before step R can begin.
Step A must be finished before step D can begin.
Step G must be finished before step M can begin.
Step B must be finished before step Q can begin.
Step Q must be finished before step V can begin.
Step B must be finished before step W can begin.
Step S must be finished before step H can begin.
Step P must be finished before step X can begin.
Step I must be finished before step M can begin.
Step A must be finished before step S can begin.
Step M must be finished before step X can begin.
Step L must be finished before step S can begin.
Step S must be finished before step W can begin.
Step L must be finished before step V can begin.
Step Z must be finished before step X can begin.
Step M must be finished before step R can begin.
Step T must be finished before step A can begin.
Step N must be finished before step V can begin.
Step M must be finished before step H can begin.
Step E must be finished before step D can begin.
Step F must be finished before step V can begin.
Step B must be finished before step O can begin.
Step G must be finished before step U can begin.
Step J must be finished before step C can begin.
Step G must be finished before step F can begin.
Step Y must be finished before step M can begin.
Step F must be finished before step D can begin.
Step M must be finished before step P can begin.
Step F must be finished before step T can begin.
Step G must be finished before step A can begin.
Step G must be finished before step Z can begin.
Step K must be finished before step V can begin.
Step J must be finished before step Z can begin.
Step O must be finished before step Z can begin.
Step B must be finished before step E can begin.
Step Z must be finished before step V can begin.
Step Q must be finished before step O can begin.
Step J must be finished before step D can begin.
Step Y must be finished before step E can begin.
Step D must be finished before step R can begin.
Step I must be finished before step F can begin.
Step M must be finished before step V can begin.
Step I must be finished before step D can begin.
Step O must be finished before step P can begin.")


(deftest part-1
  (testing "example"
    (is (= "CABDFE" (solve-1 example))))
  (testing "input"
    (is (= "BGJCNLQUYIFMOEZTADKSPVXRHW" (solve-1 input)))))


(deftest part-2
  (testing "example"
    (is (= 15 (solve-2 example 2 0))))
  (testing "input"
    (is (= 1017 (solve-2 input 5 60)))))