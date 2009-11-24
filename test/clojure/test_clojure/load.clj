;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Stephen C. Gilardi

;;  Tests for Clojure load functions
;;
;;  scgilardi (gmail)
;;  Created 15 November 2009

(ns clojure.test-clojure.load
  (:use clojure.test))

(def complex-load-actual
     (atom []))

(def complex-load-reference
     ["(clojure.core/load 'load.tree1a)\n"
      "(clojure.core/load 'load.tree1b)\n"
      "(clojure.core/load 'load.tree1c)\n"
      "(clojure.core/load 'load.tree1d1)\n"
      ";; loaded compiled load.tree1d1\n"
      "(clojure.core/load 'load.tree1d2)\n"
      "(clojure.core/load 'load.tree2a)\n"
      "(clojure.core/load 'load.tree2b1)\n"
      ";; loaded compiled load.tree2b1\n"
      "(clojure.core/load 'load.tree2b2)\n"
      ";; loaded compiled load.tree2b2\n"
      ";; loaded compiled load.tree2a\n"
      "(clojure.core/in-ns 'load.tree1d2)\n"
      "(clojure.core/refer 'load.tree2a"
      ")\n"
      ";; loaded compiled load.tree1d2\n"
      "(clojure.core/load 'load.tree1d3)\n"
      ";; loaded compiled load.tree1d3\n"
      ";; loaded compiled load.tree1c\n"
      ";; loaded compiled load.tree1b\n"
      ";; loaded compiled load.tree1a\n"
      "(clojure.core/load 'load.tree1a)\n"
      ";; skipped load.tree1a\n"
      ";; load.tree1a is stale, reloading\n"
      "(clojure.core/load 'load.tree1b)\n"
      "(clojure.core/load 'load.tree1c)\n"
      "(clojure.core/load 'load.tree1d1)\n"
      ";; skipped load.tree1d1\n"
      "(clojure.core/load 'load.tree1d2)\n"
      "(clojure.core/load 'load.tree3a)\n"
      "(clojure.core/load 'load.tree3b1)\n"
      ";; loaded compiled load.tree3b1\n"
      "(clojure.core/load 'load.tree3b2)\n"
      ";; loaded source load.tree3b2\n"
      ";; loaded compiled load.tree3a\n"
      "(clojure.core/in-ns 'load.tree1d2)\n"
      "(clojure.core/refer 'load.tree3a"
      ")\n"
      ";; loaded source load.tree1d2\n"
      "(clojure.core/load 'load.tree1d3)\n"
      ";; skipped load.tree1d3\n"
      ";; loaded source load.tree1c\n"
      ";; loaded source load.tree1b\n"
      ";; loaded source load.tree1a\n"
      ";; load.tree1a is stale, reloading\n"
      "(clojure.core/load 'load.tree1b)\n"
      "(clojure.core/load 'load.tree1c)\n"
      "(clojure.core/load 'load.tree1d1)\n"
      ";; skipped load.tree1d1\n"
      "(clojure.core/load 'load.tree1d2)\n"
      "(clojure.core/load 'load.tree3a)\n"
      "(clojure.core/load 'load.tree3b1)\n"
      ";; skipped load.tree3b1\n"
      "(clojure.core/load 'load.tree3b2)\n"
      ";; skipped load.tree3b2\n"
      ";; loaded source load.tree3a\n"
      "(clojure.core/in-ns 'load.tree1d2)\n"
      "(clojure.core/refer 'load.tree3a"
      ")\n"
      ";; loaded source load.tree1d2\n"
      "(clojure.core/load 'load.tree1d3)\n"
      ";; skipped load.tree1d3\n"
      ";; loaded source load.tree1c\n"
      ";; loaded source load.tree1b\n"
      ";; loaded source load.tree1a\n"])

(def load-warning-actual (atom nil))

(def load-warning-reference
     (str "load.source-missing2 is stale, but \"load/source_missing2.clj\" "
          "is not available to reload\n"))

(defn unit-source-file
  [unit]
  (let [url (.getResource (clojure.lang.RT/baseLoader)
                          (#'clojure.core/source-path unit))]
    (java.io.File. (.getPath url))))

(defn touch-source
  [unit]
  (let [file (unit-source-file unit)]
    (.setLastModified file (System/currentTimeMillis))))

(defn delete-source
  [unit]
  (.delete (unit-source-file unit)))

(defn rename-source
  [from-unit to-file]
  (.renameTo (unit-source-file from-unit) to-file))

(defn save-progress
  [fmt & args]
  (swap! complex-load-actual conj (apply format fmt args)))

(defn save-warning
  [fmt & args]
  (reset! load-warning-actual (apply format fmt args)))

(deftest test-load
  (testing "Should ignore self-loads without comment"
    (is (nil? (load 'load.cyclic0))))
  (testing "Should reject cyclic dependencies"
    (testing "a->b->a"
      (is (thrown-with-msg? Exception #".*Cyclic load dependency.*"
            (load 'load.cyclic1))))
    (testing "a->b->c->d->b"
      (is (thrown-with-msg? Exception #".*Cyclic load dependency.*"
            (load 'load.cyclic3)))))
  (testing "Should record load results"
    (testing "one record for each unit"
      (let [prev-count (count (load-records))]
        (load 'load.alib)
        (is (= (+ prev-count 2) (count (load-records))))))
    (let [[unit-record lib-record]
          (filter #(#{'load.aunit 'load.alib} (:name %)) (load-records))]
      (testing "in order"
        (is (= (:name unit-record) 'load.aunit))
        (is (= (:name lib-record) 'load.alib)))
      (testing "with sequential indexes"
        (is (= (:index unit-record) (dec (:index lib-record)))))
      (testing "with correct dependencies"
        (is (= (:dependencies lib-record) ['load.aunit])))))
  (testing "Should load code"
    (testing "from the loaded unit"
      (is (= ((ns-resolve 'load.alib 'libfunc)) "libfunc")))
    (testing "from the loaded unit's dependencies"
      (is (= ((ns-resolve 'load.alib 'unitfunc)) "unitfunc"))))
  (testing "Should ensure freshly loaded code"
    (binding [*load-progress-hook* save-progress]
      (testing "when everything is newly compiled"
        (load 'load.tree1a)
        (is (empty? (filter #(and (re-matches #"load\.tree.*" (-> % :name name))
                                  (= :source (:origin %))) (load-records)))))
      (testing "when a change triggers multiple reloads for staleness"
        (is (true? (touch-source 'load.tree3b2)))
        (let [tree1d2-file (unit-source-file 'load.tree1d2)]
          (is (true? (delete-source 'load.tree1d2)))
          (is (true? (rename-source 'load.tree1d2-new tree1d2-file)))
          (is (true? (touch-source 'load.tree1d2))))
        (load 'load.tree1a)
        (dorun (map #(is (= %1 %2))
                    @complex-load-actual
                    complex-load-reference)))))
  (testing "Should warn when compiled resource is stale, but no source is available"
    (binding [*load-warning-hook* save-warning]
      (load 'load.source-missing1)
      (is (true? (delete-source 'load.source-missing2)))
      (is (true? (touch-source 'load.source-missing4)))
      (load 'load.source-missing1)
      (is (= @load-warning-actual load-warning-reference)))))
