;;; cl-treemaps - Common LISP binary trees
;;; Copyright (C) 2010  Tobias Wich <tobias.wich@electrologic.org>
;;; 
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :cl-treemaps-test)


(def-suite redblack-tests
    :description "The test suite for the redblack tree.")
(in-suite redblack-tests)


(test fill-tree
  (let ((tree (make-tree :type :red-black)) ;; nothing can go wrong here
	(testlists)
	(testlist1 (remove-duplicates
		    (loop for i from 1 to 1000 collect i)))
	(testlist2 (remove-duplicates
		    (loop repeat 1000 for i = (random 1000000) collect i))))
    ;; check if correct tree was created
    (is (typep tree 'redblack-tree-map))
    ;; prepare testlist
    (setf testlists (append (list testlist1) (list testlist2)))
    ;; loop for every testlist
    (loop for testlist in testlists do
	 (progn
	   ;; insert all values
	   (loop for i in testlist do
		(progn
		  (is (= i (setf (get-tree-entry tree i) i)))
		  (is (not (zerop (cl-treemaps::rb-assert tree))))))
	   ;; search all values
	   (loop for i in testlist do
		(is (= i (get-tree-entry tree i) i)))
	   ;; delete all values
	   (loop for i in testlist do
		(progn
		  (is (del-tree-entry tree i))
		  (is (not (zerop (cl-treemaps::rb-assert tree))))))
	   ;; final check if tree is empty
	   (is (not (cl-treemaps::data tree)))))))

(test split-merge-tree
  (let ((tree (make-tree :type :red-black))) ;; nothing can go wrong here
    ;; insert values into tree
    (loop for i from 0 to 1000 do
	 (setf (get-tree-entry tree i) i))
    (let (tree-list)
      ;; split tree into equal sized halfs
      (setf tree-list (split-tree tree 500))
      ;; check if every half contains the correct elements
      (loop for i from 0 to 499 do
	   (is (= i (get-tree-entry (first tree-list) i) i)))
      (loop for i from 500 to 999 do
	   (is (= i (get-tree-entry (second tree-list) i) i)))
      ;; merge all entries from second tree into first
      (merge-trees (first tree-list) (second tree-list))
      ;; test if all elements are now in first tree
      (loop for i from 0 to 1000 do
	   (is (= i (get-tree-entry (first tree-list) i) i))))))

(test count-tree
  (let ((tree (make-tree :type :red-black))) ;; nothing can go wrong here
    ;; insert values into tree
    (loop for i from 0 to 100 do
	 (progn
	   (is (= (treemap-count tree) i))
	   (setf (get-tree-entry tree i) i)
	   (is (= (treemap-count tree) (1+ i)))))
    ;; delete values
    (loop for i from 100 downto 0 do
    	 (progn
    	   (is (= (treemap-count tree) (1+ i)))
    	   (del-tree-entry tree i)
    	   (is (= (treemap-count tree) i))))))

(test tree-iterator
  (let ((tree (make-tree :type :red-black))) ;; nothing can go wrong here
    ;; insert values into tree
    (loop for i from 0 to 100 do
	 (progn
	   (is (= (treemap-count tree) i))
	   (setf (get-tree-entry tree i) i)
	   (is (= (treemap-count tree) (1+ i)))))
    (with-treemap-iterator (iter tree)
      (loop for i from 0 to 100 do
	   (multiple-value-bind (a b c) (iter)
	     (declare (ignore a c))
	     (is (= i b)))))))
