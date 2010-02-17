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

(in-package :cl-treemaps)


(defclass tree-map ()
  ()
  (:documentation "Base class for a tree map."))


;;;
;;; public interface functions
;;;

(defun make-tree (&key (test #'<) (type :red-black))
  (make-tree-intern test type))

(defgeneric clr-tree (tree)
  (:documentation "Remove all elements from a tree."))

(defgeneric get-tree-entry (tree key &optional value)
  (:documentation "Get the value for a given key, or insert/update if a value is supplied."))

(defgeneric del-tree-entry (tree key)
  (:documentation "Remove a single element specified by key from a tree."))

(defsetf get-tree-entry get-tree-entry)

(defgeneric split-tree (tree index)
  (:documentation "Split a tree into two new trees. Index is a number that describes the index of the split
node when the tree is seen as a list. The node corresponding to index is included in the second tree."))

(defgeneric merge-trees (first second)
  (:documentation "Merge the second tree into the first."))


;;;
;;; internal functions
;;;

(defgeneric make-tree-intern (test type)
  (:documentation "Create a new tree map."))

(defgeneric tree-dot (tree file)
  (:documentation "Serialise tree as dot file (graphviz)."))
