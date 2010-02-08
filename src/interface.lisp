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

(defun make-tree (&key (test #'<) (type :binary))
  (make-tree-intern test type))

(defgeneric get-tree-entry (tree key)
  (:documentation ""))

(defgeneric del-tree-entry (tree key)
  (:documentation "Remove a single element specified by key from a tree."))

(defgeneric clr-tree (tree)
  (:documentation "Remove all elements from a tree."))

(defsetf get-tree-entry update-tree-entry)


;;;
;;; internal functions
;;;

(defgeneric make-tree-intern (test type)
  (:documentation "Create a new tree map."))

(defgeneric update-tree-entry (tree key value)
  (:documentation "Insert a new key value pair into the tree."))

(defgeneric compare (tree a b)
  (:documentation "Compare element a and b with the tree's test function."))
