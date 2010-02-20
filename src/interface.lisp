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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
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
  "Tree constructor which selects the appropriate tree type depending on the type keyword.

test is a function that can compare two distinct keys. The function must return
t if the first argument is less than the second and return nil otherwise."
  (make-tree-intern test type))


(defgeneric treemap-count (tree)
  (:documentation "Return the number of elements in the tree."))


(defgeneric clr-tree (tree)
  (:documentation "Remove all elements from the tree.
The empty tree is returned, which is the same object as supplied."))


(defgeneric get-tree-entry (tree key &optional value)
  (:documentation "Get the value for a given key, or insert/update if a value is supplied.

Multiple values are returned with the first being the value and the second a
boolean whether the key exists in the tree. If the second value is nil, means
key not present, than the first ohne is also nil. On an update/insert action
the second value will always be true, because if the element didn't exist
beforehand, it will certainly afterwards.

The distinction between a search and update/insert is made with the optional
parameter value. If it is not set, a search will be performed. Otherwise the
operation will be an update/insert. For convenience setf can be used with this
method."))

(defsetf get-tree-entry get-tree-entry)


(defgeneric del-tree-entry (tree key)
  (:documentation "Remove a single element specified by key from a tree."))


(defgeneric split-tree (tree index)
  (:documentation "Split a tree into two new trees.

Index is a number that describes the index of the split node when the
tree is seen as a list, which is established by a DFS algorithm. The
node corresponding to index is included in the second tree."))


(defgeneric merge-trees (first second)
  (:documentation "Merge the second tree into the first one The second tree is left untouched."))


(defgeneric map-tree (function tree)
  (:documentation "Apply function to all elements of the tree in sorted order.

Exactly like hash maps, the behaviour is undefined when the tree is altered while
the function runs. nil is always returned."))


(defmacro with-treemap-iterator ((iterator treemap) &body body)
  "Macro to establish a tree iterator similar to with-hash-table-iterator.

iterator is bound to an iterator function which returns one node per invocation.
The behaviour of the iterator function is described in tree-iterator-fun. The
iterator function is placed with a macrolet into the body.
treemap is the tree that will be iterated."
  (let ((iter-fun (gensym)))
    `(let ((,iter-fun (tree-iterator-fun ,treemap)))
       ;; macrolet to inject iterator into body
       (macrolet ((,iterator ()
		    '(funcall ,iter-fun)))
	 ,@body))))


;;;
;;; internal functions
;;;

(defgeneric make-tree-intern (test type)
  (:documentation "Create a new tree map of a specific type.

This method is called by make-tree and needs to be specialised by every tree
implementation. The specialisation is done with the type parameter."))

(defgeneric tree-dot (tree file)
  (:documentation "Serialise tree as dot file for further processing with graphviz."))

(defgeneric tree-iterator-fun (tree)
  (:documentation "Internal method to generate an interator function for the tree. This is used by
the with macro.

The return values of the returned function is multiple value list with 3 values.
The first value is a boolean to show if an element was found.
The second value is the key of the element.
The third value is the value of the element.
If no more elements are left in the iterator, then only nil is returned."))
