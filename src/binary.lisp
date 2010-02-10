;; ;;; cl-treemaps - Common LISP binary trees
;; ;;; Copyright (C) 2010  Tobias Wich <tobias.wich@electrologic.org>
;; ;;; 
;; ;;; This library is free software; you can redistribute it and/or
;; ;;; modify it under the terms of the GNU Lesser General Public
;; ;;; License as published by the Free Software Foundation; either
;; ;;; version 2.1 of the License, or (at your option) any later version.
;; ;;; 
;; ;;; This library is distributed in the hope that it will be useful,
;; ;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; ;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; ;;; Lesser General Public License for more details.
;; ;;; 
;; ;;; You should have received a copy of the GNU Lesser General Public
;; ;;; License along with this library; if not, write to the Free Software
;; ;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

;; (in-package :cl-treemaps)


;; (defclass binary-tree-map (tree-map)
;;   ((data    :accessor data :initform nil)
;;    (testfun :accessor testfun :initarg :testfun :initform (error "No test function specified.")))
;;   (:documentation "Binary tree implementation."))


;; (defmethod make-tree-intern ((test function) (type (eql :binary)))
;;   (let (tree)
;;     (setf tree (make-instance 'binary-tree-map :testfun test))))


;; (defmethod clr-tree ((tree binary-tree-map))
;;   (setf (data tree) nil)
;;   tree)


;; (defmethod get-tree-entry ((tree binary-tree-map) key)
;;   ;; do normal search
;;   (labels ((local-find (node)
;; 	     (if (not node)
;; 		 ;; node not existant
;; 		 (values nil nil)
;; 		 (let ((stored-key (first node)))
;; 		   (cond
;; 		     ;; stored-key = key
;; 		     ((not (or (compare tree key stored-key) (compare tree stored-key key)))
;; 		      (values (second node) t))
;; 		     ;; left path
;; 		     ((compare tree key stored-key)
;; 		      (local-find (third node)))
;; 		     ;; right path
;; 		     ((compare tree stored-key key)
;; 		      (local-find (fourth node))))))))
;;     ;; call find method
;;     (local-find (data tree))))


;; (defmethod update-tree-entry ((tree binary-tree-map) key value)
;;   (let ((cmp (testfun tree)))
;;   ;; do normal search
;;   (labels ((local-insert (node parent direction)
;; 	     (if (not node)
;; 		 ;; node not existant, create and insert
;; 		 (let ((new-node (list key value nil nil)))
;; 		   (cond ((not parent) ; tree is empty
;; 			  (setf (data tree) new-node))
;; 			 ((eq direction 'left) ;; set left parent reference
;; 			  (setf (third parent) new-node))
;; 			 ((eq direction 'right) ;; set right parent reference
;; 			  (setf (fourth parent) new-node)))
;; 		   value)
;; 		 ;; search in the tree
;; 		 (let ((stored-key (first node)))
;; 		   (cond
;; 		     ;; stored-key = key
;; 		     ((not (or (funcall cmp key stored-key) (funcall cmp stored-key key)))
;; 		      ;; update value
;; 		      (setf (second node) value)
;; 		      value)
;; 		     ;; left path
;; 		     ((funcall cmp key stored-key)
;; 		      (local-insert (third node) node 'left))
;; 		     ;; right path
;; 		     ((funcall cmp stored-key key)
;; 		      (local-insert (fourth node) node 'right)))))))
;;     ;; call find method
;;     (values (local-insert (data tree) nil nil) t))))


;; (defmethod compare ((tree binary-tree-map) a b)
;;   (funcall (testfun tree) a b))
