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

;(declaim (optimize (debug 3)))
(declaim (optimize (debug 0)
		   (safety 0)
		   (speed 3)
		   (compilation-speed 0)))

(defclass redblack-tree-map (tree-map)
  ((data    :accessor data :initform nil
	    :documentation "A node consists of the values key, data, color, left, right.")
   (testfun :accessor testfun :initarg :testfun :initform (error "No test function specified.")))
  (:documentation "Red-Black tree implementation."))


(defmethod make-tree-intern ((test function) (type (eql :red-black)))
  (let (tree)
    (setf tree (make-instance 'redblack-tree-map :testfun test))))


(defmethod clr-tree ((tree redblack-tree-map))
  (setf (data tree) nil)
  tree)


(defmethod get-tree-entry ((tree redblack-tree-map) key &optional (value nil update?))
  (let ((cmp (testfun tree))
	(path))
    (declare (type function cmp))

    (labels (;; main function for search, insert and update
	     (local-insert (node direction)
	       (if (not node)
		   (if update?
		       ;; node not existent, create and insert
		       (let ((new-node (list key value 'red nil nil)))
			 (cond ((not path) ; tree is empty
				(setf (data tree) new-node))
			       ((eq direction 'left) ; set left parent reference
				(setf (fourth (first path)) new-node))
			       ((eq direction 'right) ; set right parent reference
				(setf (fifth (first path)) new-node)))
			 ;; new node inserted, call repair function
			 (repair-insert1 new-node path)
			 (values value t))
		       ;; pure search method (can't find node)
		       (values nil nil))
		   ;; search in the tree
		   (let ((stored-key (first node)))
		     (if update? ; add node to path, so parent is visible
			 (push node path))
		     (cond
		       ;; stored-key = key
		       ((not (or (funcall cmp key stored-key) (funcall cmp stored-key key)))
			(if update?
			    ;; update value
			    (setf (second node) value))
			(values (second node) t))
		       ;; left path
		       ((funcall cmp key stored-key)
			(local-insert (fourth node) 'left))
		       ;; right path
		       ((funcall cmp stored-key key)
			(local-insert (fifth node) 'right))))))


	     ;;;; repair functions
	     ;; repair: parent not existant
	     (repair-insert1 (node path)
	       (if (not path)
		   (setf (third node) 'black)
		   (repair-insert2 node path)))

	     ;; repair: parent is black
	     (repair-insert2 (node path)
	       (if (eq (third (first path)) 'red) ; do nothing if black
		   (repair-insert3 node path)))

	     ;; repair: uncle and parent red
	     (repair-insert3 (node path)
	       (let* ((parent (first path))
		      (grandparent (second path))
		      (uncle (get-brother parent grandparent)))
		 (if (and uncle (eq 'red (third uncle))) ; uncle = red
		     (progn
		       (setf (third parent) 'black)
		       (setf (third uncle) 'black)
		       (setf (third grandparent) 'red)
		       (repair-insert1 grandparent (cddr path))) ; call first repair function
		     (repair-insert4 node path))))

	     ;; repair: black or no uncle and 
	     (repair-insert4 (node path)
	       (let ((parent (first path))
		     (grandparent (second path)))
		 (cond
		   ;; node right to parent and parent left to grandparent
		   ((and (eq node (fifth parent)) (eq parent (fourth grandparent)))
		    (rotate-left tree parent grandparent)
		    (repair-insert5 (fourth node) (append (list node) (cdr path))))
		   ;; node left to parent and parent right to grandparent
		   ((and (eq node (fourth parent)) (eq parent (fifth grandparent)))
		    (rotate-right tree parent grandparent)
		    (repair-insert5 (fifth node) (append (list node) (cdr path)))))))

	     ;; repair: node has black or no uncle and is left child of red father
	     (repair-insert5 (node path)
	       (let ((parent (first path))
		     (grandparent (second path))
		     (grandgrandparent (third path)))
		 (setf (third parent) 'black)
		 (setf (third grandparent) 'red)
		 (if (and (eq node (fourth parent)) (eq parent (fourth grandparent)))
		     ;; node is left to parent and parent left to grandparent
		     (rotate-right tree grandparent grandgrandparent)
		     ;; node is right to parent and parent right to grandparent
		     (rotate-left tree grandparent grandgrandparent)))))

      ;; call find/update/insert method
      (local-insert (data tree) nil))))


(defun get-brother (node parent)
  (let ((left (fourth parent))
	(right (fifth parent)))
    (if (eq left node)
	right
	left)))


(defun rotate-left (tree parent grandparent)
  (let* ((node (fifth parent))
	 (left-child (fourth node)))
    (setf (fourth node) parent) ; move node up and parent down
    (setf (fifth parent) left-child) ; move left child to place where node was
    (if grandparent
	(if (eq (fourth grandparent) parent) ;; parent was left child of grandparent
	    (setf (fourth grandparent) node)
	    (setf (fifth grandparent) node))
	(setf (data tree) node))))

(defun rotate-right (tree parent grandparent)
  (let* ((node (fourth parent))
	 (right-child (fifth node)))
    (setf (fifth node) parent) ; move node up and parent down
    (setf (fourth parent) right-child) ; move right child to place where node was
    (if grandparent
	(if (eq (fourth grandparent) parent) ;; parent was left child of grandparent
	    (setf (fourth grandparent) node)
	    (setf (fifth grandparent) node))
	(setf (data tree) node))))


;; (defmethod del-tree-entry ((tree redblack-tree-map) key)
;;   (let ((cmp (testfun tree))
;; 	(path))
;;     (declare (type function cmp))

;;     (labels (;; main function for search and remove
;; 	     (local-search (node)
;; 	       (if (not node)
;; 		   nil    ; no such node found in the tree
;; 		   (progn ; check out which node to take next
;; 		     (let ((stored-key (first node)))
;; 		       (push node path)
;; 		       (cond
;; 			 ;; stored-key = key
;; 			 ((not (or (funcall cmp key stored-key) (funcall cmp stored-key key)))
;; 			  (local-remove node)
;; 			 ;; left path
;; 			 ((funcall cmp key stored-key)
;; 			  (local-search (fourth node)))
;; 			 ;; right path
;; 			 ((funcall cmp stored-key key)
;; 			  (local-search (fifth node)))))))))

;; 	     ;; find maximum child
;; 	     (local-max (node)
;; 	       (cond ((fifth node) ; right child present
;; 		      (push node path)
;; 		      (local-max (fifth node)))
;; 		     (t            ; maximum found
;; 		      node)))
;; 	     ;; find minimum child
;; 	     (local-min (node)
;; 	       (cond ((fourth node) ; right child present
;; 		      (push node path)
;; 		      (local-min (fourth node)))
;; 		     (t            ; maximum found
;; 		      node)))
;; 	     ;; replace node a with node b
;; 	     (swap-nodes (a b)
;; 	       (setf (first a) (first b))
;; 	       (setf (second a) (second b)))

;; 	     ;; remove node
;; 	     (local-remove (node)
;; 	       (let (del-node)
;; 		 (cond
;; 		   ((fourth node) ; at least one left child present
;; 		    ;; find maximum starting from left child
;; 		    (setf del-node (local-max (fourth node)))
;; 		    ;; swap nodes
;; 		    (swap-nodes node del-node)
;; 		    ;; call deletion algorithm
;; 		    (del-child del-node 'left))

;; 		   ((fifth node) ; at least one right child
;; 		    ;; find minimum starting from right child
;; 		    (setf del-node (local-min (fifth node)))
;; 		    ;; swap nodes
;; 		    (swap-nodes node del-node)
;; 		    ;; call deletion algorithm
;; 		    (del-child del-node 'right))

;; 		   (t             ; no child here, empty tree and I'm done
;; 		    (clr-tree tree)))
;; 		 ;; delete node i found
;; 		 (local-remove-child del-node)
;; 		 t))

;; 	     (del-child (node child-at)
;; 	       (let ((child (if (eq child-at 'left) (fourth node) (fifth node))))
;; 		 (cond (child
;; 			(swap-nodes node child)
;; 			(if (eq (third node) 'black)
;; 			    (if (eq (third child) 'red)
;; 				(setf (third child) 'black)
;; 				(repair-delete1 child (append (list node) path)))))
;; 		       (t ; no child at node
			
;; 			))
;; 		 ;; finnally delete node
;; 		 (let ((parent (first path)))
;; 		   (cond (parent ; parent has node right if child-at is left
			  
;; 			  )
;; 			 (t      ; node is root
