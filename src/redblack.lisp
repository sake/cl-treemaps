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

(declaim (optimize (debug 3)))
;; (declaim (optimize (debug 0)
;; 		   (safety 0)
;; 		   (speed 3)
;; 		   (compilation-speed 0)))

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
  (let (ret-val ret-status)
    (labels ((local-< (a b)
	       (funcall (the function (testfun tree)) a b))
	     (local-= (a b)
	       (and (not (local-< a b))
		    (not (local-< b a))))

	     ;; search
	     (local-search (node)
	       (if (not node)
		   ;; no such node
		   nil
		   ;; else
		   (let ((node-key (rb-key node)))
		     (cond ((local-= key node-key)  ; node found
			    (setf ret-val (rb-value node))
			    (setf ret-status t)
			    nil)
			   (t
			    ;; go deeper in the appropriate direction
			    (local-search (rb-child node (if (local-< node-key key) 'right 'left))))))))


	     ;; update
	     (local-insert ()
	       (if (not (data tree))
		   ;; nothing in this tree yet
		   (progn
		     (setf ret-val (rb-value
				    (setf (data tree) (rb-make-node key value))))
		     (setf ret-status t))

		   ;; else: tree not empty
		   (let ((head (rb-make-empty-node))
			 (g) (t-node) ; grandparent & parent
			 (p) (q) ; iterator & parent
			 (dir 'left)
			 (last-dir))
		     ;; set up values
		     (setf t-node head)
		     (setf g nil)
		     (setf p nil)
		     (setf (rb-right t-node)
			   (setf q (data tree)))

		     ;; search down tree
		     (loop do
			  (progn
			    (cond ((not q)
				   ;; insert new node
				   (setf (rb-child p dir)
					 (setf q (rb-make-node key))))

				  ;; color flip
				  ((and (is-red (rb-left q))
					(is-red (rb-right q)))
				   (setf (rb-color q) 'red)
				   (setf (rb-color (rb-left q)) 'black)
				   (setf (rb-color (rb-right q)) 'black)))

			    ;; fix red violation
			    (if (and (is-red q) (is-red p))
				(let ((dir2 (if (eq (rb-right t-node) g) 'right 'left)))
				  (setf (rb-child t-node dir2)
					(if (eq q (rb-child p last-dir))
					    (rb-rotate-single g (not-dir last-dir))
					    (rb-rotate-double g (not-dir last-dir))))))

			    ;; this is the node
			    (if (local-= (rb-key q) key)
				(progn
				  (setf ret-val
					(setf (rb-value q) value))
				  (setf ret-status t)
				  (loop-finish)))

			    ;; update helpers
			    (setf last-dir dir)
			    (setf dir (if (local-< (rb-key q) key) 'right 'left))
			    (if g (setf t-node g))
			    (setf g p)
			    (setf p q)
			    (setf q (rb-child q dir))))

		     ;; update root
		     (setf (data tree) (rb-right head))))
	       ;; make root black
	       (setf (rb-color (data tree)) 'black)))

      (cond (update?
	     (local-insert))
	    (t
	     (local-search (data tree)))))
    (values ret-val ret-status)))
		     

;;;
;;; internal helper functions
;;;

(defun rb-key (node &optional (value nil update?))
  (if update?
      (setf (first node) value)
      (first node)))
(defsetf rb-key rb-key)

(defun rb-value (node &optional (value nil update?))
  (if update?
      (setf (second node) value)
      (second node)))
(defsetf rb-value rb-value)


(defun rb-color (node &optional (value nil update?))
  (if update?
      (setf (third node) value)
      (third node)))
(defsetf rb-color rb-color)

(defun is-red (node)
  (and node
       (eq (rb-color node) 'red)))

(defun is-black (node)
  (not (is-red node)))


(defun rb-child (node direction &optional (value nil update?))
  (if (eq direction 'left)
      ;; get left child
      (if update?
	  (setf (fourth node) value)
	  (fourth node))
      ;; get right child
      (if update?
	  (setf (fifth node) value)
	  (fifth node))))
(defsetf rb-child rb-child)

(defun rb-left (node &optional (value nil update?))
  (if update?
      (setf (rb-child node 'left) value)
      (rb-child node 'left)))
(defsetf rb-left rb-left)

(defun rb-right (node &optional (value nil update?))
  (if update?
      (setf (rb-child node 'right) value)
      (rb-child node 'right)))
(defsetf rb-right rb-right)


(defun not-dir (direction)
  (if (eq direction 'left)
      'rigth
      'left))


(defun rb-rotate-single (root direction)
  (let ((save (rb-child root (not-dir direction))))
    ;; rotate
    (setf (rb-child root (not-dir direction)) (rb-child save direction))
    (setf (rb-child save direction) root)
    ;; recolor
    (setf (rb-color root) 'red)
    (setf (rb-color save) 'black)
    save))

(defun rb-rotate-double (root direction)
  (setf (rb-child root (not-dir direction))
	(rb-rotate-single (rb-child root (not-dir direction)) (not-dir direction)))
  (rb-rotate-single root direction))


(defun rb-make-node (key &optional (value nil))
  (list key value 'red nil nil))

(defun rb-make-empty-node ()
  (list nil nil 'red nil nil))


(defun rb-set-node (old new)
  "Overwrite old node with new node. This is needed because pointers are not available."
  (setf (rb-key old) (rb-key new))
  (setf (rb-value old) (rb-value new))
  (setf (rb-color old) (rb-color new))
  (setf (rb-left old) (rb-left new))
  (setf (rb-right old) (rb-right new))
  nil)


;;;
;;; test helpers
;;;

(defun rb-assert (tree)
  (labels (
	   ;; comparison functions
	   (local-< (a b)
	     (funcall (the function (testfun tree)) a b))
	   (local-> (a b)
	     (local-< b a))
	   (local-= (a b)
	     (and (not (local-< a b))
		  (not (local-< b a))))
	   (local-<= (a b)
	     (or (local-< a b)
		 (local-= a b)))
	   (local->= (a b)
	     (or (local-> a b)
		 (local-= a b)))

	   ;; check function
	   (local-assert (root)
		    (let (lh rh)
		      (if (not root)
			  (return-from local-assert 1)
			  ;; else
			  (let ((ln (rb-left root))
				(rn (rb-right root)))
			    ;; consecutive red links
			    (if (is-red root)
				(if (or (is-red ln) (is-red rn))
				    (progn
				      (print "Red violation")
				      (return-from local-assert 0))))

			    (setf lh (local-assert ln))
			    (setf rh (local-assert rn))

			    ;; invalid binary search tree
			    (if (or (and ln (local->= (rb-key ln) (rb-key root)))
				    (and rn (local-<= (rb-key rn) (rb-key root))))
				(progn
				  (print "Binary tree violation")
				  (return-from local-assert 0)))

			    ;; black height mismatch
			    (if (and (not (zerop lh))
				     (not (zerop rh))
				     (not (= lh rh)))
				(progn
				  (print "Black violation")
				  (return-from local-assert 0)))

			    ;; only count black links
			    (if (and (not (zerop lh))
				     (not (zerop rh)))
				(return-from local-assert (if (is-red root) lh (1+ lh)))
				(return-from local-assert 0)))))))
    ;; call check for the tree
    (local-assert (data tree))))


;;;
;;; legacy
;;;

(defun get-brother (node parent)
  (let ((left (fourth parent))
	(right (fifth parent)))
    (if (eq left node)
	right
	left)))
