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

(in-package :cl-user)

(defpackage cl-treemaps-test-system
  (:use :cl :asdf))

(in-package :cl-treemaps-test-system)

(defsystem cl-treemaps-test
  :name "cl-treemaps-test"
  :description "Common LISP binary trees - Testpackage"
  :long-description "Test system for cl-treemaps."
  :author "Tobias Wich <tobias.wich@electrologic.org>"
  :version "0.1"
  :license "BSD"
  :depends-on (cl-treemaps FiveAM)
  :components
  ((:module test
	    :serial t
	    :components
	    ((:file "package")
	     (:file "testcases")
	     (:file "suites")))))


;; method to call tests
(defmethod perform ((o test-op) (c (eql (find-system 'cl-treemaps-test))))
  (operate 'load-op c)
  (funcall (intern "RUN-ALL-SUITES" :cl-treemaps-test))) ; important: string must be uppercase to match symbol
