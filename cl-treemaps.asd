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

(defpackage cl-treemaps-system
  (:use :cl :asdf))

(in-package :cl-treemaps-system)

(defsystem cl-treemaps
  :name "cl-treemaps"
  :description "Common Lisp binary trees"
;  :long-description "Clon is a name derived from JSON and is in fact a object persitance layer which uses an
;  underlying database technology to realise the persitance."
  :author "Tobias Wich <tobias.wich@electrologic.org>"
  :version "0.1"
  :license "BSD"
;  :depends-on (flexi-streams ieee-floats cl-containers)
  :components
  ((:module src
	    :serial t
	    :components
	    ((:file "package")
	     (:file "interface")
	     (:file "red-black")))))


;; method to call tests
(defmethod perform ((o test-op) (c (eql (find-system 'cl-treemaps))))
  (operate 'test-op 'cl-treemaps-test))
