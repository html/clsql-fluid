;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql-fluid.asd
;;;; Purpose:  System definition for CLSQL-FLUID
;;;; Authors:  Stephen Compall
;;;; Created:  December 2008
;;;;
;;;; $Id$
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:clsql-fluid-system (:use #:asdf #:cl))
(in-package #:clsql-fluid-system)

(defsystem clsql-fluid
    :name "CLSQL-Fluid"
    :author "Stephen Compall <scompall@nocandysw.com>"
    :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
    :licence "Lessor Lisp General Public License"
    :description "Common Lisp SQL Fluid Connection Pools"
    :long-description "A full database type based on fluids, or
per-thread connections."
    :depends-on (clsql closer-mop bordeaux-threads)
    :components
    ((:module sql
	      :components
	      ((:module base
			:pathname ""
			:components
			((:file "fluid")))))))
