;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     fluid.lisp
;;;; Purpose:  The fluid-database database type and methods
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2008 by Stephen Compall
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

;;; Fluids based on Bordeaux-Threads (upstream...)

(defpackage #:clsql-fluid-bt
  (:use #:cl)
  (:export #:make-fluid #:fluid-lock #:fluid-value)
  (:import-from #:bordeaux-threads #:make-lock #:current-thread
		#:thread-alive-p #:with-lock-held)
  (:documentation "Fluids implemented around Bordeaux-Threads."))

(in-package #:clsql-fluid-bt)

(defstruct fluid
  "A container for a different value in each thread.  Values are not
inherited."
  (lock (make-lock))
  (table (make-hash-table :test #'eql) :type hash-table :read-only t)
  (gc-function #'identity :type (or symbol cons function) :read-only t)
  (gc-count 0 :type (and (integer 0) fixnum))
  (gc-frequency 30 :type (and (integer 1) fixnum) :read-only t))

(defun fluid-value (fluid &optional (thread (current-thread)))
  "Answer two values: the value of FLUID for THREAD, and whether a
value is present."
  (let ((table (fluid-table fluid)))
    (with-lock-held ((fluid-lock fluid))
      (gethash thread table))))

(defun fluid-gc (fluid)
  "Clean up FLUID.  *Assume it is locked in this thread.*"
  (let (to-gc (table (fluid-table fluid)) (gcer (fluid-gc-function fluid)))
    (maphash (lambda (thread v)
	       (unless (thread-alive-p thread)
		 (push thread to-gc)
		 (funcall gcer v)))
	     table)
    (dolist (thread to-gc)
      (remhash thread table)))
  (setf (fluid-gc-count fluid) 0))

(defun (setf fluid-value) (new-value fluid &optional (thread (current-thread)))
  "Alter the value of FLUID for THREAD to NEW-VALUE, answering
NEW-VALUE."
  (with-lock-held ((fluid-lock fluid))
    (setf (gethash thread (fluid-table fluid)) new-value)
    (when (>= (incf (fluid-gc-count fluid))
	      (fluid-gc-frequency fluid))
      (fluid-gc fluid)))
  new-value)

(in-package #:clsql-sys)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(clsql-fluid-bt:make-fluid clsql-fluid-bt:fluid-value)))

;;; Slot forwarding

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+sub-db-forwarded-slots+)
    (defconstant +sub-db-forwarded-slots+
      '(name state autocommit transaction transaction-level
	attribute-cache))))

;; Lispworks requires these to be ready when compiling.
(eval-when (#+lispworks :compile-toplevel :load-toplevel :execute)
  (defclass sub-db-forwarding-class (standard-class)
    ()
    (:documentation "Forward some slots to an underlying database."))

  (defmethod c2mop:validate-superclass ((class sub-db-forwarding-class) superclass)
    "Allow standard-class, c2mop:standard-class, and myself."
    (and (eql (class-of class) (find-class 'sub-db-forwarding-class))
	 (loop with scc = (class-of superclass)
	       for ccname in '(standard-class c2mop::standard-class
			       sub-db-forwarding-class)
	       thereis (eql scc (find-class ccname))))))

(defmethod c2mop:slot-value-using-class
    ((class sub-db-forwarding-class) inst slot-def)
  (let ((slotdname (c2mop:slot-definition-name slot-def)))
    (if (and (find slotdname +sub-db-forwarded-slots+)
	     (slot-boundp inst 'sub-pool))
	(slot-value (fluid-sub-database inst) slotdname)
	(call-next-method))))

(defmethod (setf c2mop:slot-value-using-class)
    (new-value (class sub-db-forwarding-class) inst slot-def)
  (let ((slotdname (c2mop:slot-definition-name slot-def)))
    (if (and (find slotdname +sub-db-forwarded-slots+)
	     (slot-boundp inst 'sub-pool))
	(setf (slot-value (fluid-sub-database inst) slotdname) new-value)
	(call-next-method))))

(defmethod c2mop:slot-boundp-using-class
    ((class sub-db-forwarding-class) inst slot-def)
  (let ((slotdname (c2mop:slot-definition-name slot-def)))
    (if (and (find slotdname +sub-db-forwarded-slots+)
	     (slot-boundp inst 'sub-pool))
	(slot-boundp (fluid-sub-database inst) slotdname)
	(call-next-method))))

(defmethod c2mop:slot-makunbound-using-class
    ((class sub-db-forwarding-class) inst slot-def)
  (let ((slotdname (c2mop:slot-definition-name slot-def)))
    (if (and (find slotdname +sub-db-forwarded-slots+)
	     (slot-boundp inst 'sub-pool))
	(slot-makunbound (fluid-sub-database inst) slotdname)
	(call-next-method))))

;;; Sugar for `conn-pool's

(defclass fluid-database (database)
  ((database-type :initform :fluid)
   (fluid :initform (make-fluid-database-fluid) :reader fluid-database-fluid)
   (sub-pool :reader sub-pool))
  (:metaclass sub-db-forwarding-class)
  (:documentation "A special kind of database that allocates from a
  pool and forwards database API calls to the thread's backing
  database connection."))

(defparameter *error-on-print-object-p* t
  "Displays database object always, instead of throwing error in case of sql-database-error")

(defmethod print-object ((object fluid-database) stream)
  (if *error-on-print-object-p* 
      (return-from print-object (call-next-method)))

  (let* ((successp t)
         (string (handler-case (with-output-to-string (s)
                                 (call-next-method object s))
                   (sql-database-error (c)
                                       (setf successp nil)
                                       c))))
    (if successp
        (write-string string stream)
        (print-unreadable-object (object stream :type t :identity t)
          (format stream "\"~A\"" string))))
  object)

(defmethod initialize-instance :after
    ((fd fluid-database) &key connection-spec database-type &allow-other-keys)
  (setf (slot-value fd 'sub-pool)
	(find-or-create-connection-pool connection-spec database-type)))

(defun make-fluid-database-fluid ()
  (make-fluid :gc-frequency 5 :gc-function #'release-to-pool))

(defun fluid-sub-database (fd)
  "Answer the fluid value for `fluid-database'."
  (let ((fluid (fluid-database-fluid fd)))
    (or (fluid-value fluid)
	(setf (fluid-value fluid)
	      (acquire-from-pool nil nil (sub-pool fd))))))

(defmacro define-fluid-forward (methname meth-arglist)
  "Define a method with only required and keyword arguments on
METHNAME for `fluid-database'.  METH-ARGLIST should have T in
positions specialized on `fluid-database', and NIL in other places."
  (let (argrefs arglist keyrefs
	(req-arglist (loop for elt in meth-arglist
			   until (member elt lambda-list-keywords)
			   collect elt))
	(key-arglist (member '&key meth-arglist)))
    (mapc (lambda (fd?)
	    (let ((gensym (gensym)))
	      (cond (fd?
		     (push `(fluid-sub-database ,gensym) argrefs)
		     (push `(,gensym fluid-database) arglist))
		    (t
		     (push gensym argrefs)
		     (push gensym arglist)))))
	  req-arglist)
    (setf keyrefs (mapcan (lambda (kwarg)
			    (list (intern (symbol-name kwarg) 'keyword) kwarg))
			  (cdr key-arglist)))
    (setf argrefs (nreverse argrefs)
	  arglist (nreverse arglist))
    `(defmethod ,methname (,@arglist ,@key-arglist)
       ,(etypecase methname
	  (symbol `(,methname ,@argrefs ,@keyrefs))
	  ((cons (eql setf) (cons symbol null))
	     `(setf (,(second methname) ,@(cdr argrefs) ,@keyrefs)
		    ,(car argrefs)))))))

(define-fluid-forward database-type (t))
(define-fluid-forward database-query (nil t nil nil))
(define-fluid-forward database-execute-command (nil t))
(define-fluid-forward database-query-result-set (nil t &key full-set result-types))
(define-fluid-forward database-dump-result-set (nil t))
(define-fluid-forward database-store-next-row (nil t nil))
(define-fluid-forward database-truncate (t))
(define-fluid-forward database-create-sequence (nil t))
(define-fluid-forward database-drop-sequence (nil t))
(define-fluid-forward database-sequence-next (nil t))
(define-fluid-forward database-list-sequences (t &key owner))
(define-fluid-forward database-set-sequence-position (nil nil t))
(define-fluid-forward database-sequence-last (nil t))
(define-fluid-forward database-start-transaction (t))
(define-fluid-forward database-commit-transaction (t))
(define-fluid-forward database-abort-transaction (t))
(define-fluid-forward database-list-tables (t &key owner))
(define-fluid-forward database-list-tables-and-sequences (t &key owner))
(define-fluid-forward database-list-views (t &key owner))
(define-fluid-forward database-list-indexes (t &key owner))
(define-fluid-forward database-list-table-indexes (nil t &key owner))
(define-fluid-forward database-list-attributes (nil t &key owner))
(define-fluid-forward database-attribute-type (nil nil t &key owner))
(define-fluid-forward database-add-attribute (nil nil t))
(define-fluid-forward database-rename-attribute (nil nil nil t))
(define-fluid-forward database-underlying-type (t))
(define-fluid-forward database-create-large-object (t))
(define-fluid-forward database-write-large-object (nil nil t))
(define-fluid-forward database-read-large-object (nil t))
(define-fluid-forward database-delete-large-object (nil t))
(define-fluid-forward database-prepare (nil nil t nil nil))

;;; Functions requiring special attention

(defmethod database-type-library-loaded ((db-type (eql :fluid)))
  t)

(defmethod database-type-load-foreign ((db-type (eql :fluid)))
  t)

(defmethod database-initialize-database-type ((db-type (eql :fluid)))
  t)

(defmethod database-disconnect ((db fluid-database))
  (setf (slot-value db 'fluid) (make-fluid-database-fluid))
  (clear-conn-pool (sub-pool db)))

(defmethod database-reconnect ((db fluid-database))
  (loop for subdb across (all-connections (sub-pool db))
	do (database-reconnect subdb)))

;; Things I don't think are needed, even though they have a db-type or
;; database parameter:
;;
;;  * database-name-from-spec
;;  * database-connect
;;  * database-create
;;  * database-destroy
;;  * database-probe
;;  * database-list
;;  * oid
;;  * db-type-*, specifically because of underlying-type
;;
;; If a DB interface function with a db-type or database parameter is
;; not mentioned above, it is a bug.
(export 'fluid-database)
(export '*error-on-print-object-p*)
(import 'fluid-database '#:clsql)
(export 'fluid-database '#:clsql)
