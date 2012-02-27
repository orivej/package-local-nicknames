;;; Common, more or less

(in-package #:package-local-nicknames)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *global-alias* nil)
  (unless *global-alias*
    (setf (fdefinition 'find-global-package)
          (fdefinition 'find-package))
    (setf (fdefinition 'global-package-nicknames)
          (fdefinition 'package-nicknames))
    (setf *global-alias* t)))

;;; since we can't modify the PACKAGE struct, we store the aliases externally
;;; in a weak key hash table from package objects ->
;;;  hash of nickname -> real-name
(defvar *packages-with-local-nicknames*
  #+sbcl
  (make-hash-table :weakness :key :test 'eq)
  #+ccl
  (make-hash-table :weak :key :test 'eq))

(defun package-local-nicknames (package-designator)
  (let* ((package (find-package package-designator))
         (local-nicknames (gethash package *packages-with-local-nicknames*))
         (list nil))
    (when local-nicknames
      (maphash (lambda (k v) (push (list k v) list)) local-nicknames))
    list))

(defun set-package-local-nicknames (package-designator nicknames)
  (let* ((package (find-package package-designator))
         (ht (make-hash-table :test 'equal)))
    (assert package nil "no package for designator ~s in set-package-local-nicknames" package-designator)
    (loop for (n rn) in nicknames
       do (setf (gethash n ht) rn))
    (setf (gethash package *packages-with-local-nicknames*) ht)))

;; todo: real API for this...
(defun find-package-using-package (name package-designator &key (errorp t))
  (when (packagep name)
    (return-from find-package-using-package name))
  (check-type name (or symbol string character) "package-designator")
  (let* ((package (if (packagep package-designator)
                      package-designator
                      (find-package package-designator)))
         (local-nicknames (gethash package *packages-with-local-nicknames*))
         (real-name (when local-nicknames (gethash (string name)
                                                   local-nicknames)))
         (real-package (when real-name (find-global-package real-name))))
    ;; should not finding a package be an error?
    (when (and real-name (not real-package) errorp)
      ;; todo: real error
      (error "package nickname ~s in package ~s points to non-existant package ~s" name (package-name package) real-name))
    ;; return t/nil in 2nd value to indicate whether a nickname was defined
    ;; in case we didn't signal an error
    (values real-package (and real-name t))))

(defun package-nicknames-using-package (name local)
  (let* ((name (find-package name))
         (local (find-package local))
         (local-nicknames (gethash local *packages-with-local-nicknames*))
         (nicknames (global-package-nicknames name)))
    (when local-nicknames
      (maphash (lambda (k v)
                 (when (eq (find-package v) name)
                   (push k nicknames)))
               local-nicknames))
    nicknames))
