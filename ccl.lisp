(in-package #:ccl)

(use-package '#:package-local-nicknames)

(defun find-global-package (name &optional (len (length name)))
  (declare (fixnum len))
  (with-package-list-read-lock
      (dolist (p %all-packages%)
        (if (dolist (pkgname (pkg.names p))
              (when (and (= (the fixnum (length pkgname)) len)
                         (dotimes (i len t)
                           ;; Aref: allow non-simple strings
                           (unless (eq (aref name i) (schar pkgname i))
                             (return))))
                (return t)))
          (return p)))))


(defun %define-package-pln (name size 
                             external-size ; extension (may be nil.)
                             nicknames
                             shadow
                             shadowing-import-from-specs
                             use
                             import-from-specs
                             intern
                             export
                            local-nicknames
			     &optional doc)
  (if (eq use :default) (setq use *make-package-use-defaults*))
  (let* ((pkg (find-package name)))
    (if pkg
      ;; Restarts could offer several ways of fixing this.
      (unless (string= (package-name pkg) name)
        (cerror "Redefine ~*~S"
                "~S is already a nickname for ~S" name pkg))
      (setq pkg (make-package name
                              :use nil
                              :internal-size (or size 60)
                              :external-size (or external-size
                                                 (max (length export) 1)))))
    (unuse-package (package-use-list pkg) pkg)
    (rename-package pkg name nicknames)
    (set-package-local-nicknames pkg local-nicknames)
    (flet ((operation-on-all-specs (function speclist)
             (let ((to-do nil))
               (dolist (spec speclist)
                 (let ((from (pop spec)))
                   (dolist (str spec)
                     (multiple-value-bind (sym win) (find-symbol str from)
                       (if win
                         (push sym to-do)
                         ; This should (maybe) be a PACKAGE-ERROR.
                         (cerror "Ignore attempt to ~s ~s from package ~s"
                                 "Cannot ~s ~s from package ~s" function str from))))))
               (when to-do (funcall function to-do pkg)))))
      
      (dolist (sym shadow) (shadow sym pkg))
      (operation-on-all-specs 'shadowing-import shadowing-import-from-specs)
      (use-package use pkg)
      (operation-on-all-specs 'import import-from-specs)
      (dolist (str intern) (intern str pkg))
      (when export
        (let* ((syms nil))
          (dolist (str export)
            (multiple-value-bind (sym found) (find-symbol str pkg)
              (unless found (setq sym (intern str pkg)))
              (push sym syms)))
          (export syms pkg)))
      (when (and doc *save-doc-strings*)
        (set-documentation pkg t doc))
      pkg)))

(handler-bind ((simple-error (lambda (c)
                               (when (find-restart 'ccl::never-complain)
                                 (continue c)))))
  (defun %find-pkg (name &optional (len (length name)))
    (or (and (boundp '*package*)
             (find-package-using-package name *package*))
        (find-global-package name len)))

  (defmacro defpackage (name &rest options)
    "Defines a new package called PACKAGE. Each of OPTIONS should be one of the 
   following: 
    (NICKNAMES {package-name}*)

    (SIZE <integer>)
    (SHADOW {symbol-name}*)
    (SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
    (USE {package-name}*)
    (IMPORT-FROM <package-name> {symbol-name}*)
    (INTERN {symbol-name}*)
    (EXPORT {symbol-name}*)
    (LOCAL-NICKNAMES (nickname real-name)*)
    (IMPLEMENT {package-name}*)
    (LOCK boolean)
    (DOCUMENTATION doc-string)
   All options except SIZE, LOCK, and :DOCUMENTATION can be used multiple 
   times."
    (let* ((size nil)
           (all-names-size 0)
           (intern-export-size 0)
           (shadow-etc-size 0)
           (documentation nil)
           (all-names-hash (let ((all-options-alist nil))
                             (dolist (option options)
                               (let ((option-name (car option)))
                                 (when (memq option-name
                                             '(:nicknames :local-nicknames :shadow :shadowing-import-from
                                               :use :import-from :intern :export))
                                   (let ((option-size (length (cdr option)))
                                         (cell (assq option-name all-options-alist)))
                                     (declare (fixnum option-size))
                                     (if cell
                                         (incf (cdr cell) option-size)
                                         (push (cons option-name option-size) all-options-alist))
                                     (when (memq option-name '(:shadow :shadowing-import-from :import-from :intern))
                                       (incf shadow-etc-size option-size))
                                     (when (memq option-name '(:export :intern))
                                       (incf intern-export-size option-size))))))
                             (dolist (cell all-options-alist)
                               (let ((option-size (cdr cell)))
                                 (when (> option-size all-names-size)
                                   (setq all-names-size option-size))))
                             (when (> all-names-size 0)
                               (make-hash-table :test 'equal :size all-names-size))))
           (intern-export-hash (when (> intern-export-size 0)
                                 (make-hash-table :test 'equal :size intern-export-size)))
           (shadow-etc-hash (when (> shadow-etc-size 0)
                              (make-hash-table :test 'equal :size shadow-etc-size)))
           (external-size nil)
           (nicknames nil)
           (local-nicknames nil)
           (shadow nil)
           (shadowing-import-from-specs nil)
           (use :default)
           (import-from-specs nil)
           (intern nil)
           (export nil))
      (declare (fixnum all-names-size intern-export-size shadow-etc-size))
      (labels ((string-or-name (s) (string s))
               (duplicate-option (o)
                 (signal-program-error "Duplicate ~S option in ~S ." o options))
               (duplicate-name (name option-name)
                 (signal-program-error "Name ~s, used in ~s option, is already used in a conflicting option ." name option-name))
               (all-names (option-name tail already)
                 (when (eq already :default) (setq already nil))
                 (when all-names-hash
                   (clrhash all-names-hash))
                 (dolist (name already)
                   (setf (gethash (string-or-name name) all-names-hash) t))
                 (dolist (name tail already)
                   (setq name (string-or-name name))
                   (unless (gethash name all-names-hash) ; Ok to repeat name in same option.
                     (when (memq option-name '(:shadow :shadowing-import-from :import-from :intern))
                       (if (gethash name shadow-etc-hash)
                           (duplicate-name name option-name))
                       (setf (gethash name shadow-etc-hash) t))
                     (when (memq option-name '(:export :intern))
                       (if (gethash name intern-export-hash)
                           (duplicate-name name option-name))
                       (setf (gethash name intern-export-hash) t))
                     (setf (gethash name all-names-hash) t)
                     (push name already)))))
        (dolist (option options)
          (let ((args (cdr option)))
            (ecase (%car option)
              (:size 
               (if size 
                   (duplicate-option :size) 
                   (setq size (car args))))		 
              (:external-size 
               (if external-size 
                   (duplicate-option :external-size) 
                   (setq external-size (car args))))
              (:nicknames (setq nicknames (all-names nil args nicknames)))
              ;; TODO
              (:local-nicknames (setq local-nicknames (append local-nicknames
                                                              (mapcar (lambda (args)
                                                                        (reverse (all-names nil args local-nicknames)))
                                                                      args))))
              (:shadow (setq shadow (all-names :shadow args shadow)))
              (:shadowing-import-from
               (destructuring-bind (from &rest shadowing-imports) args
                 (push (cons (string-or-name from)
                             (all-names :shadowing-import-from shadowing-imports nil))
                       shadowing-import-from-specs)))
              (:use (setq use (all-names nil args use)))
              (:import-from
               (destructuring-bind (from &rest imports) args
                 (push (cons (string-or-name from)
                             (all-names :import-from imports nil))
                       import-from-specs)))
              (:intern (setq intern (all-names :intern args intern)))
              (:export (setq export (all-names :export args export)))
              (:documentation
               (if documentation
                   (duplicate-option :documentation)
                   (setq documentation (cadr option)))))))
        (if local-nicknames
            `(eval-when (:execute :compile-toplevel :load-toplevel)
               (%define-package-pln ',(string-or-name name)
                                    ',size 
                                    ',external-size 
                                    ',nicknames
                                    ',shadow
                                    ',shadowing-import-from-specs
                                    ',use
                                    ',import-from-specs
                                    ',intern
                                    ',export
                                    ',local-nicknames
                                    ',documentation))
            `(eval-when (:execute :compile-toplevel :load-toplevel)
               (%define-package ',(string-or-name name)
                                ',size 
                                ',external-size 
                                ',nicknames
                                ',shadow
                                ',shadowing-import-from-specs
                                ',use
                                ',import-from-specs
                                ',intern
                                ',export
                                ',documentation))))))
  ;; TODO - adapt the package-ref stuff to play nice with local nicknames. For now, just gonna blow
  ;; away that optimisation.
  (define-compiler-macro find-package (&whole w package &environment env)
    (declare (ignore package env))
    w
    #+nil(let* ((ref (package-ref-form package env)))
      (if ref
          `(package-ref.pkg ,ref)
          w)))
)

(pushnew :package-local-nicknames *features*)
