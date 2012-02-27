(in-package #:sb-impl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '#:package-local-nicknames))

;;; can't redefine %defpackage, since existing defpackage forms would
;;; have already expanded to calls with the original signature
(defun %defpackage-pln (name nicknames size shadows shadowing-imports
                        use imports interns exports implement lock
                        local-nicknames doc-string
                        source-location)
  (declare (type simple-string name)
           (type list nicknames shadows shadowing-imports
                 imports interns exports local-nicknames)
           (type (or list (member :default)) use)
           (type (or simple-string null) doc-string)
           ;;#-package-local-nicknames
           ;;(ignore local-nicknames)
           #-sb-package-locks
           (ignore implement lock))
  (with-package-graph ()
    (let* ((existing-package (find-package name))
           (use (use-list-packages existing-package use))
           (shadowing-imports (import-list-symbols shadowing-imports))
           (imports (import-list-symbols imports)))
      (if existing-package
          (prog1
            (update-package-with-variance existing-package name
                                          nicknames source-location
                                          shadows shadowing-imports
                                          use imports interns exports
                                          implement lock doc-string)
            (let ((old-local-nicknames (package-local-nicknames existing-package)))
              (setf old-local-nicknames (set-difference old-local-nicknames local-nicknames :test #'equal))
              (when old-local-nicknames
                (warn 'package-at-variance
                      :format-control "~A also has the following local nicknames: ~% ~{ ~S -> ~S ~}"
                      :format-arguments (cons name old-local-nicknames)))
              (set-package-local-nicknames existing-package local-nicknames)))
          (let ((package (make-package name
                                       :use nil
                                       :internal-symbols (or size 10)
                                       :external-symbols (length exports))))
            (prog1
                (update-package package
                                nicknames source-location
                                shadows shadowing-imports
                                use imports interns exports
                                implement lock doc-string)
              (set-package-local-nicknames package local-nicknames)))))))

;;; trying to redefine find-package tends to break things, so define it
;;; with another name and (setf fdefinition) later
(defun find-package-pln (package-designator)
  (or (and (boundp '*package*)
           (find-package-using-package package-designator *package*))
      (find-global-package package-designator)))


(without-package-locks
  (defmacro defpackage (package &rest options)
    #+sb-doc
    #.(format nil
    "Defines a new package called PACKAGE. Each of OPTIONS should be one of the
    following: ~{~&~4T~A~}
    All options except ~{~A, ~}and :DOCUMENTATION can be used multiple
    times."
              '((:nicknames "{package-name}*")
                (:size "<integer>")
                (:shadow "{symbol-name}*")
                (:shadowing-import-from "<package-name> {symbol-name}*")
                (:use "{package-name}*")
                (:import-from "<package-name> {symbol-name}*")
                (:intern "{symbol-name}*")
                (:export "{symbol-name}*")
                #+sb-package-locks (:implement "{package-name}*")
                #+sb-package-locks (:lock "boolean")
                ;;#!+package-local-nicknames
                (:local-nicknames "(<nickname> <real-name>)*)")
                (:documentation "doc-string"))
              '(:size #+sb-package-locks :lock))
    (let ((nicknames nil)
          (size nil)
          (shadows nil)
          (shadowing-imports nil)
          (use nil)
          (use-p nil)
          (imports nil)
          (interns nil)
          (exports nil)
          (implement (stringify-package-designators (list package)))
          (implement-p nil)
          (lock nil)
          (local-nicknames nil)
          (doc nil))
      #-sb-package-locks
      (declare (ignore implement-p))
      (dolist (option options)
        (unless (consp option)
          (error 'simple-program-error
                 :format-control "bogus DEFPACKAGE option: ~S"
                 :format-arguments (list option)))
        (case (car option)
          (:nicknames
           (setf nicknames (stringify-package-designators (cdr option))))
          (:size
           (cond (size
                  (error 'simple-program-error
                         :format-control "can't specify :SIZE twice."))
                 ((and (consp (cdr option))
                       (typep (second option) 'unsigned-byte))
                  (setf size (second option)))
                 (t
                  (error
                   'simple-program-error
                   :format-control ":SIZE is not a positive integer: ~S"
                   :format-arguments (list (second option))))))
          (:shadow
           (let ((new (stringify-string-designators (cdr option))))
             (setf shadows (append shadows new))))
          (:shadowing-import-from
           (let ((package-name (stringify-package-designator (second option)))
                 (names (stringify-string-designators (cddr option))))
             (let ((assoc (assoc package-name shadowing-imports
                                 :test #'string=)))
               (if assoc
                   (setf (cdr assoc) (append (cdr assoc) names))
                   (setf shadowing-imports
                         (acons package-name names shadowing-imports))))))
          (:use
           (setf use (append use (stringify-package-designators (cdr option)) )
                 use-p t))
          (:import-from
           (let ((package-name (stringify-package-designator (second option)))
                 (names (stringify-string-designators (cddr option))))
             (let ((assoc (assoc package-name imports
                                 :test #'string=)))
               (if assoc
                   (setf (cdr assoc) (append (cdr assoc) names))
                   (setf imports (acons package-name names imports))))))
          (:intern
           (let ((new (stringify-string-designators (cdr option))))
             (setf interns (append interns new))))
          (:export
           (let ((new (stringify-string-designators (cdr option))))
             (setf exports (append exports new))))
          #+sb-package-locks
          (:implement
           (unless implement-p
             (setf implement nil))
           (let ((new (stringify-package-designators (cdr option))))
             (setf implement (append implement new)
                   implement-p t)))
          #+sb-package-locks
          (:lock
           (when lock
             (error 'simple-program-error
                    :format-control "multiple :LOCK options"))
           (setf lock (coerce (second option) 'boolean)))
          ;;#!+package-local-nicknames
          (:local-nicknames
           (let ((new (mapcar #'stringify-package-designators
                              (cdr option))))
             ;; fixme: expand list if we allow multiple nicknames
             ;; in one clause? (ex: (:real-name :foo :bar) ->
             ;; (:real-name :foo) (:real-name :bar)
             (setf local-nicknames (append local-nicknames new))))
          (:documentation
           (when doc
             (error 'simple-program-error
                    :format-control "multiple :DOCUMENTATION options"))
           (setf doc (coerce (second option) 'simple-string)))
          (t
           (error 'simple-program-error
                  :format-control "bogus DEFPACKAGE option: ~S"
                  :format-arguments (list option)))))
      (check-disjoint `(:intern ,@interns) `(:export  ,@exports))
      (check-disjoint `(:intern ,@interns)
                      `(:import-from
                        ,@(apply #'append (mapcar #'rest imports)))
                      `(:shadow ,@shadows)
                      `(:shadowing-import-from
                        ,@(apply #'append (mapcar #'rest shadowing-imports))))
      (if local-nicknames
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (%defpackage-pln ,(stringify-string-designator package)
                              ',nicknames ',size ',shadows ',shadowing-imports
                              ',(if use-p use :default) ',imports ',interns
                              ',exports ',implement ',lock
                              ',local-nicknames ',doc
                              (sb-c:source-location)))
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (%defpackage ,(stringify-string-designator package) ',nicknames
                          ',size ',shadows ',shadowing-imports
                          ',(if use-p use :default) ',imports ',interns
                          ',exports ',implement ',lock ',doc
                          (sb-c:source-location)))))))

(without-package-locks
 (setf (fdefinition 'find-package)
       #'find-package-pln))

(push :package-local-nicknames *features*)
