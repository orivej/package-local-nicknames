(defpackage #:foo
  (:use :cl)
  (:export #:foo)
  (:local-nicknames (:hoge :cl)
                    (:bar :cl-user)))

(in-package #:foo)

(defun foo () (format t "foo:foo!~%"))

(defparameter cl-user::*foo* 1)

(defparameter bar::*piyo* (hoge:* bar::*foo* 2))


(in-package #:cl)
(defpackage #:bar
  (:use :common-lisp)
  (:local-nicknames (:cl :foo)))

(in-package #:bar)
(cl:foo)