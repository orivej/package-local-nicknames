(defpackage #:package-local-nicknames
  (:use :cl)
  (:export #:find-global-package
           #:global-package-nicknames
           #:*packages-with-local-nicknames*
           #:package-local-nicknames
           #:set-package-local-nicknames
           #:find-package-using-package
           #:package-nicknames-using-package))
