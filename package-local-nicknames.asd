(defsystem :package-local-nicknames
  :serial t
  :components ((:file "package")
               (:file "common")
               #+sbcl
               (:file "sbcl")
               #+ccl
               (:file "ccl")))
