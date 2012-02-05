(defsystem :package-local-nicknames
  :serial t
  :components ((:file "package")
               #+sbcl(:file "sbcl")
               #+ccl(:file "ccl")
               (:file "package-local-nicknames")))