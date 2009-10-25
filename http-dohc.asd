;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :http-dohc
  :serial t
  :components ((:file "package")
               (:file "read-buffer")
               (:file "server"))
  :depends-on (:iolib :bordeaux-threads :cl-irregsexp :anaphora :babel))
