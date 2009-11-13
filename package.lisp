(cl:defpackage #:http-dohc
  (:use #:cl #:anaphora)
  (:import-from #:cl-irregsexp.bytestrings #:simple-byte-vector #:force-string #:force-simple-byte-vector)
  (:import-from #:cl-irregsexp #:match-bind #:match-replace-all))
