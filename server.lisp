(in-package #:http-dohc)

(defun start-server-iolib (port &key (number-threads 50))
  (let ((leader-lock (bordeaux-threads:make-lock))
        (server-socket (iolib:listen-on
                        (iolib:make-socket :connect :passive
                                           :ipv6 nil
                                           :local-host iolib:+ipv4-unspecified+
                                           :local-port port))))
    (loop repeat number-threads do
         (bordeaux-threads:make-thread
          (lambda ()
            (declare (optimize speed))
            (let ((read-buffer (make-rbuf))
                  client-connection)
              (loop
                 (bordeaux-threads:with-lock-held (leader-lock)
                   (setf client-connection (iolib:accept-connection server-socket)))
                 (ignore-errors
                   (unwind-protect
                        (handle-request-iolib client-connection read-buffer)
                     (clear read-buffer)
                     (close (iolib:shutdown client-connection :read t :write t)))))))))))

(defvar *request*)

(defun handle-request-iolib (client-connection read-buffer)
  (declare (optimize speed))
  (let ((*request* (make-request :client-connection client-connection
                                 :read-buffer read-buffer)))
    (catch 'socket-error
      (loop while (io.multiplex:wait-until-fd-ready (iolib:socket-os-fd client-connection) :input 5)
         do (multiple-value-bind (method uri)
                (read-request-line client-connection read-buffer)
              (read-headers client-connection read-buffer))
           (iolib:send-to
            client-connection
            #.(apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                     (mapcar #'cl-irregsexp.bytestrings:force-simple-byte-vector
                             (list
                              "HTTP/1.1 200 OK" +crlf+
                              "Date: Fri, 31 Dec 1999 23:59:59 GMT" +crlf+
                              "Content-Type: text/html; charset=utf8" +crlf+
                              "Connection: Keep-Alive" +crlf+
                              "Content-Length: 20" +crlf+ +crlf+
                              "<h1>HARRO HARRO</h1>" +crlf+))))
           (ensure-buffers-flushed *request*)))))

(defstruct request
  client-connection
  read-buffer
  method
  uri
  headers
  body)

(defun ensure-buffers-flushed (request)
  (declare (ignore request))
  ;; (read-headers request)
;;   (unless (request-body request)
;;     ;; read and discard content-length bytes from client-connection
;;     )
  )

(defun read-request-line (client-connection read-buffer)
  (let ((rline (read-to-newline client-connection read-buffer nil)))
    (values
     (if (and (= 80 (aref rline 0))
              (= 79 (aref rline 1))
              (= 83 (aref rline 2))
              (= 84 (aref rline 3)))
         :post
         :get) ;; yes, I went there
     (babel:octets-to-string rline
                             :start (1+ (position 32 rline))
                             :end (position 32 rline :from-end t)))))

(defun read-headers (client-connection read-buffer)
  (read-to-newline client-connection read-buffer t)
  ;; (loop for line = (read-to-delimiter client-connection read-buffer +lf+)
;;        until (< (length line) 2)
;;        collect line)
  )

;; (defun read-body (request)
;;   )

