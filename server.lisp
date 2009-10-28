(in-package #:http-dohc)

(defun start-server (port &key (number-threads 50))
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
                        (handle-request client-connection read-buffer)
                     (clear read-buffer)
                     (close (iolib:shutdown client-connection :read t :write t)))))))))))

(defvar *request*)
(defvar *pages* ())

(declaim (inline dispatch))
(defun dispatch (uri-path)
  (declare (optimize speed))
  (aif (assoc uri-path *pages* :test #'equalp)
       (let ((handler (cdr it)))
         (declare (type function handler))
         (funcall handler))))

(defun handle-request (client-connection read-buffer)
  (declare (optimize speed))
  (let ((*request* (make-request :client-connection client-connection
                                 :read-buffer read-buffer))
        (keep-alive? t))
    (catch 'socket-error
      (loop while (and keep-alive?
                       (io.multiplex:wait-until-fd-ready (iolib:socket-os-fd client-connection)
                                                         :input 5))
         do (match-bind
                (progn method (+ #\Space) uri (+ #\Space) "HTTP/" http-version (? #\Return))
                (read-to-newline client-connection read-buffer nil)
              (setf (request-method *request*) method)
              (read-headers client-connection read-buffer)
              (match-bind (path (or (last) (progn "?" query-string)))
                  uri
                (setf (request-path *request*) path
                      (request-query-string *request*) query-string)
                (iolib:send-to client-connection (dispatch path)))
              (if (equalp http-version #.(force-simple-byte-vector "1.0"))
                  (setf keep-alive? nil)
                  (ensure-buffers-flushed *request*)))))))

(defmacro defpage (uri-path &body body)
  `(push (cons ,(babel:string-to-octets uri-path) (lambda () ,@body)) *pages*))

(defpage "/test"
    (concatenate
     'simple-byte-vector
     #.(apply #'concatenate 'simple-byte-vector
              (mapcar #'force-simple-byte-vector
                      (list
                       "HTTP/1.1 200 OK" +crlf+
                       "Date: Fri, 31 Dec 1999 23:59:59 GMT" +crlf+
                       "Content-Type: text/html; charset=utf8" +crlf+
                       "Connection: Keep-Alive" +crlf+
                       "Content-Length: 20" +crlf+ +crlf+ ;; FIXME
                       "<h1>Hello ")))
     (get-query-parameter #.(force-simple-byte-vector "name") *request*)
     #.(force-simple-byte-vector "</h1>")))

(defstruct request
  client-connection
  read-buffer
  method
  path
  query-string
  %query-params
  headers
  body)

(defun get-query-parameter (param-name request)
  (declare (optimize speed))
  (unless (request-%query-params request)
    (let ((params ()))
      (match-bind ((* name "=" value (or (last) "&")
                      '(push (cons (tpd2.http::url-encoding-decode name) (tpd2.http::url-encoding-decode value)) params)))
          (request-query-string request))
      (setf (request-%query-params request) params)))
  (cl-irregsexp.utils:alist-get (request-%query-params request) param-name :test #'teepeedee2.lib:byte-vector=-fold-ascii-case))

(defun ensure-buffers-flushed (request)
  (declare (ignore request))
  ;; (read-headers request)
;;   (unless (request-body request)
;;     ;; read and discard content-length bytes from client-connection
;;     )
  )

(defun read-headers (client-connection read-buffer)
  (read-to-newline client-connection read-buffer t))

;; (defun read-body (request)
;;   )

