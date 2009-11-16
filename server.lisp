(in-package #:http-dohc)

(defvar *default-connection-timeout* 5 "seconds")

(defun start-server (port &key (number-threads 50))
  (let ((server-socket (iolib:fd-of
                        (iolib:listen-on
                         (iolib:make-socket
                          :connect :passive
                          :ipv6 nil
                          :local-host iolib:+ipv4-unspecified+
                          :local-port port))))
        (accept-leader-lock (make-lock))
        (mux-leader-lock (make-lock))
        (waiting-fds-lock (make-lock))
        (waiting-fds (make-hash-table))
        (io-tasks ())
        (mux (make-instance iomux:*default-multiplexer*))
        (last-cull-time (get-universal-time)))
    (declare (ignorable accept-leader-lock))
    (flet ((close-epoll-connection (fd)
             (iomux:unmonitor-fd mux fd)
             (ignore-errors (isys:%sys-close fd))))
      (loop repeat number-threads do
           (make-thread ;; accept() thread pool
            (lambda ()
              (declare (optimize speed)
                       (ftype (function (t t t) fixnum) sockets::%accept)
                       (inline sockets::%accept))
              (let ((read-buffer (make-rbuf))
                    fd)
                (loop ;; accept() is thread-safe on Linux
                   #-linux (acquire-lock accept-leader-lock)
                   (setf fd (sockets::%accept server-socket
                                              (cffi:null-pointer)
                                              (cffi:null-pointer)))
                   #-linux (release-lock accept-leader-lock)
                   (ignore-errors
                     (if (handle-request fd read-buffer)
                         (progn
                           (with-lock-held (waiting-fds-lock)
                             (setf (gethash fd waiting-fds)
                                   (cons (get-universal-time) read-buffer)))
                           (iomux:monitor-fd mux fd '(:read :epoll-oneshot))
                           (setf read-buffer (make-read-buffer)))
                         (progn (isys:%sys-close fd)
                                (clear read-buffer))))))))
           (make-thread ;; epoll thread pool
            (lambda ()
              (let (my-task
                    fd-buffer)
                (loop (with-lock-held (mux-leader-lock)
                        ;; this assumes thread safety in adding events
                        ;; to the mux (true for epoll)
                        (setf my-task
                              (progn
                                (unless io-tasks
                                  (setf io-tasks
                                        (iomux:harvest-events
                                         mux *default-connection-timeout*)))
                                (pop io-tasks)))
                        (with-lock-held (waiting-fds-lock)
                          (when (< (+ last-cull-time
                                      *default-connection-timeout*)
                                   (get-universal-time))
                            (loop for fd being the hash-key of waiting-fds
                               using (hash-value state) do
                               (when (< (+ (car state)
                                           *default-connection-timeout*)
                                        (get-universal-time))
                                 (close-epoll-connection fd)
                                 (remhash fd waiting-fds)
                                 (when (eql fd (car my-task))
                                   (setf my-task nil))))
                            (setf last-cull-time (get-universal-time)))
                          (setf fd-buffer
                                (cdr (gethash (car my-task) waiting-fds)))
                          (remhash (car my-task) waiting-fds))
                        ;; TODO: if we're not using epoll (ONESHOT),
                        ;; need to turn off events on this fd
                        )
                   (when my-task
                     (if (member :error (cdr my-task))
                         (close-epoll-connection (car my-task))
                         (ignore-errors
                           (unwind-protect
                                (handle-request (car my-task) fd-buffer)
                             (with-lock-held (waiting-fds-lock)
                               (setf (gethash (car my-task) waiting-fds)
                                     (cons (get-universal-time) fd-buffer)))
                             (iomux:update-fd mux (car my-task)
                                              '(:read :epoll-oneshot) nil)
                             ))))))))))))

(defvar *request*)
(defvar *pages* ())

(declaim (inline dispatch))
(defun dispatch (uri-path)
  (declare (optimize speed))
  (aif (assoc uri-path *pages* :test #'equalp)
       (let ((handler (cdr it)))
         (declare (type function handler))
         (funcall handler))))

(defun handle-request (fd read-buffer)
  (declare (optimize speed))
  (let ((*request* (make-request :fd fd
                                 :read-buffer read-buffer)))
    (match-bind
        (progn method (+ #\Space) uri (+ #\Space) "HTTP/" http-version (? #\Return))
        (read-to-newline fd read-buffer nil)
      (setf (request-method *request*) method)
      (read-headers fd read-buffer)
      (match-bind (path (or (last) (progn "?" query-string)))
          uri
        (setf (request-path *request*) path
              (request-query-string *request*) query-string)
        (let ((response (dispatch path)))
          (declare (inline isys:%sys-write)
                   (ftype (function (t t t) fixnum) isys:%sys-write))
          (cffi:with-pointer-to-vector-data (ptr response)
            (isys:%sys-write fd ptr (length response)))))
      (unless (equalp http-version #.(force-simple-byte-vector "1.0"))
        (ensure-buffers-flushed *request*)
        t))))

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
  fd
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
                      '(push (cons (url-encoding-decode name)
                                   (url-encoding-decode value))
                        params)))
          (request-query-string request))
      (setf (request-%query-params request) params)))
  (cl-irregsexp.utils:alist-get (request-%query-params request) param-name
                                :test #'byte-vector=))

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

