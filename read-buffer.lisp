(in-package :http-dohc)

;; some of this code is borrowed from TPD2, Lisp LGPL licensed

(defvar +crlf+ (force-simple-byte-vector #(13 10)))
(defconstant +lf+ 10)
(defconstant +cr+ 13)

(defvar *read-buffer-size* 1024)

(defvar *read-buffer-pool* ())
(defvar *read-buffer-pool-lock* (make-lock))

(defun make-read-buffer ()
  (aif (with-lock-held (*read-buffer-pool-lock*)
         (pop *read-buffer-pool*))
       it
       (make-rbuf)))

(defun free-read-buffer (buf)
  (clear buf)
  (with-lock-held (*read-buffer-pool-lock*)
    (push buf *read-buffer-pool*)))

(defstruct rbuf
  (buffer (make-array *read-buffer-size* :element-type '(unsigned-byte 8)))
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defun find-newline (seq start end two-consecutive-newlines?)
  (declare (optimize speed)
           (type simple-byte-vector seq)
           (type fixnum start end))
  (if two-consecutive-newlines?
      (loop for i of-type fixnum from start below end do
           (when (and (= +lf+ (aref seq i))
                      (or (= +lf+ (aref seq (incf i)))
                          (and (= +cr+ (aref seq i))
                               (= +lf+ (aref seq (incf i))))))
             (return i)))
      (position +lf+ seq :start start :end end)))

(declaim (inline clear))
(defun clear (buffer)
  (declare (optimize speed))
  (setf (rbuf-start buffer) 0
        (rbuf-end buffer) 0))

(defun sync (buffer)
  (declare (optimize speed))
  (when (= (rbuf-start buffer) (rbuf-end buffer))
    (clear buffer)))

(defun prepare-read (buf &optional (size *read-buffer-size*))
  (declare (optimize speed)
           (type fixnum size))
  (let ((old-buffer (rbuf-buffer buf)))
    (declare (type simple-byte-vector old-buffer))
    (when (> size (- (length old-buffer) (rbuf-start buf)))
      (if (= (rbuf-start buf) (rbuf-end buf))
          (progn (when (> size (length old-buffer))
                   (setf (rbuf-buffer buf) (make-array size :element-type '(unsigned-byte 8))))
                 (clear buf))
          ;; Unfortunately cannot use adjust-array as that might make non "simple" arrays
          (let ((new-buf (make-array (max (length old-buffer) size) :element-type '(unsigned-byte 8))))
            (replace new-buf old-buffer :start2 (rbuf-start buf) :end2 (rbuf-end buf))
            (decf (rbuf-end buf) (rbuf-start buf))
            (setf (rbuf-start buf) 0)
            (setf (rbuf-buffer buf) new-buf))))))

(defun eat-to-newline (buf two-newlines?)
  (declare (optimize speed))
  (let ((buffer (rbuf-buffer buf)))
    (declare (type simple-byte-vector buffer))
    (awhen (find-newline buffer (rbuf-start buf) (rbuf-end buf) two-newlines?)
      (prog1 (subseq buffer (rbuf-start buf) it)
        (setf (rbuf-start buf) (1+ it))
        (sync buf)))))

(declaim (inline %read)
         (ftype (function (t t t) fixnum) %read))
(cffi:defcfun ("read" %read) :int
  (fd :int)
  (buf :pointer)
  (size :unsigned-long))

(defun recv (fd buffer offset)
  (declare (optimize speed (safety 0))
           (type fixnum offset)
           (type simple-byte-vector buffer))
  (the fixnum
    (cffi:with-pointer-to-vector-data (buf-ptr buffer)
      (%read fd
             (cffi:inc-pointer buf-ptr offset)
             (- (length buffer) offset)))))

(defun read-to-newline (socket buf two-newlines?)
  (declare (optimize speed))
  (loop (aif (eat-to-newline buf two-newlines?)
             (return it)
             (progn (prepare-read buf)
                    (incf (rbuf-end buf)
                          (recv (iolib:fd-of socket) (rbuf-buffer buf) (rbuf-end buf)))))))

(defun read-body (socket buf size)
  (prepare-read buf size)
  (multiple-value-bind (buffer bytes-read)
      (iolib:receive-from socket
                          :buffer (rbuf-buffer buf)
                          :start (rbuf-start buf)
                          :wait-all t)
    (when (/= size bytes-read) ;; FIXME
      (error "Error reading request body"))
    (incf (rbuf-end buf) bytes-read)
    (subseq buffer (rbuf-start buf) (rbuf-end buf))))

(defun url-encoding-decode (encoded)
  (declare (type simple-byte-vector encoded))
  (match-replace-all 
      encoded
    ((progn "%" (val (unsigned-byte :length 2 :base 16)))
     (make-array 1 :element-type '(unsigned-byte 8) :initial-element val))
    ("+" " ")))

(declaim (inline byte-vector=-fold-ascii-case))
(defun byte-vector= (a b)
  (declare (optimize speed) (type simple-byte-vector a b))
  (and (= (length a) (length b))
       (loop for i from 0 below (length a)
          always (= (aref a i) (aref b i)))))
