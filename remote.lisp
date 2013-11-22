(require :usocket)

(defparameter *server* nil)
(defparameter *handlers* (make-hash-table :test 'equal))
(defparameter *script-path* (concatenate 'string
					 (sb-ext:posix-getenv "HOME")
					 "/code/utils/scripts/"))

(defun default-handler (code state)
  (format t "unused key ~A ~A~%" code state))

(defmacro defhandler (code state &rest body)
  `(setf (gethash (cons ,code ,state) *handlers*) (lambda (code state) ,@body)))

(defun dispatch (code state)
  (let ((handler (gethash (cons code state) *handlers*)))
    (if handler
	(funcall handler code state)
	(default-handler code state))))

(defun handle (strm)
  (loop do
       (let* ((sym (with-input-from-string (str (read-line strm)) (read str)))
	      (code (car sym))
	      (state (cdr sym)))
	 (dispatch code state))))

(defun start (port)
  (setf *server* (usocket:socket-server "0.0.0.0" port 'handle)))

(defun call-script (script-name)
  (sb-ext:run-program "/bin/sh" `("-c" ,(concatenate 'string *script-path* script-name))))

(defhandler 'up 'down
  (call-script "volume-up"))

(defhandler 'down 'down
  (call-script "volume-down"))

(defhandler 'asterix 'down
  (call-script "mute"))

(defhandler 'hash 'down
  (call-script "cmus-run"))

(defhandler 'right 'down
  (call-script "cmus-next"))

(defhandler 'left 'down
  (call-script "cmus-prev"))

(defhandler 'ok 'down
  (call-script "cmus-toggle"))

(defhandler 'zero 'down
  (call-script "cmus-random"))
