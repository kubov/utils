(require :usocket)
(in-package :usocket)

(defparameter *server* nil)
(defparameter *handlers* (make-hash-table :test 'equal))

(defun default-handler (code state)
  (format t "unused key ~A ~A~%" code state))

(defmacro defhandler (code state &rest body)
  `(setf (gethash (cons ,code ,state) *handlers*) (lambda (code state) ,@body)))

(defun dispatch (code state)
  (let ((handler (gethash (cons code state) *handlers*)))
    (if handler
	(funcall handler code state)
	(default-handler code state))))

(defun forever (fun)
  (lambda () (loop do (funcall fun))))

(defun handle (strm)
  (loop do
       (let* ((sym (with-input-from-string (str (read-line strm)) (read str)))
	      (code (car sym))
	      (state (cdr sym)))
	 (dispatch code state))))

(defun start (port)
  (setf *server* (socket-server "0.0.0.0" port 'handle))))

(defhandler 'up 1
  (format t "Volume up~%"))
