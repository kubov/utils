(defparameter *sieve* nil)
(defparameter *limit* 10000000)
(defparameter *lower-limit* 100)

(defun get-random-coprime-to (n)
  (let ((x (random n)))
    (loop do
         (setf x (random n))
       while (/= (gcd n x) 1))
    x))

(defun gcd-extended (x y)
  (do ((a1 1 a2)
       (b1 0 b2)
       (z1 x z2)
       (a2 0 (- a1 (* d a2)))
       (b2 1 (- b1 (* d b2)))
       (z2 y (- z1 (* d z2)))
       (d  0))
      ((zerop z2) (values a1 x b1 y z1))
    (setq d (floor z1 z2))))

(defun modular-inverse (a modulus)
  (multiple-value-bind (a1 x b1 y z1)
		       (gcd-extended a modulus)
    (if (= z1 1) a1
	(error "modulus error"))))

(defun init-sieve ()
  (setf *sieve* (make-array *limit*))
  (loop for i from 0 below *limit* do
       (setf (aref *sieve* i) i))
  (loop for i from 2 below *limit* do
       (loop for j from (+ i i) by i below *limit* do
            (setf (aref *sieve* j) nil))))

(defun get-random-prime ()
  (if *sieve*
      (let ((ri nil))
        (loop do
             (setf ri (+ (random
                          (- *limit* *lower-limit*))
                         *lower-limit*))
              while (equal (aref *sieve* ri) nil))
        (aref *sieve* ri))
      (init-sieve)))

(defun modular-exponent (a e m)
  (let ((c 1) (base a))
    (do 
	((if (oddp e)
	    (setf c (mod (* c base) m)))
	    (setf e (ash c -1))
	 (setf base (mod (* base base) m)))
	t)
    c))

(defun generate-keys ()
  (let* ((p (get-random-prime))
         (q (get-random-prime))
         (n (* p q))
         (totient (* (- p 1) (- q 1)))
         (e (get-random-coprime-to totient))
         (d (modular-inverse e totient)))
    `(:pubkey (,e . ,n)
              :privkey (,d . ,n))))

(defun encrypt (message pubkey)
  (let ((e (car pubkey))
        (n (cdr pubkey))
        (ret (make-array (array-dimension message 0))))
    (loop for i from 0 below (array-dimension message 0) do
         (setf (aref ret i) (modular-exponent
                             (if (integerp (aref message i))
                                 (aref message i)
                                 (char-int (aref message i))) e n)))
    ret))

