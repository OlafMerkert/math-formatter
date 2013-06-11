(defpackage :math-utils-format
  (:shadow #:format)
  (:use :cl :ol :iterate)
  (:import-from :finite-fields #:integer-mod #:remainder #:modulus)
  (:import-from :elliptic-curve-weierstrass #:point-2 #:x #:y #:ec-point-infinity #:elliptic-curve-weierstrass #:ws-a #:ws-b)
  (:import-from :generic-math #:zero-p #:one-p  #:minus-p)
  (:import-from :polynomials #:polynomial #:coefficients #:degree #:var)
  (:import-from :power-series #:power-series #:constant-series #:constant-coefficient)
  (:import-from :valuations-coeff #:polynomial-values #:power-series-values)
  (:export
   #:format
   #:*print-poly-pretty*))

(in-package :math-utils-format)


(defgeneric format% (object))

(defun format (object)
  (format% object))

(defparameter *integer-factorisation* nil)

;;; primitive objects
(defmethod format% ((integer integer))
  ;; TODO automatic factorisation
  (mft:integer integer))

(defmethod format% ((rational rational))
  (mft:fraction (format (numerator rational))
                (format (denominator rational))))


(defmethod format% ((symbol symbol))
  (mft:variable symbol))

(defmethod format% ((string string))
  (mft:text string))

(defmethod format% ((character character))
  (mft:text character))

(defmethod format% ((number number))
  (mft:number number))

(defmethod format% ((real real))
  (mft:number real))

;; complex numbers
(defmethod format% ((complex complex))
  ;; TODO sign detection.
  (mft:sum (list (format (realpart complex))
                 (mft:product (list (format% #\i)
                                    (format (imagpart complex)))))))


;;; more complicated math objects
(defmethod format% ((integer-mod integer-mod))
  (mft:beside (format (remainder integer-mod))
              (mft:tuple (list (format% "mod")
                               (format (modulus integer-mod))))))

(defmethod format% ((point-2 point-2))
  (mft:tuple (list (x point-2) (y point-2))))

(defmethod format% ((ec-point-infinity ec-point-infinity))
  (mft:infinity))

(defmethod format% ((fraction fractions:fraction))
  (mft:fraction (fractions:numerator fraction)
                (fractions:denominator fraction)))

;;; polynomials and power series
(defmethod format% ((constant-series constant-series))
  (format (constant-coefficient constant-series)))

(defparameter print-additional-terms 5)

(defparameter *print-poly-pretty* nil)

;;; four entries in the list: coefficient, exponent, whether coeff is
;;; one, and whether it had sign swapped.
(defun all-coeffs (coefficients degree)
  (iter (for i downfrom degree)
        (for c in-vector coefficients)
        (collect (list c i))))

(defun clean-coeffs (coefficients degree)
  (iter (for i downfrom degree)
        (for c in-vector coefficients)
        (for m = (and (not (first-iteration-p))
                      (minus-p c)))
        (for cc = (if m (gm:- c) c))
        (unless (zero-p c)
          (collect (list cc i (one-p cc) m)))))

;;; TODO some inconsistence with print-superscript, which does not get
;;; called with results of print-math-object
(defun format-monomial (var params)
  (dbind (coeff deg one neg) params
    (declare (ignorable neg))
    (cond ((and one (zerop deg))
           (format 1))
          ((zerop deg)
           (format coeff))
          ((and one (= deg 1))
           (format var))
          ((= deg 1)
           (mft:product (list
                         (format coeff)
                         (format var))))
          (one
           (mft:superscript (format var) (format deg)))
          (t
           (mft:product
            (list (format coeff)
                  (mft:superscript (format var) (format deg))))))))

(defun format-monomial/all (var params)
  (dbind (coeff deg) params
    (mft:product (list
                  (format coeff)
                  (mft:superscript (format var) (format deg))))))

(defun format-polynomial (polynomial)
  (let ((cc (clean-coeffs (coefficients polynomial) (degree polynomial))))
    (if cc
        (mft:infix-expression
         (list* (if (fourth (first cc)) '- nil)
                (mapcar (lambda (x) (if (fourth x) '- '+)) (rest cc)))
         (mapcar (clambda format-monomial (var polynomial) x!) cc))
        (format 0))))

(defun format-polynomial/all (polynomial)
  (let ((cc (all-coeffs (coefficients polynomial) (degree polynomial))))
    (mft:sum
     (mapcar (clambda format-monomial/all (var polynomial) x!) cc))))

(defun format-power-series (power-series)
  (let ((cc (clean-coeffs (lazy-array-take (coefficients power-series)
                                           (+ (max (degree power-series) 0)
                                              print-additional-terms)
                                           nil)
                          (degree power-series))))
    (mft:infix-expression
     (append1
      (list* (if (fourth (first cc)) '- nil)
             (mapcar (lambda (x) (if (fourth x) '- '+)) (rest cc)))
      '+)
     (append1
      (mapcar (clambda format-monomial (var power-series) x!) cc)
      (mft:ellipsis)))))

(defun format-power-series/all (power-series)
  (let ((cc (all-coeffs (lazy-array-take (coefficients power-series)
                                         (+ (degree power-series) print-additional-terms)
                                         nil)
                        (degree power-series))))
    (mft:sum
     (append1 (mapcar (clambda format-monomial/all (var power-series) x!) cc)
              (mft:ellipsis)))))

(defmethod format% ((polynomial polynomial))
  (if *print-poly-pretty*
      (format-polynomial polynomial)
      (format-polynomial/all polynomial)))

(defmethod format% ((power-series power-series))
  (if *print-poly-pretty*
      (format-power-series power-series)
      (format-power-series/all power-series)))

(defmethod format% ((polynomial-values polynomial-values))
  (format-polynomial/all polynomial-values))

(defmethod format% ((power-series-values power-series-values))
  (format-power-series/all power-series-values))


;; elliptic-curve-weierstrass
;; elementary-matrix
;; vector

;;; TODO what about presentations? those should be automatically generated.
