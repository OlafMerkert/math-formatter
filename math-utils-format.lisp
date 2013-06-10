(defpackage :math-utils-format
  (:shadow #:format)
  (:use :cl :ol :iterate)
  (:import-from :finite-fields #:integer-mod #:remainder #:modulus)
  (:export))

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


;;; fancier stuff
(defmethod format% ((integer-mod integer-mod))
  (mft:beside (format (remainder integer-mod))
              (mft:tuple (list (format% "mod")
                               (format (modulus integer-mod))))))

;; polynomial
;; polynomial-values
;; power-series-values
;; power-series
;; constant-series
;; elliptic-curve-weierstrass
;; point-2
;; ec-point-infinity
;; elementary-matrix
;; vector

