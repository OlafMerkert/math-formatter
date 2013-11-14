(defpackage :math-utils-format
  (:shadow #:format)
  (:use :cl :ol :iterate)
  (:import-from :finite-fields #:integer-mod #:remainder #:modulus)
  (:import-from :elliptic-curve-weierstrass #:point-2 #:x #:y #:ec-point-infinity #:elliptic-curve-weierstrass #:ws-a #:ws-b)
  (:import-from :generic-math #:zero-p #:one-p  #:minus-p)
  (:import-from :polynomials #:polynomial #:coefficients #:degree #:var)
  (:import-from :power-series #:power-series #:constant-series #:constant-coefficient)
  (:import-from :valuations-coeff #:polynomial-values #:power-series-values)
  (:import-from :continued-fractions-power-series #:continued-fraction #:partial-quotients)
  (:import-from :infinite-math #:infinity+ #:infinity-)
  (:shadowing-import-from :linear-algebra/vectors #:vector )
  (:import-from :linear-algebra/vectors #:matrix #:entries)
  (:export
   #:format
   #:*print-poly-pretty*
   #:*print-additional-terms*
   #:*continued-fraction-display-length*
   #:*enable-presentations*
   #:format-pretty))

(in-package :math-utils-format)


(defgeneric format% (object))

(defparameter *enable-presentations* nil)

(defmacro! predif ((o!test object) then else)
  "Like `if', but if `test' is a function, check for its value on
`object'. Otherwise, fall back to the behaviour of `if'."
  `(if (or (not ,g!test)
           (and (functionp ,g!test)
                (not (funcall ,g!test ,object))))
       ,else
       ,then))

(defun format (object)
  (predif (*enable-presentations* object)
          (mft:object-data (format% object)
                           object)
          (format% object)))

(defparameter *integer-factorisation* nil)

;;; primitive objects
(defmethod format% ((integer integer))
  ;; TODO automatic factorisation
  ;; TODO add colour
  (mft:integer integer))

(defmethod format% ((rational rational))
  (mft:fraction (format (numerator rational))
                (format (denominator rational))))


(defmethod format% ((symbol symbol))
  ;; TODO add colour
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
  (mft:beside (list (format (remainder integer-mod))
                    (mft:parentheses
                     (mft:beside(list (format% "mod")
                                      (format (modulus integer-mod))))))))

(defmethod format% ((point-2 point-2))
  (mft:tuple (list (format (x point-2)) (format (y point-2)))))

(defmethod format% ((ec-point-infinity ec-point-infinity))
  (mft:infinity))

(defmethod format% ((fraction fractions:fraction))
  ;; no need for denom, when it is simple.
  (if (or (gm:zero-p (fractions:numerator fraction))
          (gm:one-p (fractions:denominator fraction)))
      (format (fractions:numerator fraction))
      (mft:fraction (format (fractions:numerator fraction))
                    (format (fractions:denominator fraction)))))

;;; polynomials and power series
(defmethod format% ((constant-series constant-series))
  (format (constant-coefficient constant-series)))

(defparameter *print-additional-terms* 5)

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
                                              *print-additional-terms*)
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
                                         (+ (degree power-series) *print-additional-terms*)
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

;;; continued fractions
(defparameter *continued-fraction-display-length* 5)

(defmethod format% ((continued-fraction continued-fraction))
  (mft:continued-fraction (append1
                           (map 'list
                                #'format
                                (lazy-array-take
                                 (partial-quotients continued-fraction)
                                 *continued-fraction-display-length*
                                 nil))
                           (mft:ellipsis))))

(defmethod format% ((inf (eql infinity+)))
  (mft:infinity))

(defmethod format% ((inf (eql infinity-)))
  (mft:infix-expression1 '- (mft:infinity)))

;; todo elliptic-curve-weierstrass
;; todo elementary-matrix
;;; vector
(defmethod format% ((vector vector))
  (mft:matrix2 (map-array1 #'format (entries vector))))

;;; TODO what about presentations? those should be automatically generated.

(defun format-pretty (object &key (presentations t))
  (let ((*print-poly-pretty* t)
        (*enable-presentations* presentations))
    (format object)))
