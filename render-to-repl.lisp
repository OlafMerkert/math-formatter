(defpackage :render-to-repl
  
  (:use :cl :ol :iterate )
  (:export))

(in-package :render-to-repl)

(defgeneric render (object stream))

(defmacro def-render-method ((format) &body body)
  `(defmethod render ((,format ,(symb+ :mft format)) stream)
     (flet ((pr (x) (princ x stream))
              (fmt  (&rest args) (apply #'format stream args))
              (rndr (x) (render x stream)))
       ,@body)))

(defmacro def-render-methods (&body specs)
  `(progn
     ,@(mapcar #`(def-render-method ,@a1) (group specs 2))))


;; placeholders
(def-render-methods
  (ellipsis) (pr "...")
  (infinity) (pr "∞"))

;; primitives
(def-render-methods
  (integer) (pr (mft:n integer))
  (number) (pr (mft:n number))
  (variable) (pr (mft:name variable))
  (text) (pr (mft:content text)))

;; composed
(def-render-method (fraction)
  (rndr (mft:numerator fraction))
  (pr "/")
  (rndr (mft:denominator fraction)))

(def-render-method (superscript)
  (rndr (mft:base superscript))
  (pr "^")
  (rndr (mft:exponent superscript)))

(def-render-method (subscript)
  (rndr (mft:base subscript))
  (pr "_")
  (rndr (mft:index subscript)))

(def-render-method (parentheses)
  (pr (mft:open parentheses))
  (rndr (mft:body parentheses))
  (pr (mft:close parentheses)))

(def-render-method (infix-expression)
  (iter (for op in (mft:operators infix-expression))
        (for arg in (mft:arguments infix-expression))
        (unless (first-iteration-p)
          (pr " "))
        (when op
          (pr op)
          (pr " "))
        (rndr arg)))

(defun short-type-of (object)
  (string-upcase (subseq (symbol-name (unbox (type-of object))) 0 3)))


(def-render-method (object-data)
  (fmt "#~A[" (short-type-of (mft:object object-data)))
  (rndr (mft:body object-data))
  (pr "]"))

;;; setup print-object methods

(bind-multi ((math-type finite-fields:integer-mod
                        elliptic-curve-weierstrass:point-2
                        polynomials:polynomial
                        power-series:power-series
                        power-series:constant-series
                        fractions:fraction))
  (defmethod print-object ((object math-type) stream)
    (render (math-utils-format:format object) stream)))
