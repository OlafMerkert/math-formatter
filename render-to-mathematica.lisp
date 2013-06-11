(defpackage :render-to-mathematica
  (:use :cl :ol :iterate )
  (:export))

(in-package :render-to-mathematica)

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
  (variable) (fmt "~(~A~)" (mft:name variable))
  (text) (fmt "\"~A\"" (mft:content text)))

;; composed
(def-render-method (fraction)
  (pr "(")
  (rndr (mft:numerator fraction))
  (pr ")/(")
  (rndr (mft:denominator fraction))
  (pr ")"))

(def-render-method (superscript)
  (pr "(")
  (rndr (mft:base superscript))
  (pr ")^(")
  (rndr (mft:exponent superscript))
  (pr ")"))

(def-render-method (subscript)
  (rndr (mft:base subscript))
  (pr "[")
  (rndr (mft:index subscript))
  (pr "]"))

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
  (rndr (mft:body object-data)))

;;; generate output string
(defun mathematica-export (object)
  (with-output-to-string (stream)
    (render (math-utils-format:format object) stream)))

;; TODO figure out whether these output functions are sufficient, or
;; whether we need more specific stuff.

