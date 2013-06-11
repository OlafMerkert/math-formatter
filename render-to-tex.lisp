(defpackage :render-to-tex
  (:use :cl :ol :iterate )
  (:export))

(in-package :render-to-tex)

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
  (ellipsis) (pr "\\dots")
  (infinity) (pr "\\infty"))

;; primitives
(def-render-methods
  (integer) (pr (mft:n integer))
  (number) (pr (mft:n number))
  (variable) (let ((name (mkstr (mft:name variable))))
               (if (length=1 name)
                   (pr name)
                   (fmt "\\mathrm{~A}" name)))
  (text) (fmt "\\text{~A}" (mft:content text)))

;; composed
(def-render-method (fraction)
  (pr "\\frac{")
  (rndr (mft:numerator fraction))
  (pr "}{")
  (rndr (mft:denominator fraction))
  (pr "}"))

(def-render-method (superscript)
  (pr "{")
  (rndr (mft:base superscript))
  (pr "}^{")
  (rndr (mft:exponent superscript))
  (pr "}"))

(def-render-method (subscript)
  (pr "{")
  (rndr (mft:base subscript))
  (pr "}_{")
  (rndr (mft:index subscript))
  (pr "}"))

(def-render-method (parentheses)
  (pr "\\left")
  (pr (mft:open parentheses))
  (rndr (mft:body parentheses))
  (pr "\\right")
  (pr (mft:close parentheses)))

(def-render-method (infix-expression)
  (iter (for op in (mft:operators infix-expression))
        (for arg in (mft:arguments infix-expression))
        (unless (first-iteration-p)
          (pr " "))
        (if op
            (fmt "~A " op)
            (unless (first-iteration-p)
              (pr "\\, ")))
        (rndr arg)))

(defun short-type-of (object)
  (string-upcase (subseq (symbol-name (unbox (type-of object))) 0 3)))

(def-render-method (object-data)
  (rndr (mft:body object-data)))

;;; setup print-object methods

(defun print-tex (object stream)
  (let ((math-utils-format:*print-poly-pretty* t))
   (render (math-utils-format:format object) stream)))

