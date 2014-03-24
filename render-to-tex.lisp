(defpackage :render-to-tex
  (:use :cl :ol :iterate )
  (:export
   #:render))

(in-package :render-to-tex)

(defgeneric render (object stream))

(defmacro def-render-method ((format) &body body)
  `(defmethod render ((,format ,(symb+ :mft format)) stream)
     (labels ((pr (x) (princ x stream))
              (fmt  (&rest args) (apply #'format stream args))
              (rndr (x) (render x stream))
              (varfmt (name) (variable->command (mkstr name) stream)))
       ,@body)))

;;; automatic recognition of commands (for operators and variables)
(defpar known-commands '("alpha"
                         "beta"
                         "gamma"
                         "delta"
                         "epsilon"
                         "phi"
                         "psi"
                         "pi"
                         "theta"
                         "exp"
                         "log"
                         "lim"))

(defpar uppercase-vars '(#\X))

(defun variable->command (name stream)
  (if (length=1 name)
      (if (member (elt name 0) uppercase-vars :test #'char-equal)
          (format stream "~:@(~A~)" name)
          (format stream "~(~A~)" name))
      (if (member name known-commands :test #'string-equal)
          (format stream "~(\\~A~)" name)
          (format stream "~(\\mathrm{~A}~)" name))))

(defmacro def-render-methods (&body specs)
  `(progn
     ,@(mapcar #`(def-render-method ,@a1) (group specs 2))))

(defmethod render ((obj (eql nil)) stream)
  (warn "Rendering NIL"))

;; placeholders
(def-render-methods
  (ellipsis) (pr "\\dots")
  (infinity) (pr "\\infty"))

;; primitives
(def-render-methods
  (integer) (pr (mft:n integer))
  (number) (pr (mft:n number))
  (variable) (varfmt (mft:name variable))
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
            (varfmt op)
            (unless (first-iteration-p)
              (pr "\\, ")))
        (rndr arg)))

(defun short-type-of (object)
  (string-upcase (subseq (symbol-name (unbox (type-of object))) 0 3)))

(def-render-method (object-data)
  (rndr (mft:body object-data)))

(def-render-method (grid2)
  (ecase (mft:dim grid2)
    (1 ;; output horizontal grid
     (pr "\\begin{matrix}")
     (iter (for element in-vector (mft:elements grid2))
           (unless (first-iteration-p) (pr " & "))
           (rndr element))
     (pr "\\end{matrix}"))
    (2 ;; output 2d grid
     (pr "\\begin{matrix}")
     (let ((array (mft:elements grid2)))
       (iter (for i from 0 below (array-dimension array 0))
             (unless (first-iteration-p) (pr " \\\\"))
             (iter (for j from 0 below (array-dimension array 1))
                   (unless (first-iteration-p) (pr " & "))
                   (rndr (aref array i j)))))
     (pr "\\end{matrix}"))))

;;; setup print-object methods

(defun print-tex (object stream)
  (let ((math-utils-format:*print-poly-pretty* t))
   (render (math-utils-format:format object) stream)))

;;; todo treat thing like \sqrt specially
