(defpackage :render-to-repl
  (:use :cl :ol :iterate )
  (:export))

(in-package :render-to-repl)

(defgeneric render (object stream))

(defmacro def-render-method ((format) &body body)
  `(defmethod render ((,format ,(symb+ :mft format)) stream)
     (flet ((pr (x) (princ x stream))
            (fmt  (&rest args) (apply #'format stream args))
            (rndr (x) (render x stream))
            (nl () (terpri stream)))
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

(defun pad-string (string width &optional (pad-character #\space) at-end)
  "Pad the given `string' with `pad-character', at the front or
`at-end', such that the length becomes `width'."
  (let ((missing (- width (length string))))
    (signcase missing
              (error "Cannot pad: string ~S is already longer than desired with ~A"
                     string width)
              string
              (if at-end
                  (concatenate 'string string (n-copies missing pad-character))
                  (concatenate 'string (n-copies missing pad-character) string)))))

(defun pad-columns (array &optional (pad-character #\space) at-end)
  "For a 2d `array' of strings, ensure that all columns have same
length by adding `pad-character' by default at the front, or at the
end if `at-end' is non-nil. Note this operation is destructive."
  (dotimes (j (array-dimension array 1))
    ;; first compute the maximal width
    (let ((width (iter (for i from 0 below (array-dimension array 0))
                       (maximizing (length (aref array i j))))))
      (dotimes (i (array-dimension array 0))
        (setf (aref array i j)
              (pad-string (aref array i j) width pad-character at-end)))))
  array)

(def-render-method (grid2)
  (ecase (mft:dim grid2)
    (1 ;; output horizontal grid
     (iter (for element in-vector (mft:elements grid2))
           (unless (first-iteration-p) (pr "  "))
           (rndr element)))
    (2 ;; output 2d grid
     (let ((array (pad-columns
                    (map-array1 (lambda (x) (with-output-to-string (string)
                                         (render x string)))
                                (mft:elements grid2))
                    #\space)))
       (iter (for i from 0 below (array-dimension array 0))
             (unless (first-iteration-p) (nl))
             (iter (for j from 0 below (array-dimension array 1))
                   (unless (first-iteration-p) (pr "  "))
                   (pr (aref array i j))))))))

;;; setup print-object methods

(bind-multi ((math-type finite-fields:integer-mod
                        elliptic-curve-weierstrass:point-2
                        polynomials:polynomial
                        power-series:power-series
                        power-series:constant-series
                        fractions:fraction
                        linear-algebra/vectors:vector))
  (defmethod print-object ((object math-type) stream)
    (princ "#M[" stream)
    (render (math-utils-format:format object) stream)
    (princ "]" stream)
    object))
