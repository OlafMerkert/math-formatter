(in-package :math-formatter)

(defparameter *default-scaling* 0)
(defparameter *default-colour* nil)

(defclass abstract-format ()
  ((colour :initarg :colour
           :initform *default-colour*
           :accessor colour)
   (scaling :initarg :scaling
            :initform *default-scaling*
            :accessor scaling)))

(defun formatted-p (object)
  (typep object 'abstract-format))

(defmacros! define-abstract-formats (name &rest slots)
  `(progn
     (defclass ,name (abstract-format)
       ,(mapcar (lambda (slot)
                  (destructuring-bind (slot-name &optional default-value) (mklist slot)
                    `(,slot-name :reader ,slot-name
                                 :initform ,default-value
                                 :initarg ,(keyw slot-name))))
                slots))
     (defun ,name (&optional ,@slots
                             (colour *default-colour*)
                             (scaling *default-scaling*))
       (make-instance ',name
                      ,@(mapcan #`(,(keyw (unbox1 a1)) ,(unbox1 a1))
                                (append slots '(scaling colour)))))))

(defmacro define-composite-format (name slots &body body)
  `(defun ,name (&optional ,@slots
                           (colour *default-colour*)
                           (scaling *default-scaling*))
     (let ((*default-scaling* scaling)
           (*default-colour* colour))
       ,@body)))

(define-abstract-formats
  (ellipsis)
  (infinity)
  (object-data body object)
  ;; atomic formats
  (integer n)
  (number n)
  (variable name)
  (text content)
  ;; compound formats
  (fraction numerator denominator)
  (superscript base exponent)
  (subscript base index)
  (infix-expression operators arguments)
  (parentheses body (open #\() (close #\)))
  (grid2 elements))

;; automatically scale exponent and index down
(defmethod initialize-instance :after ((subscript subscript) &key)
  (decf (scaling (index subscript))))

(defmethod initialize-instance :after ((superscript superscript) &key)
  (decf (scaling (exponent superscript))))

;; helper functions for working with grids
(defun dim (grid2)
  (length (array-dimensions (elements grid2))))


;;; composite formats
(define-composite-format tuple (coordinates (separator #\,) )
  (parentheses (if (<= (length coordinates) 1) (first coordinates)
                   (infix-expression (foreach1 nil separator coordinates)
                                     coordinates))))

(define-composite-format prefix-expression (operator arguments (separator #\,))
  (infix-expression1 operator (tuple arguments separator)))

(define-composite-format factorisation (factors)
  (infix-expression (foreach1 nil '* factors)
                    (map 'list (lambda (factor)
                                 (if (consp factor)
                                     (superscript (car factor)
                                                  (cdr factor))
                                     factor))
                         factors)))

(define-composite-format rational-factorisation (factors)
  (let ((nums (remove-if-not (lambda (factor) (or (atom factor) (<= 0 (cdr factor)))) factors))
        (dens (remove-if-not (lambda (factor) (and (consp factor) (> 0 (cdr factor)))) factors)))
    (fraction (factorisation nums) (factorisation dens))))

(define-composite-format sum (summands)
  (infix-expression (foreach1 nil '+ summands) summands))

(define-composite-format product (factors)
  (infix-expression (foreach nil factors) factors))

(define-composite-format continued-fraction (partial-quotients)
  (if (length=1 partial-quotients)
      (first partial-quotients)
      (sum (list (first partial-quotients)
                 (fraction (integer 1)
                           (continued-fraction (rest partial-quotients)))))))

(define-composite-format beside (things)
  (infix-expression (foreach nil things) things))

(define-composite-format infix-expression1 (op arg)
  (infix-expression (list op) (list arg)))

(define-composite-format matrix2 (elements)
  (parentheses (grid2 elements)))
