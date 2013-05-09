(in-package :math-formatter)

(defclass abstract-format ()
  ())

(defmacro define-abstract-format (name &rest slots)
  `(progn
     (defclass ,name (abstract-format)
        ,(mapcar (lambda (slot)
                   (destructuring-bind (slot-name &optional default-value) (mklist slot)
                     `(,slot-name :reader ,slot-name
                                  :initform ,default-value
                                  :initarg ,(keyw slot-name))))
                 slots))))

(defmacro define-abstract-formats (&body formats)
  `(progn ,@(mapcar #`(define-abstract-format ,@a1) formats)))

(define-abstract-formats
  (object-data body object)
  (integer)
  (ellipsis)
  (fraction numerator denominator)
  (superscript base exponent)
  (subscript base index)
  (infix-expression operators arguments)
  (prefix-expression operator arguments (separator #\,))
  (parentheses body (open #\() (close #\))))

;;; todo composite formats
