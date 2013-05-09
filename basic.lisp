(in-package :math-formatter)

(defclass abstract-format ()
  ())

(defun carx (x)
  (if (consp x)
      (car x)
      x))


(defmacro define-abstract-format (name &rest slots)
  `(progn
     (defclass ,name (abstract-format)
        ,(mapcar (lambda (slot)
                   (destructuring-bind (slot-name &optional default-value) (mklist slot)
                     `(,slot-name :reader ,slot-name
                                  :initform ,default-value
                                  :initarg ,(keyw slot-name))))
                 slots))
     ;; todo really want &optional??
     (defun ,name (&optional ,@slots)
       (make-instance ',name
                      ,@(mapcan #`(,(keyw (carx a1)) ,(carx a1)) slots)))))

(defmacro define-composite-format (name slots &body body)
  `(defun ,name (&optional ,@slots)
     ,@body))

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

  (parentheses body (open #\() (close #\))))

(defun foreach (item list)
  (map 'list (ilambda (x) item) list))

(defun foreach1 (first item list)
  (list* first (foreach item (rest list))))

;;; todo composite formats
(define-composite-format prefix-expression (operator arguments (separator #\,))
  (infix-expression (list operator)
                    (parentheses (infix-expression (foreach1 nil separator arguments)
                                                   arguments))))

(define-composite-format factorisation (factors)
  (infix-expression (foreach nil factors)
                    (map 'list (lambda (factor)
                                 (if (consp factor)
                                     ;; todo verify factorisation uses conses?
                                     (superscript (car factor) (cdr factor))
                                     factor))
                         factors)))

(define-composite-format sum (summands)
  (infix-expression (foreach1 nil '+ summands) summands))

(define-composite-format product (factors)
  (infix-expression (foreach nil factors) factors))

(define-composite-format continued-fraction (partial-quotients)
  (if (length=1 partial-quotients)
      (first partial-quotients)
      (sum (list (first partial-quotients)
                 (fraction 1
                           (continued-fraction (rest partial-quotients)))))))
