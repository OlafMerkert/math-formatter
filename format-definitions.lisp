(in-package :math-formatter)

(defclass abstract-format ()
  ((colour :initarg :colour
           :initform 'black
           :accessor colour)
   (scaling :initarg :scaling
            :initform 0
            :accessor scaling)))

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
                      ,@(mapcan #`(,(keyw (unbox1 a1)) ,(unbox1 a1)) slots)))))

(defmacro define-composite-format (name slots &body body)
  `(defun ,name (&optional ,@slots)
     ,@body))

(defmacro define-abstract-formats (&body formats)
  `(progn ,@(mapcar #`(define-abstract-format ,@a1) formats)))

(define-abstract-formats
  (ellipsis)
  (infinity)
  (object-data body object)
  (integer n)
  (fraction numerator denominator)
  (superscript base exponent)
  (subscript base index)
  (infix-expression operators arguments)
  (parentheses body (open #\() (close #\))))



;;; todo composite formats
(define-composite-format tuple (coordinates (separator #\,) )
  (parentheses (infix-expression (foreach1 nil separator coordinates)
                                  coordinates)))

(define-composite-format prefix-expression (operator arguments (separator #\,))
  (infix-expression (list operator)
                    (tuple arguments separator)))

(define-composite-format factorisation (factors)
  (infix-expression (foreach1 nil '* factors)
                    (map 'list (lambda (factor)
                                 (if (consp factor)
                                     ;; todo verify factorisation uses conses?
                                     (superscript (car factor) (cdr factor))
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
