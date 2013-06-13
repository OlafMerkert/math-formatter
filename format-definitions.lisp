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

(defmacro define-abstract-formats (&body formats)
  `(progn ,@(mapcar #`(define-abstract-format ,@a1) formats)))

(define-abstract-formats
  (ellipsis)
  (infinity)
  (object-data body object)
  (integer n)
  (number n)
  (variable name)
  (text content)
  (fraction numerator denominator)
  (superscript base exponent)
  (subscript base index)
  (infix-expression operators arguments)
  (parentheses body (open #\() (close #\))))

;; automatically scale exponent and index down
(defmethod initialize-instance :after ((subscript subscript) &key)
  (decf (scaling subscript)))

(defmethod initialize-instance :after ((superscript superscript) &key)
  (decf (scaling superscript)))


;;; todo composite formats
(define-composite-format tuple (coordinates (separator #\,) )
  (parentheses (if (<= (length coordinates) 1) (first coordinates)
                   (infix-expression (foreach1 nil separator coordinates)
                                     coordinates))))

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

(define-composite-format beside (things)
  (infix-expression (foreach nil things) things))
