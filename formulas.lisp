(defpackage :formulas
  (:use :cl :ol :iterate )
  (:export
   #:formula-prepare
   #:+
   #:=
   #:>=
   #:<=
   #:>
   #:<
   #:*
   #:/
   #:-
   #:^
   #:_
   #:formula-with-math-objects))

(in-package :formulas)

;;; support for rendering formal lisp expressions to the
;;; math-interactor, this is the analogue of math-output-prepare for
;;; S-expressions

(defun formula-prepare (formula)
  (cond ((consp formula) (formula-prepare% (car formula) (cdr formula)))
        ;; allow insertion of prepared math-objects into formulas 
        ((mft:formatted-p formula) formula)
        ((atom formula) (atom-prepare formula))))

(defun atom-prepare (atom)
  (etypecase atom
    (integer   (mft:integer atom))
    (character (mft:text atom))
    (string    (mft:text atom))
    (symbol    (mft:variable atom))
    (number    (mft:number atom))))

(defgeneric formula-prepare% (function-symbol arguments))

(defmacro def-formula-prepare (head &body body)
  `(defmethod formula-prepare% ((head (eql ',head)) arguments)
     (let ((n (length arguments)))
       (declare (ignorable n))
       ,@body)))

(bind-multi ((op + = >= <= > <))
  (def-formula-prepare op
    (mft:infix-expression (foreach1 nil 'op arguments)
                          (mapcar #'formula-prepare arguments))))

(def-formula-prepare * 
  (mft:product (mapcar #'formula-prepare arguments)))

(def-formula-prepare nil
  (mft:beside (mapcar #'formula-prepare arguments)))

(def-formula-prepare / 
  (case n
    (1 (mft:fraction (mft:integer 1) (formula-prepare (first arguments))))
    (2 (mft:fraction (formula-prepare (first arguments))
                     (formula-prepare (second arguments))))
    (t (mft:fraction (formula-prepare (first arguments))
                     (mft:product (mapcar #'formula-prepare (rest arguments)))))))

(def-formula-prepare - 
  (if (= 1 n)
      (mft:infix-expression1 '- (formula-prepare (first arguments)))
      (mft:infix-expression (foreach1 nil '- arguments)
                            (mapcar #'formula-prepare arguments))))

(def-formula-prepare ^
  (mft:superscript (formula-prepare (first arguments))
                   (formula-prepare (second arguments))))

(def-formula-prepare _
  (mft:subscript (formula-prepare (first arguments))
                 (formula-prepare (second arguments))))

(defmethod formula-prepare% ((function-symbol symbol) arguments)
  ;; TODO put the arguments in brackets, perhaps separated by commas
  (mft:prefix-expression function-symbol (mapcar #'formula-prepare arguments)))


(defmacro formula-with-math-objects (math-objects formula)
  `(let ,(mapcar (compose #`(,(first a1) (math-utils-format:format-pretty
                                          ,(second a1)
                                          :presentations t))
                          (lambda (mo-spec)
                            (cond ((consp mo-spec) mo-spec)
                                  ((symbolp mo-spec) (list mo-spec mo-spec))
                                  (t (error "invalid math-object-spec ~A" mo-spec)))))
                 math-objects)
     (formula-prepare ,formula)))

;; TODO spacers
