(defpackage :math-formatter
  (:nicknames :mft)
  (:shadow #:integer
           #:numerator
           #:denominator
           #:open
           #:close
           #:sum)
  (:use :cl :ol :iterate )
  (:export
   #:+dots+
   #:+infinity+
   #:colour
   #:scaling
   #:object-data
   #:body
   #:object
   #:integer
   #:n
   #:fraction
   #:numerator
   #:denominator
   #:superscript
   #:base exponent
   #:subscript
   #:index
   #:infix-expression
   #:operators
   #:arguments
   #:parentheses
   #:body
   #:open
   #:close
   ))

(in-package :math-formatter)


