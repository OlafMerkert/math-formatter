(defpackage :math-formatter
  (:nicknames :mft)
  (:shadow #:integer
           #:numerator
           #:denominator
           #:open
           #:close
           #:sum
           #:number
           #:variable)
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
   #:base
   #:exponent
   #:subscript
   #:index
   #:infix-expression
   #:operators
   #:arguments
   #:parentheses
   #:body
   #:open
   #:close
   #:tuple
   #:prefix-expression
   #:factorisation
   #:rational-factorisation
   #:sum
   #:product
   #:continued-fraction
   #:*default-scaling*
   #:*default-colour*
   #:variable
   #:name
   #:content
   #:text
   #:number
   #:beside
   #:infinity
   #:ellipsis
   #:formatted-p))

(in-package :math-formatter)

