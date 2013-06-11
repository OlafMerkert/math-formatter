(defsystem math-formatter
  :depends-on (ol-utils iterate
                        math-utils
                        continued-fractions)
  :serial t
  :components ((:file "packages")
               (:file "format-definitions")
               (:file "math-utils-format")
               (:file "render-to-repl")
               (:file "render-to-tex")
               (:file "render-to-mathematica")))
