(in-package :math-utils-format)

;; elliptic-curve-weierstrass
(defmethod format% ((curve ec-ws:elliptic-curve-weierstrass))
  (formulas:formula-with-math-objects ((a (ec-ws:ws-a curve))
                                       (b (ec-ws:ws-b curve)))
                             `(= (^ y 2)
                                 (+ (^ x 3) (* ,a x) ,b))))
