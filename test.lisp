(load "packages.lisp")
(load "smartmeter.lisp")
(load "data-logger.lisp")
(load "credentials.lisp")

(defparameter *bogus-meter*
  (make-instance 'smart-meter-texas:electric-meter 
                 :esiid "abc" 
                 :meter-number "abd"))
(defparameter *long-meter*
  (make-instance 'smart-meter-texas:electric-meter 
                 :esiid "sadkjfewafnawoenklamsclkmaelfjieafmlasmcaleifjlakrgnlajmlek" 
                 :meter-number "awer"))
