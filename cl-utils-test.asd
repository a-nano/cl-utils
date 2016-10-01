#|
  This file is a part of cl-utils project.
  Copyright (c) 2016 Akihide Nano (an74abc@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-utils-test-asd
  (:use :cl :asdf))
(in-package :cl-utils-test-asd)

(defsystem cl-utils-test
  :author "Akihide Nano"
  :license ""
  :depends-on (:cl-utils
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-utils"))))
  :description "Test system for cl-utils"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
