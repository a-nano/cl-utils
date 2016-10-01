#|
  This file is a part of cl-utils project.
  Copyright (c) 2016 Akihide Nano (an74abc@gmail.com)
|#

#|
  Common lips utilitys

  Author: Akihide Nano (an74abc@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-utils-asd
  (:use :cl :asdf))
(in-package :cl-utils-asd)

(defsystem cl-utils
  :version "0.1"
  :author "Akihide Nano"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-utils"))))
  :description "Common lips utilitys"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-utils-test))))
