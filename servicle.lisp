
(ql:quickload 'cl-fad)
(ql:quickload 'hunchentoot)

(defun serve-this (args)
  (declare (ignore args))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :document-root (probe-file #P".")
                                    :port 4040))
  (read-char *standard-input*)
  (sb-ext:exit))


;./buildapp --load ~/quicklisp/setup.lisp --load servicle.lisp --entry serve-this --output fx
