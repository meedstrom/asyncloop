;;; asyncloop-debug.el --- Debug helpers -*- lexical-binding: t -*-
(require 'asyncloop)

(defun asyncloop-reset-all ()
  "Cancel all asyncloops and wipe `asyncloop-objects'.
Mainly for debugging."
  (interactive)
  (ignore-errors
    (cl-loop for cell in asyncloop-objects
             do (asyncloop-cancel (cdr cell))))
  (setq asyncloop-objects nil))

(provide 'asyncloop-debug)
