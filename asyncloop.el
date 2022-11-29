;;; asyncloop.el --- Non-blocking series of functions -*- lexical-binding: t -*-

;; Copyright (C) 2022 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;; Author:  <meedstrom@teknik.io>
;; Created: 2022-10-30
;; Version: 0.1.0
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/asyncloop
;; Package-Requires: ((emacs "28.1") (named-timer "0.1"))

;;; Commentary:

;; Use `asyncloop-run' to call a series of functions without hanging Emacs.

;; TODO: optimize performance so the package can be used even for millions of
;; calls.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'named-timer) ;; prevents bugs every day

(defvar asyncloop-debug t
  "Whether to reveal buffers of debug messages.")

(defun asyncloop-debug-buffer (id)
  "Buffer to write debug messages to."
  (let ((bufname (concat (unless asyncloop-debug " ") "*" (symbol-name id) "*")))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname)
          (set (make-local-variable 'window-point-insertion-type) t)
          (setq-local truncate-lines t)
          (setq-local buffer-read-only nil)
          (current-buffer)))))

;; TODO: find some premade package for custom message buffers -- the ones I've
;; found all deal with showing log files, but here we are visiting no file.
(defun asyncloop-echo (id &rest args)
  "Log a message to the debug buffer associated with ID.
Arguments ARGS same as for `format'."
  (declare (indent defun))
  (with-current-buffer (asyncloop-debug-buffer id)
    (goto-char (point-min))
    (insert (apply #'format
                   (cons (concat (format-time-string "%T: ")
                                 (car args))
                         (cdr args))))
    (newline)
    ;; Pass the string back to caller so it can be used to also signal
    ;; something, as in (error (asyncloop-echo id "..."))
    (apply #'format args)))

(defclass asyncloop ()
  ((funs            :initarg :funs)
   (remainder       :initarg :remainder       :initform nil)
   (last-idle-value :initarg :last-idle-value :initform 0)
   (on-abort        :initarg :on-abort        :initform nil)
   (per-stage       :initarg :per-stage       :initform nil)))

(defvar asyncloop-objects nil
  "Alist identifying unique asyncloop objects.")

(defmacro asyncloop-remainder (id)
  "Get the variable \"remainder\" from asyncloop identified by ID.
The expression (asyncloop-remainder ID) returns the list of functions
that remain to be executed for that asyncloop.

If, as most likely, we're called from within a function that's
called by a running asyncloop, you can schedule a new function
invocation to be run on the next \"tick\" like this:

(push #'some-additional-function (asyncloop-remainder ID))

which can be useful for looping through a list by consuming it
one item at a time: have a function push itself back onto the
execution queue until the list is empty."
  `(slot-value (alist-get ,id asyncloop-objects) :remainder))

(defun asyncloop-defuse (id)
  "Stop continuation of asyncloop identified by ID, and ensure
it'll be restarted fresh on the next `asyncloop-run'.  Finally
call the associated :on-abort function if it exists."
  (with-slots (:remainder :on-abort) (alist-get id asyncloop-objects)
    (setq asyncloop-just-launched (remove id asyncloop-just-launched))
    (named-timer-cancel id)
    (setf :remainder nil)
    (and :on-abort (funcall :on-abort id))))

(cl-defun asyncloop-chomp (id &optional polite
                     &aux (obj (alist-get id asyncloop-objects)))
  "Call the next function in the asyncloop identified by ID.
Also pass ID onwards as an argument to this function to allow it
to manipulate the running asyncloop, via `asyncloop-remainder'
and `asyncloop-defuse'.

Finally, schedule another invocation of `asyncloop-chomp'.  Optional
argument POLITE ensures waiting for at least 1 second of idle
time before invoking.

Note that `asyncloop-chomp' must be called indirectly via
`named-timer-run'; to call it any other way will hopefully
trigger an error.  This mandate helps preserve the expected
behavior from the chain of timers, since `named-timer-run'
cancels any call that may have been pending, avoiding
double-calls."
  (with-slots (:remainder :last-idle-value :per-stage)
              (alist-get id asyncloop-objects)
    (setq asyncloop-just-launched (remove id asyncloop-just-launched))
    (and :per-stage (funcall :per-stage id))
    (when :remainder
      (let ((func (pop :remainder)))
        (condition-case err
            (progn
              ;; Ensure that the associated timer is inactive while this
              ;; executes, because if active, that indicates something else
              ;; called this, and we consider that usage pattern an error.
              (when (member (named-timer-get id) timer-list)
                (let ((errmsg
                       "Timer active during execution.  Possibly `asyncloop-chomp' was called directly."))
                  (asyncloop-echo id errmsg)
                  (asyncloop-defuse id)
                  (error errmsg)))
              (let* ((then (current-time))
                     (elapsed (progn
                                (funcall func id) ;; Real work happens here
                                (float-time (time-since then)))))
                (asyncloop-echo id "Finished in %.3fs: %S" elapsed func)
                (when (> elapsed 2)
                  ;; (asyncloop-defuse id)
                  (message "Function took more than 2 seconds: %S" func)))
              ;; Schedule the next step.
              ;; The :remainder may be empty for two reasons, either there was
              ;; no more to run, or the last function called asyncloop-defuse.
              (when :remainder
                (let ((idled-time (or (current-idle-time) 0)))
                  (if (or (time-less-p 1.0 idled-time)
                          (and (not polite)
                               (time-less-p :last-idle-value idled-time)))
                      ;; If user hasn't done any I/O since last chomp, proceed
                      ;; immediately to the next call.  If being impolite, do so
                      ;; even inside less than 1.0 second of idle, until there
                      ;; is user input, in which case give up and be polite.
                      (named-timer-run id 0.01 nil #'asyncloop-chomp id)
                    ;; Otherwise give Emacs a moment to respond to user input,
                    ;; and stay polite as long as input keeps happening.
                    (named-timer-idle-run id 1.0 nil #'asyncloop-chomp id 'politely))
                  (setf :last-idle-value idled-time))))
          ((error quit)
           (asyncloop-echo id "Asyncloop interrupted because: %s" err)
           ;; Restore state so we don't skip a function just b/c it failed
           (push func :remainder)
           (when (eq (car err) 'error)
             (setf :remainder nil)
             (error "Function %S failed: %s" func (cdr err)))))))))

;; (cl-defun asyncloop-run-wrapper (&rest args)
;;   "Batch calls from concomitant hooks for less log clutter."
;;   (named-timer-run (intern (concat "wrap-" (symbol-name (car (last args)))))
;;     .1 nil #'apply #'asyncloop-run args))

(defvar asyncloop-just-launched nil)

;;;###autoload
(cl-defun asyncloop-run
    (funs &key on-interrupt-discovered per-stage on-abort on-start
     (id (intern
          (concat "asyncloop-"
                  (number-to-string
                   (abs (sxhash (append (list per-stage on-abort on-start)
                                        funs))))))))
  "Attempt to run the series of functions in list FUNS.

Run them as a pseudo-asynchronous loop that pauses for user
activity to keep Emacs feeling snappy.

The loop as a whole is assigned an identifier based on uniqueness
of input, and refuses to run multiple instances with the same
identifier.  That means that if, perhaps accidentally, you call
this several times with identical input in a short timeframe,
only the first invocation is likely to do anything, and the rest
may no-op in favour of letting the already running asyncloop
finish.  You may optionally specify a custom identifier ID.

If the previous asyncloop (with the same identifier) seems to
have been interrupted and left in an incomplete state, call the
optional function ON-INTERRUPT-DISCOVERED, then resume the loop,
picking up where it was left off.

For each function in the asyncloop, call the optional function
PER-STAGE just before.

At the very start of a fresh loop, also call the optional function
ON-START.  This can be useful for preparing variables such that
PER-STAGE will work as you intend.

All the functions inside FUNS and those provided in the optional
arguments are passed one argument: the identifier for the loop.
See `asyncloop-remainder' and `asyncloop-defuse' for how that can
be used."
  (declare (indent defun))
  (unless (member id asyncloop-just-launched)
    (push id asyncloop-just-launched)
    (with-slots (:remainder :last-idle-value :funs)
        (or (alist-get id asyncloop-objects)
            (setf (alist-get id asyncloop-objects)
                  (asyncloop :funs funs
                             :on-abort on-abort
                             :per-stage per-stage)))
      (if (member (named-timer-get id) timer-list)
          (asyncloop-echo id "Already running, letting it continue: %S" id)
        (if (and :remainder (not (equal :remainder :funs)))
            ;; Something like a C-g must have interrupted execution.  Resume.
            (progn
              (and on-interrupt-discovered (funcall on-interrupt-discovered id))
              (when :remainder
                (asyncloop-echo id "Loop had been interrupted, resuming: %S" id)
                (setf :last-idle-value 0)
                (named-timer-idle-run id 1.0 nil #'asyncloop-chomp id 'politely)))
          ;; Start anew the full loop
          (asyncloop-echo id
            "Starting %S.  Left unexecuted last time: %S" id :remainder)
          (setf :remainder :funs)
          (setf :last-idle-value 0)
          (and on-start (funcall on-start id))
          (when :remainder
            (named-timer-run id 0.01 nil #'asyncloop-chomp id)))))))

(provide 'asyncloop)

;;; asyncloop.el ends here
