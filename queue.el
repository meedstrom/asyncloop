;;; queue.el --- non-blocking series of functions -*- lexical-binding: t -*-

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
;; Homepage: https://github.com/meedstrom/queue
;; Package-Requires: ((emacs "28.1") (named-timer "0.1"))

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'named-timer) ;; prevents bugs every day

(defvar queue-debug t
  "Whether to make available a buffer of debug messages.")

(defun queue-debug-buffer ()
  "Buffer to write debug messages to."
  (let ((bufname (concat (unless queue-debug " ") "*Queue.el debug*")))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname)
          (setq-local truncate-lines t)
          (setq-local buffer-read-only nil)
          (current-buffer)))))

;; FIXME: finish implmenting the id buffer
(defun queue-echo (id &rest args)
  "Log a message to the debug buffer associated with ID.
Arguments ARGS same as for `format'."
  (with-current-buffer (queue-debug-buffer)
    (goto-char (point-min))
    (insert (apply #'format
                   (cons (concat (format-time-string "%T: ")
                                 (car args))
                         (cdr args))))
    (newline)
    ;; pass also to caller, enabling e.g. (warn (queue-echo "..."))
    (apply #'format args)))

(defclass queue ()
  ((funs            :initarg :funs)
   (remainder       :initarg :remainder       :initform nil)
   (last-idle-value :initarg :last-idle-value :initform 0)
   (on-abort        :initarg :on-abort        :initform nil)
   (per-stage       :initarg :per-stage       :initform nil)))

(defvar queue-objects nil
  "Alist identifying unique queue objects.")

(defmacro queue-remainder (id)
  "Get the variable \"remainder\" from queue identified by ID.
The expression (queue-remainder ID) returns the list of functions
that remain to be executed for that queue.

If, as most likely, we're called from within a function that's
called by a running queue, you can schedule a new function
invocation to be run on the next \"tick\" like this:

(push #'some-additional-function (queue-remainder ID))

which can be useful for looping through a list by consuming it
one item at a time: have a function push itself back onto the
queue until the list is empty."
  `(slot-value (alist-get id queue-objects) :remainder))

(cl-defun queue-chomp
    (id &optional polite per-stage
        &aux (queue (alist-get id queue-objects)))
  "Call the next function in the queue identified by ID.
Also pass ID onwards as an argument to this function to allow it
to manipulate the running queue, via `queue-remainder'.

Finally, schedule another invocation of `queue-chomp'.  Optional
argument POLITE ensures waiting for at least 1 second of idle
time before invoking.

PER-STAGE same as in `queue-run'.

Note that `queue-chomp' must be called indirectly via
`named-timer-run'; to call it any other way will hopefully
trigger an error.  This mandate helps preserve the expected
behavior from the chain of timers, since `named-timer-run'
cancels any call that may have been pending, avoiding
double-calls."
  (with-slots (:remainder :last-idle-value :per-stage) queue
    (if (and :per-stage
             (eq 'please-abort (funcall :per-stage)))
        (queue-takedown id)
      ;; No abort requested, proceed
      (let ((func (pop :remainder)))
        (condition-case err
            (progn
              ;; Ensure that the associated timer is inactive while this executes,
              ;; because if active, that indicates something else called this,
              ;; and we consider that usage pattern an error.
              (when (member (named-timer-get id) timer-list)
                (error (queue-echo
                        (concat "Timer was not cancelled, possibly `queue-chomp'"
                                " was not called by `named-timer-run'."))))
              ;; TODO: Warn if time exceeds 2s. And implement dead man's switch of 2s.
              (let ((then (time-to-seconds))
                    ;; Real work happens here
                    (result (funcall func id)))
                (queue-echo "Ran in %ss: %s" (- (time-to-seconds) then) func))
              (if (eq result 'please-abort)
                  (queue-takedown id)
                ;; Schedule the next step.
                (when :remainder
                  (let ((idled-time (or (current-idle-time) 0)))
                    (if (or (time-less-p 1.0 idled-time)
                            (and (not polite)
                                 (time-less-p :last-idle-value idled-time)))
                        ;; If user hasn't done any I/O since last chomp, go go go.
                        ;; If being impolite, go even within 1.0 second timeframe
                        ;; until there is user input, in which case give up and be
                        ;; polite.
                        (named-timer-run id 0 nil #'queue-chomp id nil)
                      ;; Otherwise give Emacs a moment to respond to user input,
                      ;; and stay polite as long as input keeps happening.
                      (named-timer-idle-run id 1.0 nil #'queue-chomp id 'politely))
                    (setf :last-idle-value idled-time)))))
          ((error quit)
           (queue-echo "Queue interrupted because: %s" err)
           (push func :remainder) ;; recover state so we don't skip a function just b/c it failed
           (when (eq (car err) 'error)
             (error "Function %s failed: %s" func (cdr err)))))))))

;; REVIEW: maybe name it takedown or cancel
;; REVIEW: do we need to keep using a return value from PER-STAGE &
;; ON-INTERRUPT-DISCOVERED?  or can we make a somehow totally differnt workflow
;; where these don't need to return any signal but directly call the abort and
;; then somehow kill the parent caller? that's obviously a no but idk
(defun queue-takedown (id)
  (named-timer-cancel id)
  (let ((queue (alist-get id queue-objects)))
    (setf (slot-value queue :remainder) nil)
    (funcall (slot-value queue :on-abort))))

;;;###autoload
(cl-defun queue-run
    (funs &key on-interrupt-discovered on-start per-stage on-abort
          &aux (id (make-symbol
                    (concat "queue-" (number-to-string
                                      (abs (sxhash (append funs per-stage on-abort)))))))
               (queue (alist-get id queue-objects)))
  "Attempt to run the series of functions in list FUNS.

Run them as a pseudo-asynchronous queue that pauses for user
input to keep Emacs feeling snappy.

The queue as a whole is assigned an identifier based on
uniqueness of input, so calling this several times with identical
input may not re-start the queue, but no-op in favour of letting
the already running queue finish.

Each function is passed one argument, the identifier, see
`queue-remainder' for info on how that can be used.  The return
values are ignored.

All the optional keyword arguments accept one function each.
These functions' return values are ignored except that if they
return the symbol 'please-abort, the queue will be aborted and
the function ON-ABORT called.

If the previous queue seems to have been interrupted and left in
an incomplete state, call ON-INTERRUPT-DISCOVERED, then resume
executing the queue, picking it up where it was left off.

For each function in the queue, call PER-STAGE just before."
  (declare (indent defun))
  (unless queue
    (setq queue (queue :funs funs :on-abort on-abort :per-stage per-stage))
    (setf (alist-get id queue-objects) queue))
  (with-slots (:remainder :last-idle-value :funs) queue
    ;; TODO: implement again the bool :active -- with more clarity this time.
    ;; the (member timer-list) is unreliable, may as well not use.  Maybe the
    ;; bool should be called something else...  :has-a-timer or :is-scheduled...
    ;; it's sounding more like it should be a function.  But there's nothing I
    ;; know of that could be checked in realtime.  Ok no, the rock-solid check
    ;; is a dead-man's switch: just before each chomp, set :active true and
    ;; start a once-off timer of 2 seconds that will set :active to false.  That
    ;; way it's kept true the whole time that we're actually chomping, so if
    ;; it's become false we know it was interrupted somehow.
    (if (member (named-timer-get id) timer-list)
        (queue-echo "Already running queue, letting it continue: %s" id)
      (if (and :remainder
               (not (equal :remainder :funs)))
          ;; Something must have interrupted execution.  Resume.
          (if (or (not on-interrupt-discovered)
                  (and on-interrupt-discovered
                       (not (eq 'please-abort (funcall on-interrupt-discovered)))))
              ;; The on-interrupt-discovered didn't ask us to abort, or we had no such function
              (progn
                (queue-echo "Queue had been interrupted, resuming: %s" id)
                (setf :last-idle-value 0)
                (named-timer-run id 0 nil #'queue-chomp id nil))
            ;; Abort
            (queue-takedown id))
        ;; Start executing the full queue
        (queue-echo "Starting %s.  Left unexecuted last time: %s" id :remainder)
        (setf :remainder :funs)
        (setf :last-idle-value 0)
        (named-timer-run id 0 nil #'queue-chomp id nil)))))

(provide 'queue)
