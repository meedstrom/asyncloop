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
(require 'named-timer) ;; prevent bugs every day

(defvar queue-debug t
  "Whether to make available a buffer of debug messages.")

(defun queue--debug-buffer ()
  "Buffer to write debug messages to."
  (let ((bufname (concat (unless queue-debug " ") "*Queue.el debug*")))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname)
          (setq-local truncate-lines t)
          (setq-local buffer-read-only nil)
          (current-buffer)))))

(defun queue-echo (&rest args)
  "Write a message to the debug buffer.
Arguments ARGS same as for `format'."
  (with-current-buffer (queue--debug-buffer)
    (goto-char (point-min))
    (insert (apply #'format
                   (cons (concat (format-time-string "%T: ")
                                 (car args))
                         (cdr args))))
    (newline)
    ;; pass also to caller, enabling e.g. (warn (queue-echo "..."))
    (apply #'format args)))

(defclass queue ()
  ((active          :initarg :active          :initform nil)
   (remainder       :initarg :remainder       :initform nil)
   (last-idle-value :initarg :last-idle-value :initform 0)
   (funs            :initarg :funs)))

(defvar queue--list nil
  "Alist of all unique queue objects.")

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
  `(slot-value (alist-get id queue--list) :remainder))

(cl-defun queue--chomp
    (id &optional polite per-stage on-abort
        &aux (queue (alist-get id queue--list)))
  "Call the next function in the queue identified by ID.
Also pass ID on unmodified as an argument to this function so it
can manipulate the running queue if needed, primarily via
`queue-remainder'.  ID also identifies the associated timer via
`named-timer-get'.

Finally, schedule another invocation of `queue--chomp'.  Optional
argument POLITE ensures waiting for at least 1 second of idle
time before invoking.

PER-STAGE and ON-ABORT same as in `queue-run'."
  (with-slots (:active :remainder :last-idle-value) queue
    (if (and per-stage
             (eq 'please-abort (funcall per-stage)))
        ;; Abort whole queue
        (progn
          (setf :active nil)
          (setf :remainder nil)
          (funcall on-abort))
      (let ((func (pop :remainder)))
        (condition-case err
            (progn
              ;; In case something called this function twice and it wasn't the
              ;; timer that did it (if the timer ran the function, it won't be
              ;; among the active timers while this body is executing, so the
              ;; error isn't tripped).
              (when (member (named-timer-get id) timer-list)
                (error (queue-echo "Timer was not cancelled before `queue--chomp'")))
              (setf :active t)
              (queue-echo "Running: %s" func)
              (funcall func id) ;; Real work happens here
              ;; Schedule the next step.  Note to reader: draw a flowchart...
              (when :remainder
                (let ((idled-time (or (current-idle-time) 0)))
                  (if (or (and polite
                               (time-less-p 1.0 idled-time))
                          (and (not polite)
                               (time-less-p idled-time 1.0)
                               (time-less-p :last-idle-value idled-time)))
                      ;; If user hasn't done any I/O since last chomp, go go go.
                      ;; If being impolite, go even within 1.0 second timeframe
                      ;; until there is user input, in which case give up and be
                      ;; polite.
                      (named-timer-run id 0 nil #'queue--chomp id nil per-stage on-abort)
                    ;; Otherwise give Emacs a momen
                    ;; t to respond to user input,
                    ;; and stay polite as long as input keeps happening.
                    (named-timer-idle-run id 1.0 nil #'queue--chomp id 'politely per-stage on-abort))
                  (setf :last-idle-value idled-time))))
          ((error quit)
           (queue-echo "Queue interrupted because: %s" err)
           (push func :remainder) ;; recover so we don't skip a function just bc it failed
           (when (eq (car err) 'error)
             (setf :active nil)
             (error "Function %s failed: %s" func (cdr err))))))
      ;; The queue finished.  By nulling :active we'll know it wasn't just
      ;; a random keyboard-quit.
      ;; REVIEW: Actually the fact :remainder is empty should be sufficient.
      (unless :remainder
        (setf :active nil)))))

;; TODO: Maybe use a function `queue-stop' instead of a return symbol
;; 'please-abort.  This would work inside the queue proper as well.
;;;###autoload
(cl-defun queue-run
    (funs &key on-interrupt-discovered on-start per-stage on-abort
          &aux (id (make-symbol (concat "queue-" (number-to-string (abs (sxhash funs))))))
               (queue (alist-get id queue--list)))
  "Attempt to run the series of functions in list FUNS.

Run them as a pseudo-asynchronous queue that pauses for user
input to keep Emacs feeling snappy.

The queue as a whole is assigned an identifier based on
uniqueness of FUNS, so calling this several times with the same
FUNS may not re-start the queue, but no-op in favour of letting
the already running queue finish.  Beware that calling this with
a slightly different FUNS results in running an entirely separate
\"thread\".

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

On (re-)starting a queue, call ON-START before starting.

For each function in the queue, call PER-STAGE just before."
  (declare (indent defun))
  (unless queue
    (setq queue (queue :funs funs))
    (push (cons id queue) queue--list))
  (with-slots (:active :remainder :last-idle-value :funs) queue
    ;; TODO: rewrite with cond to make it easier to read
    (if :active
        ;; Q: can we end up in a state where timer is inactive? A: yes, it's the
        ;; normal case for queue--chomp because it's often called by a timer, which
        ;; takes itself off the timer-list prior to calling.  However, we don't
        ;; ever call queue-run with (the package-internal) timer, so
        ;; (named-timer-get name) being in timer-list basically means a state is
        ;; active, though the opposite doesn't always mean it's not.
        (progn
          (when (not (member (named-timer-get id) timer-list))
            (error (queue-echo "No timer, but `:active' still t")))
          (if :remainder
              (queue-echo "Already active queue, letting it continue")
            ;; TODO: is it just during debugging we end up in this state? No.
            (error (queue-echo "Queue empty but `:active' still t"))))
      (if (and (not :active)
               :remainder
               (not (equal :remainder :funs)))
          ;; Something must have interrupted execution.  Resume.
          (if (or (not on-interrupt-discovered)
                  (and on-interrupt-discovered
                       (not (eq 'please-abort (funcall on-interrupt-discovered)))))
              ;; The on-interrupt-discovered function returned OK, or we didn't have such a function to run
              (progn
                (queue-echo "Queue had been interrupted, resuming")
                (named-timer-run id 0 nil #'queue--chomp id nil per-stage on-abort)))
            (named-timer-cancel id)
            (setf :active nil)
            (setf :remainder nil)
            (funcall on-abort))
        (if (or (not on-start)
                (and on-start
                     (not (eq 'please-abort (funcall on-start)))))
            ;; The on-start function didn't ask us to abort, or we didn't have an on-start function to run
            (progn
              (queue-echo "Launching new queue.  Unexecuted from last queue: %s" :remainder)
              (named-timer-cancel id)
              (setf :remainder :funs)
              (setf :last-idle-value 1.0) ;; HACK so `queue--chomp' won't wait
              (named-timer-run id 0 nil #'queue--chomp id nil per-stage on-abort))
          (named-timer-cancel id)
          (setf :active nil)
          (setf :remainder nil)
          (funcall on-abort)))))

(provide 'queue)
