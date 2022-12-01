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

;;; Code:

(require 'cl-lib)
(require 'named-timer) ;; prevents bugs every day

(defvar asyncloop-debug t
  "Whether to reveal buffers of debug messages.")

(defun asyncloop-debug-buffer (loop)
  "Get/create a debug buffer associated with LOOP."
  (let ((bufname (concat (unless asyncloop-debug " ") "*"
                         (symbol-name (asyncloop-id loop)) "*")))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname)
          (set (make-local-variable 'window-point-insertion-type) t)
          (setq-local truncate-lines t)
          (setq-local buffer-read-only nil)
          (current-buffer)))))

(defun asyncloop-log (loop &rest args)
  "Log a message to the debug buffer associated with LOOP.
Arguments ARGS same as for `format'."
  (declare (indent 1))
  (with-current-buffer (asyncloop-debug-buffer loop)
    (goto-char (point-max))
    (insert (apply #'format
                   (cons (concat (format-time-string "%T: ")
                                 (car args))
                         (cdr args))))
    (newline)
    ;; Pass the string back to caller so it can be used to also signal
    ;; something, as in (error (asyncloop-log loop "..."))
    (apply #'format args)))

(defvar asyncloop-objects nil
  "Alist identifying unique asyncloop objects.")

(cl-defstruct (asyncloop (:constructor asyncloop-create)
                         (:copier nil))
  id
  funs
  (remainder nil)
  (last-idle-value 0)
  (just-launched nil)
  (on-cancel nil)
  (per-stage nil)
  starttime)

(defmacro asyncloop-with-slots (slots obj &rest body)
  "Like `with-slots' but for a struct rather than an eieio class.
In similar fashion as `map-let', bind slot names SLOTS within the
asyncloop object OBJ, and execute BODY.  Unlike `map-let', allow
modifying these slots directly with `setf', `push' etc."
  (declare (indent 2))
  `(cl-symbol-macrolet
       ,(cl-loop
         for slot in slots
         collect `(,slot (,(intern (concat "asyncloop-" (symbol-name slot)))
                          ,obj)))
     ,@body))

(defun asyncloop-reset-all ()
  (interactive)
  (setq asyncloop-objects nil))

(defun asyncloop-cancel (loop)
  "Stop continuation of asyncloop identified by ID, and ensure
it'll be restarted fresh on the next `asyncloop-run'.  Finally,
call the associated on-cancel function if it exists."
  (asyncloop-with-slots (id remainder on-cancel just-launched) loop
    (setf just-launched nil)
    (named-timer-cancel id)
    (setf remainder nil)
    (and on-cancel (funcall on-cancel loop))))

(defvar asyncloop-debug-level 1
  "Verbosity of debug.")

;; TODO: Improve performance.  To run ~10 calls per second, there seems to be
;; about 0.5 seconds lost, i.e. one run of this boilerplate takes a full 0.050
;; secs, which adds up ... in fact doubling how long Deianira takes to finish.
;; Solutions:
;; - run the profiler and launch deianira-mode
;;   - OK it said 80% of CPU time was in funcall and 14% in GC.  That's great.  Maybe interpreted perf is worse, or the profiler misses something...
;; - reimplement named-timer (it does some checks we don't need)
;; - 
(defun asyncloop-chomp (loop &optional polite)
  "Call the next function in the asyncloop LOOP.
Then schedule another invocation of `asyncloop-chomp'.  Optional
argument POLITE ensures waiting for at least 1 second of idle
time before invoking.

Note that `asyncloop-chomp' must be called indirectly via
`named-timer-run'; to call it any other way will hopefully
trigger an error.  This mandate helps preserve the expected
behavior from the chain of timers, since `named-timer-run'
cancels any call that may have been pending, avoiding
double-calls."
  (asyncloop-with-slots (id remainder last-idle-value per-stage just-launched starttime) loop
    (setf just-launched nil)
    (when per-stage
      (funcall per-stage loop))
    (when remainder
      (let ((func (pop remainder)))
        (condition-case err
            (progn
              (when (> asyncloop-debug-level 1)
                ;; Ensure that the associated timer is inactive while this
                ;; executes, because if active, that indicates something else
                ;; called this, and we consider that usage pattern an error.
                (when (or (member (named-timer-get id) timer-list)
                          (member (named-timer-get id) timer-idle-list))
                  (asyncloop-cancel loop)
                  (error (asyncloop-log loop
                           (concat "Timer active during execution.  Possibly"
                                   " `asyncloop-chomp' was called directly.")))))
              (let ((then (current-time)))
                (funcall func loop) ;; Real work happens here
                (asyncloop-log loop
                  "Finished in %.3fs: %S" (float-time (time-since then)) func))
              ;; Schedule the next step.
              ;; The remainder may be empty for two reasons, either there was
              ;; no more to run, or the last function called asyncloop-cancel.
              (if remainder
                  ;; TODO: Improve wall-time of whole loop.  Things just got
                  ;; much worse now that I made it stay polite.  Probably should
                  ;; go back to aggressive after 1 sec of idle.  And then
                  ;; there's no need to have a "polite" argument since it never
                  ;; makes a difference, I think.
                  (let ((idled-time (or (current-idle-time) 0)))
                    (if (or (time-less-p 1.0 idled-time)
                            (and (not polite)
                                 (time-less-p last-idle-value idled-time)))
                        ;; If user hasn't done any I/O since last chomp, proceed
                        ;; immediately to the next call.  If being impolite (which
                        ;; is the case on every new launch from `asyncloop-run'),
                        ;; do so even within less than 1.0 second of idle, until
                        ;; there is user activity.
                        (named-timer-run id 0 nil #'asyncloop-chomp loop polite)
                      ;; If user has done I/O, give Emacs a moment to respond to
                      ;; user input, and stay polite for the rest of the loop.
                      (named-timer-idle-run id 1.0 nil #'asyncloop-chomp loop 'politely))
                    (setf last-idle-value idled-time))
                (asyncloop-log loop
                  "Loop finished (or cancelled) in wall time of %.3fs"
                  (float-time (time-since starttime)))))

          ((error quit)
           (asyncloop-log loop "Asyncloop interrupted because: %s" err)
           ;; Restore state so we don't skip a function just b/c it failed
           (push func remainder)
           (when (eq (car err) 'error)
             (setf remainder nil)
             (error "Function %S failed: %s" func (cdr err))))
          (t
           (asyncloop-log loop "Asyncloop interrupted because: %s" err)))))))

;;;###autoload
(cl-defun asyncloop-run
    (funs &key on-interrupt-discovered per-stage on-start on-cancel id)
  "Attempt to run the series of functions in list FUNS.

Run them as a pseudo-asynchronous loop that pauses for user
activity to keep Emacs feeling snappy.

The loop as a whole is assigned an identifier based on uniqueness
of input, and refuses to run multiple instances with the same
identifier.  That means that if, perhaps accidentally, you call
this several times with identical input in a short timeframe,
only the first invocation is likely to do anything, and the rest
will no-op in favor of letting the already running asyncloop
finish.

Though you'll probably not need it, you can supply a custom
symbol ID, which predetermines the name of the associated debug
buffer: \"*ID*\", and the timer in `named-timer-table'.

If the previous loop with the same identifier seems to have been
interrupted and left in an incomplete state, call the optional
function ON-INTERRUPT-DISCOVERED, then resume the loop, picking
up where it was left off.  To reiterate, this doesn't happen
exactly when the interrupt happens, only the next time
`asyncloop-run' itself is triggered.

\(Tip: It's not unlikely to get spurious interrupts because the
user types C-g at inopportune times, but it's also possible it's
intentional, and we must always respect a C-g.  If you're having
problems with insistent restarts, you could set
ON-INTERRUPT-DISCOVERED to `asyncloop-cancel' to get some
breathing room and watch the loop restart in full for debugging.
The problem is likely appropriately solved with a sanity check in
PER-STAGE or elsewhere.\)

For each function in FUNS, call the optional function PER-STAGE
just before.

At the very start of a fresh loop, also call the optional function
ON-START.  This can be useful for preparing variables such that
PER-STAGE will work as you intend on the first time.

All the functions inside FUNS and those provided in the optional
arguments are passed one argument: the loop object, which holds
all of this metadata.  This lets you inspect and manipulate the
running loop by passing the object to any of:

- `asyncloop-cancel'
- `asyncloop-remainder'
- `asyncloop-log'

If `asyncloop-cancel' is called by any of these functions, it
will also call the optional function ON-CANCEL.

To have a function in FUNS repeat itself until some condition is
met \(in the style of a while-loop\), have it push itself onto the
result of `asyncloop-remainder'.  As always with while-loop
patterns, take a moment to ensure that there's no way it will
repeat forever.  If it's meant to decrement a counter by
`cl-decf' or consume a list one item at a time by `pop', consider
doing that before anything else in the function body."
  (declare (indent defun))
  (let* ((id (or id (intern (concat "asyncloop-"
                                    (number-to-string
                                     (abs (sxhash (list funs
                                                        on-interrupt-discovered
                                                        per-stage
                                                        on-start
                                                        on-cancel))))))))
         (loop (or (alist-get id asyncloop-objects)
                   (setf (alist-get id asyncloop-objects)
                         (asyncloop-create
                          :id id
                          :funs funs
                          :per-stage per-stage
                          :on-cancel on-cancel)))))
    (asyncloop-with-slots (remainder last-idle-value funs just-launched starttime) loop
      ;; Ensure that being triggered by several concomitant hooks won't spam
      ;; the debug buffer, or worse, start multiple loops (somehow happens
      ;; -- maybe if a timer is at 0 seconds, it can't be cancelled?)
      (unless just-launched
        (setf just-launched t)
        (if (or (member (named-timer-get id) timer-list)
                (member (named-timer-get id) timer-idle-list))
            (asyncloop-log loop "Already running, letting it continue")
          (if (and remainder (not (equal remainder funs)))
              ;; Something like a C-g must have interrupted execution.  Resume.
              (progn
                (when on-interrupt-discovered
                  (funcall on-interrupt-discovered loop))
                (when remainder
                  (asyncloop-log loop "Loop had been interrupted, resuming")
                  (setf last-idle-value 0)
                  (named-timer-run id 0 nil #'asyncloop-chomp loop)))
            ;; Start anew the full loop
            (asyncloop-log loop "Launching anew")
            (setf remainder funs)
            (setf last-idle-value 0)
            (setf starttime (current-time))
            (when on-start
              (funcall on-start loop))
            (when remainder
              (named-timer-run id 0 nil #'asyncloop-chomp loop))))))))

(provide 'asyncloop)

;;; asyncloop.el ends here
