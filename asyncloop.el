;;; asyncloop.el --- Non-blocking series of functions -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Martin Edström

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

;; Author:  <meedstrom91@gmail.com>
;; Created: 2022-10-30
;; Version: 0.4.2-snapshot
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/asyncloop
;; Package-Requires: ((emacs "28"))

;;; Commentary:

;; Use `asyncloop-run' to call a series of functions without hanging Emacs.

;;; Code:

(require 'named-timer)
(require 'cl-lib)
(require 'subr-x)

(defun asyncloop-reset-all ()
  "Cancel all asyncloops and wipe `asyncloop-objects'."
  (interactive)
  (ignore-errors
    (cl-loop for (_id . loop) in asyncloop-objects
             do
             (asyncloop-log loop "All loops reset")
             (asyncloop-cancel loop)))
  (setq asyncloop-objects nil)
  (if (derived-mode-p 'asyncloop-debug-buffer-mode)
      (message "All asyncloops reset due to quit in buffer %s" (buffer-name))
    (message "All asyncloops reset")))

(defun asyncloop-keyboard-quit ()
  "Wrapper for `keyboard-quit' that also cancels all loops."
  (interactive)
  (unwind-protect
      (asyncloop-reset-all)
    (keyboard-quit)))

(defvar asyncloop-debug-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] #'asyncloop-keyboard-quit)
    map))

(define-derived-mode asyncloop-debug-buffer-mode special-mode
  "Asyncloop-Debug"
  (setq truncate-lines t)
  (setq buffer-read-only nil)
  (set (make-local-variable 'window-point-insertion-type) t))

(defun asyncloop-log (loop &rest args)
  "Log a message to the debug buffer associated with LOOP.
Arguments ARGS are the arguments for `format'.

Finally, return the log line as a string so you can pass it on to
`warn' or `error' or the like."
  (declare (indent 1))
  (let ((buf (asyncloop-debug-buffer loop))
        (text (apply #'format args)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format-time-string "%T: ") text)
        (newline))
      ;; Improve DX.  Some emacsen look choppy when you watch the log buffer
      ;; because they don't redisplay on every insertion, instead batching
      ;; updates in chunks of 10-20 lines.  This reduces that choppiness.
      (when (get-buffer-window buf)
        (redisplay)))
    ;; Allow the convenient sexp (warn "%s" (asyncloop-log loop "msg"))
    text))

(defun asyncloop-clock-funcall (loop fn)
  "Run FN; log result and time elapsed to LOOP's debug buffer.
Return nil if the command timed out, and return the log message
otherwise."
  (with-timeout
      (5 (asyncloop-cancel loop)
         (message "%s %s"
                  (asyncloop-log loop
                    "Canceling: a step took longer than 5 sec!")
                  ;; Let end users know which package is responsible
                  (if-let ((buf (asyncloop-debug-buffer loop)))
                      (format "See buffer %s" (buffer-name buf))
                    (format "%s called by asyncloop object %s" fn loop)))
         nil)
    (let ((fn-name (if (symbolp fn) fn "lambda"))
          (then (current-time))
          (result (funcall fn loop)))
      (asyncloop-log loop
        "Took %.2fs: %s: %s" (float-time (time-since then)) fn-name result))))

(cl-defstruct (asyncloop (:constructor asyncloop-create)
                         (:copier nil))
  funs
  starttime
  timer
  debug-buffer
  immediate-break-on-user-activity
  (paused nil)
  (remainder nil)
  (scheduled nil)
  (just-launched nil))

(defmacro asyncloop-with-slots (spec-list object &rest body)
  "Like eieio's `with-slots', but for our struct type.
Bind SPEC-LIST lexically to slot values in OBJECT, and execute BODY."
  (declare (indent 2))
  `(cl-symbol-macrolet
       ,(cl-loop
         for slot in spec-list
         collect `(,slot (,(intern (concat "asyncloop-" (symbol-name slot)))
                          ,object)))
     ,@body))

(defvar asyncloop-objects nil
  "Alist identifying unique asyncloop objects.
Expected format:
   ((ID1 . OBJECT1)
    (ID2 . OBJECT2)
    ...)")

(defun asyncloop-cancel (loop)
  "Stop asyncloop LOOP from executing more queued functions.
Ensure the loop will restart fresh on the next call to
`asyncloop-run'."
  (setf (asyncloop-remainder loop) nil)
  (asyncloop-log loop "Loop told to cancel"))

(defvar asyncloop-recursion-ctr 0
  "How deeply `asyncloop-chomp' has recursed.")

(defun asyncloop-scheduled-loops ()
  "Check if some loop is scheduled to run.
Return an alist associating active loop IDs (as car) with the
time they have left on their timers, in seconds (as cdr).  The
alist is sorted by soonest first.  If no loops are scheduled,
return nil."
  (cl-loop with result = nil
           for (id . loop) in asyncloop-objects
           for some-timer = (cl-find (named-timer-get (asyncloop-timer loop))
                                     timer-idle-list)
           when some-timer
           collect (cons id (- (timer-until some-timer nil)))
           into result
           finally return (cl-sort result #'< :key #'cdr)))
;; (asyncloop-scheduled-loops)

(defun asyncloop-chomp (loop)
  (asyncloop-with-slots (remainder starttime scheduled timer paused) loop
    ;; REVIEW: It may be more intuitive if we pop remainder and just push the
    ;; fn back on in case of an error or interruption, via `condition-case'.
    ;; Then when the programmer inspects `asyncloop-remainder', they will see
    ;; only the next functions to run and not also the function being run.  But
    ;; I don't know what's more useful.  One problem with popping only after
    ;; success, as we do here, is that it seems more difficult to reason about
    ;; if the programmer wants to manipulate `asyncloop-remainder' in a more
    ;; complex way.
    (when (asyncloop-clock-funcall loop (car remainder))
      (pop remainder)
      (cond
       ((null remainder)
        (let ((elapsed (float-time (time-since starttime))))
          (asyncloop-log loop "Loop ended, total wall-time %.2fs" elapsed)))
       (paused
        (asyncloop-log loop "Loop paused"))
       ;; (This clause only useful if :immediate-break-on-user-activity nil.
       ;; If t, we never arrive here anyway.)  User just did I/O, so free a
       ;; moment for Emacs to respond to it.  Because of the idle timer, this
       ;; moment may be extended indefinitely.
       ((input-pending-p)
        (setf scheduled t)
        (named-timer-idle-run timer 1.0 nil #'asyncloop-resume-1 loop))
       ;; round and round we go
       ((> 100 (cl-incf asyncloop-recursion-ctr))
        (asyncloop-chomp loop))
       ;; Every 100 calls, prune the call stack to minimize the risk of
       ;; tripping `max-lisp-eval-depth'.
       (t
        (setq asyncloop-recursion-ctr 0)
        (asyncloop-schedule-immediately loop)))))
  ;; NOTE: The return value is meaningful to `while-no-input'!
  nil)

(defun asyncloop-pause (loop)
  "Pause LOOP.
It can later be resumed with `asyncloop-resume'.

Tip: by saving the output of `asyncloop-run' to a variable, you
have a pointer to the loop object which you can pass to this
function."
  (asyncloop-with-slots (paused timer) loop
    (asyncloop-log loop "Loop told to pause")
    (setf paused t)
    (named-timer-cancel timer)))

;; TODO: This docstring reminded me of something. it'd be nice to offer the
;; option to set `inhibit-quit' nil, or to not use any timers at all (not even
;; idle timer) in favour of putting an internal function directly on various
;; hooks.  That has the advantage of not demoting errors to mere messages.
(defun asyncloop-resume (loop)
  "Set asyncloop LOOP to resume when Emacs has a moment.
In other words, actually places `asyncloop-resume-1' on a short
timer, which has the effect of deferring it until Emacs finishes
executing the current call stack.

A secondary effect is that by exposing this wrapper to end users
instead of `asyncloop-resume-1' itself, we ensure that the latter
is always called by a timer and not directly, making behavior
consistent.  For example, timers behave different from direct
funcalls in that they always set `inhibit-quit' t."
  (asyncloop-with-slots (paused scheduled timer) loop
    (asyncloop-log loop "Loop told to resume")
    (setf paused nil)
    (asyncloop-schedule-immediately loop)))

(defun asyncloop-schedule-immediately (loop)
  "Pass LOOP to `asyncloop-resume-1' after a very short timer.
If there's another loop to go first, log that, so when someone
watches a debug buffer they don't conclude that it got stuck.
Especially relevant because `asyncloop-recursion-ctr' maxes at
100, which means the work will oscillate, 100 calls for one loop
and then 100 calls for the other and back."
  (setf (asyncloop-scheduled loop) t)
  (when-let ((soonest (car-safe (asyncloop-scheduled-loops))))
    (when (> .01 (cdr soonest))
      (asyncloop-log loop "Another loop at work, waiting...")))
  (named-timer-idle-run (asyncloop-timer loop) .01 nil #'asyncloop-resume-1 loop))

(defun asyncloop-resume-1 (loop)
  "Resume executing asyncloop LOOP.
When user activity happens, abort and schedule another
invocation of `asyncloop-resume-1' on an idle timer."
  (asyncloop-with-slots (remainder just-launched paused scheduled immediate-break-on-user-activity timer) loop
    (setf just-launched nil)
    (setf paused nil)
    (setf scheduled nil)
    (if remainder
        (if immediate-break-on-user-activity
            (let ((result (while-no-input (asyncloop-chomp loop))))
              (cond
               ((eq t result)
                ;; User just did I/O, so free a moment for Emacs to
                ;; respond to it.  Because it's on an idle timer, this
                ;; moment may be extended indefinitely.
                (setf scheduled t)
                (named-timer-idle-run timer 1.0 nil #'asyncloop-resume-1 loop))
               ((not (null result))
                (error "Asyncloop gave an unexpected result: %s" result))))
          ;; Considered adding `with-local-quit' here.  Not sane since we expect
          ;; most functions to complete in a short time, so it would mainly be a
          ;; source of spurious bugs because C-g happened to hit at just the wrong
          ;; time.  Instead, to counter the case of a long-running or hung function,
          ;; I put a timeout inside `asyncloop-clock-funcall'.
          (asyncloop-chomp loop))
      (asyncloop-log loop "Scheduled loop was cleared, doing nothing"))))

;;;###autoload
(cl-defun asyncloop-run
    (funs &key
          immediate-break-on-user-activity
          immediate-break-on-input
          on-interrupt-discovered
          debug-buffer-name)
  "Attempt to run the series of functions in list FUNS.
Execute them in sequence, but pause on user activity to keep
Emacs feeling snappy.

Optional argument IMMEDIATE-BREAK-ON-USER-ACTIVITY says to use
`while-no-input' internally, which means that user activity can
interrupt the loop at exactly any point during execution.  The
gotcha is that you need to be more careful writing each function
in FUNS so that interruption midway won't leave your program in
an unintended state.

An interrupted function remains on schedule to run again, unless
you've manipulated the loop (more on this further below).

As a tip, you could do all heavy calculations inside some `let'
bindings, then carry out side effects all together via simple
`setq', `push', `pop' calls, which are practically instant.

If instead, IMMEDIATE-BREAK-ON-USER-ACTIVITY is left at nil, you
have fewer things to worry about as any given function will be
allowed to complete before Emacs responds to user activity.  The
drawback is that you may annoy the user if that function takes a
perceptible amount of time to complete, like 0.02s or longer.

The loop as a whole refuses to start twice with the same input if
the last invocation has not yet completed.  That means that if,
perhaps via a hook that triggers often, you call this several
times in a short timeframe, only the first invocation is likely
to do anything, and the rest will no-op in favor of letting the
already running loop finish.

If the previous invocation with the same input seems to have been
interrupted by an error and left in an incomplete state, first
call the optional function ON-INTERRUPT-DISCOVERED, then resume
the loop, picking up where it was left off.  To reiterate, this
recovery will not happen at the moment the loop is interrupted,
only the next time you execute `asyncloop-run' itself (which
could be a long while or never, depending on what hook you put it
on).

By default, ON-INTERRUPT-DISCOVERED is nil, but another
reasonable value is #\\='asyncloop-cancel.  If you are having
problems with insistent restarts, that lets you watch the loop
restart in full, which should help you debug what is going on.
The problem is likely appropriately solved with a sanity check at
the start of most/all functions in FUNS.

All the functions in FUNS, as well as the optional
ON-INTERRUPT-DISCOVERED function, are passed one argument: the
loop object, which holds metadata about it.  This lets those
functions inspect and manipulate the running loop by passing the
object on to any of the functions:

- `asyncloop-pause'
- `asyncloop-resume'
- `asyncloop-cancel'
- `asyncloop-remainder'
- `asyncloop-log'

To have a function in FUNS repeat itself until some condition is
met (in the style of a while-loop), have it push itself onto the
result of `asyncloop-remainder', effectively shoving itself back
onto the front of the queue of things-to-execute:

  (unless ...some-condition-that-means-done...
    (push #\\='this-function (asyncloop-remainder loop)))

As always with while-loop patterns, take a moment to ensure that
there is no way it will repeat forever.  If it is meant to
decrement a counter by `cl-decf' or consume a list one item at a
time by `pop', do that earlier than the above form.

Optional string DEBUG-BUFFER-NAME controls whether to create a
buffer of debug messages, and if so, its name.

It does not matter what the functions in FUNS return, but the
debug buffer prints the return values, so by returning something
interesting (I suggest a short string constructed by `format'),
you can improve your debugging experience."
  (declare (indent defun))
  (cl-assert funs)
  ;; TODO: why this assertion errors in normal use?
  ;; (dolist (fun funs)
  ;;   (cl-assert (functionp fun)))

  ;; Name it as a deterministic hash of the input, ensuring that
  ;; next call with the same input can recognize that it was
  ;; already called.
  (let* ((id (abs (sxhash (list funs on-interrupt-discovered debug-buffer-name immediate-break-on-user-activity))))
         (loop
          (or (alist-get id asyncloop-objects)
              (setf (alist-get id asyncloop-objects)
                    (asyncloop-create
                     :funs funs
                     :timer (intern (format "a%d" id))
                     :immediate-break-on-user-activity
                     (or immediate-break-on-user-activity
                         immediate-break-on-input)
                     :debug-buffer
                     (when debug-buffer-name
                       (with-current-buffer (get-buffer-create debug-buffer-name)
                         (asyncloop-debug-buffer-mode)
                         (current-buffer))))))))
    (asyncloop-with-slots (remainder scheduled just-launched starttime timer paused) loop
      ;; Ensure that being triggered by several concomitant hooks won't spam the
      ;; debug buffer, or worse, start multiple loops (this somehow happened in
      ;; the past -- maybe if a timer is at 0 seconds, it can't be cancelled?).
      (unless just-launched
        (setf just-launched t)
        (cond
         (scheduled
          (setf just-launched nil)
          (asyncloop-log loop "Already running, letting it continue"))

         ;; Resuming an apparently half-done thing
         ((and remainder (not (equal remainder funs)))
          (if paused
              (progn
                (asyncloop-log loop
                  "Resuming paused loop.  Functions left to run: %S"
                  remainder)
                (asyncloop-schedule-immediately loop))
            ;; TODO: no need to check for timeout, typically `asyncloop-run'
            ;; won't be run in a context where `inhibit-quit' is set
            (let ((timed-out
                   (when on-interrupt-discovered
                     (null (asyncloop-clock-funcall
                            loop on-interrupt-discovered)))))
              (unless timed-out
                (if (null remainder)
                    (asyncloop-log loop "Cancelled by ON-INTERRUPT-DISCOVERED")
                  (asyncloop-log loop
                    "Loop had been interrupted, resuming.  Functions left to run: %S"
                    remainder)
                  (asyncloop-schedule-immediately loop))))))

         (t
          ;; Launch anew the full loop
          (setf remainder funs)
          (setf starttime (current-time))
          (asyncloop-log loop "Loop started")
          (asyncloop-schedule-immediately loop)))))
    loop))

(set-advertised-calling-convention
 'asyncloop-run
 '(funs &key
   immediate-break-on-user-activity
   on-interrupt-discovered
   debug-buffer-name) "2023-11-13")

;;;###autoload
(define-obsolete-function-alias 'asyncloop-run-function-queue #'asyncloop-run
  "2023-11-09"
  "Changed my mind.")

(provide 'asyncloop)

;;; asyncloop.el ends here
