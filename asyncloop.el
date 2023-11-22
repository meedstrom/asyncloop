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

;; Author: Martin Edström <meedstrom91@gmail.com>
;; Created: 2022-10-30
;; Version: 0.4.6-snapshot
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/asyncloop
;; Package-Requires: ((emacs "28") (named-timer "0.1"))

;;; Commentary:

;; Use `asyncloop-run' to call a series of functions while keeping Emacs snappy
;; and responsive to user activity.
;;
;; See the docstring of that function.  It is long, but worthwhile.

;;; Code:

(require 'named-timer)
(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (asyncloop (:constructor asyncloop-create)
                         (:copier nil))
  funs
  starttime
  timer-id
  log-buffer
  immediate-break-on-user-activity
  id
  (paused nil)
  (remainder nil)
  (scheduled nil)
  (just-launched nil))

(defvar asyncloop-objects nil
  "Alist identifying unique asyncloop objects.
Expected format:
   ((ID1 . OBJECT1)
    (ID2 . OBJECT2)
    ...)")

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

(defun asyncloop-log (loop &rest args)
  "Log a message to the log buffer associated with LOOP.
Arguments ARGS are the arguments for `format'.

Finally, return the formatted string so you can pass it on to
`warn' or `error' or the like."
  (declare (indent 1))
  (let ((buf (asyncloop-log-buffer loop))
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

(defun asyncloop-reset-all ()
  "Cancel all asyncloops and wipe `asyncloop-objects'."
  (interactive)
  (ignore-errors
    (cl-loop for (_id . loop) in asyncloop-objects
             do
             (asyncloop-cancel loop)
             (asyncloop-log loop "All loops reset")))
  (setq asyncloop-objects nil)
  (if (derived-mode-p 'asyncloop-log-mode)
      (message "All asyncloops reset due to quit in buffer %s" (buffer-name))
    (message "All asyncloops reset")))

(defun asyncloop-keyboard-quit ()
  "Wrapper for `keyboard-quit' that also cancels all loops.
Only meant to be bound in asyncloop log buffers."
  (interactive)
  (unwind-protect
      (asyncloop-reset-all)
    (keyboard-quit)))

(defvar asyncloop-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] #'asyncloop-keyboard-quit)
    map))

(define-derived-mode asyncloop-log-mode special-mode
  "Asyncloop-Log"
  (setq truncate-lines t)
  (setq buffer-read-only nil)
  (set (make-local-variable 'window-point-insertion-type) t))

(defun asyncloop-clock-funcall (loop fn)
  "Run FN; log result and time elapsed to LOOP's log buffer."
  (let* ((fn-name (if (symbolp fn) fn 'lambda))
         (then (current-time))
         (result (condition-case err
                     (funcall fn loop)
                   ((t debug)
                    (asyncloop-log loop "During %S: %S" fn-name err)
                    (signal (car err) (cdr err))))))
    (asyncloop-log loop
      "Took %.2fs: %S: %s" (float-time (time-since then)) fn-name result)))

(defun asyncloop-cancel (loop &optional quietly)
  "Stop asyncloop LOOP from executing more queued functions.
Ensure the loop will restart fresh on the next call to
`asyncloop-run'.

With optional argument QUIETLY, don't log the cancellation."
  (asyncloop-with-slots (remainder paused scheduled just-launched) loop
    (unless quietly
      (asyncloop-log loop "Loop told to cancel"))
    (setf remainder nil)
    (setf paused nil)
    (setf just-launched nil)
    ;; (setf scheduled nil)
    ;; (named-timer-cancel timer-id)
    ))

(defun asyncloop-pause (loop)
  "Pause LOOP.
Tip: by saving the output of `asyncloop-run' to a variable, you
have a pointer to the loop object, which you can pass to this
function and then later to `asyncloop-resume'."
  (asyncloop-with-slots (paused scheduled timer-id) loop
    (asyncloop-log loop "Loop told to pause")
    (setf paused t)
    (setf just-launched nil)
    (setf scheduled nil)
    (named-timer-cancel timer-id)))

(defun asyncloop-notify-simultaneity (this-loop)
  "Write in all loops' logs that multiple loops are active.
This is done so when someone watches one of the logs, they don't
conclude that it got stuck because it seems to do no work."
  (cl-loop
   with others = nil
   for (_ . loop) in asyncloop-objects
   when (member (named-timer-get (asyncloop-timer-id loop))
                (append timer-idle-list timer-list))
   collect loop into others
   finally do (when others
                (dolist (loop (cons this-loop others))
                  (asyncloop-log loop
                    "Two or more asyncloops running, please wait...")))))

(defvar asyncloop-recursion-ctr 0
  "How deeply `asyncloop-chomp' has recursed.")

(defun asyncloop-chomp (loop)
  "Run functions that remain in LOOP, one at a time."
  (asyncloop-with-slots (remainder starttime scheduled timer-id paused immediate-break-on-user-activity) loop
    ;; In case the last function pushed t on REMAINDER and then got interrupted
    (when (not (functionp (car remainder)))
      (pop remainder))
    (asyncloop-clock-funcall loop (car remainder))
    (pop remainder)
    (cond
     ((null remainder)
      (let ((elapsed (float-time (time-since starttime))))
        (asyncloop-log loop "Loop ended, total wall-time %.2fs" elapsed)))
     (paused
      (asyncloop-log loop "Loop paused"))
     ;; (This clause only applies if :immediate-break-on-user-activity nil,
     ;; because if that's t, we never arrive here anyway.)
     ;; User just did I/O, so free a moment for Emacs to respond to it.
     ;; Because of the idle timer, this moment may be extended indefinitely.
     ((input-pending-p)
      (setf scheduled t)
      (named-timer-idle-run timer-id 1 nil #'asyncloop-resume-1 loop))
     ;; round and round we go
     ((> 100 (cl-incf asyncloop-recursion-ctr))
      (asyncloop-chomp loop))
     ;; Every 100 calls, prune the call stack to minimize the risk of
     ;; tripping `max-lisp-eval-depth'
     (t
      (setq asyncloop-recursion-ctr 0)
      (setf scheduled t)
      (named-timer-run timer-id 0 nil #'asyncloop-resume-1 loop)))
    (if immediate-break-on-user-activity
        ;; Say "no problems" to `while-no-input'
        nil
      ;; Say "no problems" to the caller of `with-local-quit'
      t)))

(defun asyncloop-resume-1 (loop)
  "Resume executing asyncloop LOOP.
When user activity happens, abort and schedule another
invocation of `asyncloop-resume-1' on an idle timer.

Normally, you are meant to call `asyncloop-resume', which see."
  (asyncloop-with-slots (remainder just-launched paused scheduled immediate-break-on-user-activity timer-id) loop
    (setf just-launched nil)
    (setf paused nil)
    ;; Don't proceed if descheduled by `asyncloop-pause', or if some other
    ;; mystery factor got this to execute again -- it's not just that timers
    ;; with zero time left seem to have a habit of executing even though
    ;; `cancel-timer' was called, there is also some sort of spooky action (cue
    ;; X-Files theme) that executes this function for no reason occasionally.
    ;; I suspect the Emacs timer system wasn't made to be reliable.
    (if (not scheduled)
        (asyncloop-log loop
          "Unscheduled timer activation. Hands off the wheel, ghost!")
      (setf scheduled nil)
      (if (null remainder)
          (asyncloop-log loop "Scheduled loop found cleared, doing nothing")
        (asyncloop-notify-simultaneity loop)
        (if immediate-break-on-user-activity
            (when (while-no-input (asyncloop-chomp loop))
              ;; User just did I/O, so free a moment for Emacs to respond to
              ;; it.  Use an idle timer so this moment may be extended for
              ;; however long the user does it.
              (setf scheduled t)
              (named-timer-idle-run timer-id 1 nil #'asyncloop-resume-1 loop))
          ;; Without `while-no-input', something should be able to interrupt
          ;; a hung function, so allow C-g to do so.  I tried `with-timeout'
          ;; but found it unreliable in the context of several loops active
          ;; at the same time.
          ;;
          ;; The drawback is that C-g could hit at exactly the wrong time,
          ;; interrupting a function at a non-ideal point in its execution.
          ;; We implicitly promised the user in the docstring that the function
          ;; would not be interrupted.  So when it happens, cancel the whole
          ;; loop, as that'll probably lead to more correctness.
          (when (null (with-local-quit (asyncloop-chomp loop)))
            (asyncloop-cancel loop 'quietly)
            (asyncloop-log loop "Interrupted by C-g, cancelling")))))))

(defun asyncloop-resume (loop)
  "Prep asyncloop LOOP to resume when Emacs has a moment.
Concretely, place `asyncloop-resume-1' on a short timer, which
has the effect of deferring it until Emacs finishes executing the
current call stack.

A secondary effect is that by exposing this wrapper to end users
instead of `asyncloop-resume-1', we ensure that the latter is
always called by a timer and not directly, ensuring consistent
behavior.

Otherwise they might observe inconsistent behavior because timers
behave different from direct funcalls by, for example, always
setting `inhibit-quit' t."
  (asyncloop-with-slots (paused scheduled just-launched timer-id) loop
    (asyncloop-log loop "Loop told to resume")
    (setf paused nil)
    (setf scheduled t)
    (named-timer-run timer-id 0 nil #'asyncloop-resume-1 loop)
    (cl-assert (not just-launched))))

;;;###autoload
(cl-defun asyncloop-run
    (funs &key
          immediate-break-on-user-activity
          immediate-break-on-input
          on-interrupt-discovered
          log-buffer-name
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

As a tip, you could do all heavy or bug-prone calculations inside
some `let' bindings, then carry out side-effects via simple
`setq', `push', `pop' calls, which are practically instant.

If instead, IMMEDIATE-BREAK-ON-USER-ACTIVITY is left at nil, you
have fewer things to worry about as any given function will be
allowed to complete before Emacs responds to user activity.  The
drawback is that you may annoy the user if that function takes a
perceptible amount of time to complete, like 0.02s or longer.

The loop as a whole refuses to start twice with the same input if
the last invocation has not yet completed.  That means that if,
perhaps via a hook that triggers often, it's invoked several
times in a short timeframe, only the first invocation is likely
to do anything, and the rest will no-op in favor of letting the
already running loop finish.

If the previous invocation with the same input seems to have been
interrupted by an error and left in an incomplete state, it will
first call the optional function ON-INTERRUPT-DISCOVERED, then
resume the loop, picking up where it was left off.  To reiterate,
this recovery will not happen at the moment the loop is
interrupted, only the next time `asyncloop-run' is invoked (which
could be a long while or never, depending on what hook you put it
on).

By default, ON-INTERRUPT-DISCOVERED is nil, but another
reasonable value is #\\='asyncloop-cancel.  If you are having
problems, that lets you watch the loop restart in full, which
should help you debug what is going on.  The problem is likely
appropriately solved with a sanity check at the start of most/all
functions in FUNS.

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
met (in the style of a while-loop), have it push the symbol t
onto the accessor `asyncloop-remainder', which has the effect of
ensuring that the same function will run again:

  \(unless ...some-condition-that-means-done...
    \(push t \(asyncloop-remainder LOOP-OBJECT)))

As always with while-loop patterns, take a moment to ensure that
there is no way it will repeat forever.  If it is meant to
decrement an external counter by `cl-decf' or consume a list one
item at a time by `pop', place that form earlier than the above
form.

The accessor `asyncloop-remainder' returns the list of functions
left to run.  You can manipulate the list however you like, but
note that it includes the function currently being run, as the
first element!  The list will undergo a `pop' right after the
function completes.  (That's why the above form works: the symbol
t is just a placeholder to absorb the coming `pop'.)

Somewhat nonintuitive, but it had to be designed this way for
robustness to interruption.

A takeaway: if you wish to set the list to something entirely
different via `setf', include t as first element:

  \(setf \(asyncloop-remainder LOOP-OBJECT)
         \(list t #'function-1 #'function-2 #'function-3))

Finally, optional string LOG-BUFFER-NAME says to create a buffer
of log messages with that name.

It does not matter what the functions in FUNS return, but the
log buffer prints the return values, so by returning something
interesting (I suggest a short string constructed by `format'),
you can improve your debugging experience."
  (declare (indent defun))
  (cl-assert funs)
  (dolist (fn funs)
    (unless (functionp fn)
      (error "Not a function or not yet defined as such: %s" fn)))
  (let* (;; Handle deprecated calling convention for now
         (log-buffer-name (or log-buffer-name
                              debug-buffer-name))
         (immediate-break-on-user-activity (or immediate-break-on-user-activity
                                               immediate-break-on-input))
         ;; Name it as a deterministic hash of the input, ensuring that the
         ;; next call with the same input can see that it was already called
         (id (abs (sxhash (list funs
                                on-interrupt-discovered
                                log-buffer-name
                                immediate-break-on-user-activity))))
         (loop
          (or (alist-get id asyncloop-objects)
              (setf (alist-get id asyncloop-objects)
                    (asyncloop-create
                     :id id
                     :funs funs
                     :timer-id (intern (format "a%d" id))
                     :immediate-break-on-user-activity immediate-break-on-user-activity
                     :log-buffer
                     (when log-buffer-name
                       (with-current-buffer (get-buffer-create log-buffer-name)
                         (asyncloop-log-mode)
                         (current-buffer))))))))
    (when immediate-break-on-input
      (warn "Deprecated keyword name will be removed Dec 2023: :immediate-break-on-input.  Use :immediate-break-on-user-activity."))
    (when debug-buffer-name
      (warn "Deprecated keyword name will be removed Dec 2023: :debug-buffer-name.  Use :log-buffer-name."))
    (asyncloop-with-slots (remainder scheduled just-launched starttime timer-id paused scheduled) loop
      ;; Ensure that being triggered by several simultaneous hooks won't spam
      ;; the log buffer, or worse, start duplicate loops (somehow happened in
      ;; the past -- maybe if a timer is at 0 seconds, it can't be cancelled?)
      (unless just-launched
        (setf just-launched t)
        (cond
         (scheduled
          (setf just-launched nil)
          (asyncloop-log loop "Already running, letting it continue"))

         ;; Resuming an apparently half-done loop
         ((and remainder (not (equal remainder funs)))
          (if paused
              (asyncloop-log loop
                "Loop was paused, must be explicitly unpaused via `asyncloop-resume' or `asyncloop-cancel'")
            (when on-interrupt-discovered
              (asyncloop-log loop
                "Loop had been interrupted, calling ON-INTERRUPT-DISCOVERED")
              (with-local-quit
                (asyncloop-clock-funcall loop on-interrupt-discovered)))
            (if (null remainder)
                (asyncloop-log loop "Cancelled by ON-INTERRUPT-DISCOVERED")
              (asyncloop-log loop
                "Loop had been interrupted, attempting to resume.  Functions left to run: %S"
                remainder)
              (setf scheduled t)
              (named-timer-run timer-id 0 nil #'asyncloop-resume-1 loop))))

         ;; Launch anew the full loop
         (t
          (asyncloop-log loop "Loop started")
          (setf remainder funs)
          (setf starttime (current-time))
          (setf scheduled t)
          (named-timer-run timer-id 0 nil #'asyncloop-resume-1 loop)))))
    loop))

(set-advertised-calling-convention
 'asyncloop-run
 '(funs &key
   immediate-break-on-user-activity
   on-interrupt-discovered
   log-buffer-name)
 "2023-11-13")

;;;###autoload
(define-obsolete-function-alias 'asyncloop-run-function-queue #'asyncloop-run
  "2023-11-09"
  "Changed my mind.  This alias will be removed in Dec 2023.")

;; ###autoload
;; (defun asyncloop-run-function-queue ()
;;   (error "This alias was removed, use `asyncloop-run'"))

(provide 'asyncloop)

;;; asyncloop.el ends here
