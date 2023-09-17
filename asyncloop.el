;;; asyncloop.el --- Non-blocking series of functions -*- lexical-binding: t -*-

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
<<<<<<< HEAD
;; Version: 0.2.0
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/asyncloop
;; Package-Requires: ((emacs "28.1") (named-timer "0.1"))
=======
;; Version: 0.3.0-pre
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/asyncloop
;; Package-Requires: ((emacs "28.1"))
>>>>>>> 8a7c2e9 (Bugfix)

;;; Commentary:

;; Use `asyncloop-run' to call a series of functions without hanging Emacs.

<<<<<<< HEAD
;;; Code:

(require 'cl-lib)
(require 'named-timer) ;; prevents bugs every day

(defun asyncloop-debug-buffer (loop)
  "Get/create a debug buffer associated with LOOP."
  (let ((bufname (concat "*" (symbol-name (asyncloop-id loop)) "*")))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname)
          (set (make-local-variable 'window-point-insertion-type) t)
          (current-buffer)))))
=======
;; TODO: orderless-initialisms to ignore ^dei-

;;; Code:

(require 'cl-lib)
>>>>>>> 8a7c2e9 (Bugfix)

(defun asyncloop-log (loop &rest args)
  "Log a message to the debug buffer associated with LOOP.
Arguments ARGS are the arguments for `format'."
  (declare (indent 1))
<<<<<<< HEAD
  (when (asyncloop-debug loop)
    (let ((x (apply #'format args)))
      (with-current-buffer (asyncloop-debug-buffer loop)
        (goto-char (point-max))
        (insert (format-time-string "%T: ") x)
        (newline)
        ;; Allow convenient sexp like (error (asyncloop-log loop "..."))
        x))))

(cl-defstruct (asyncloop (:constructor asyncloop-create)
                         (:copier nil))
  id
  funs
  (remainder nil)
  (last-idle-value 0)
  (just-launched nil)
  starttime
  (cancelled nil)
  debug)

(defmacro asyncloop-with-slots (slots obj &rest body)
  "Like `with-slots' but for a struct rather than an eieio class.
That is to say, in similar fashion as `map-let', bind slot names
SLOTS referring to fields of the asyncloop object OBJ, and
execute BODY.

Unlike `map-let', allow modifying these slots directly with
`setf', `push' etc.  This also means that accessing their values
takes them not from a memory-copy of the object, but directly
refers the object, allowing changes made by child function calls
to propagate back to the object accessed by the caller."
=======
  (let ((buf (asyncloop-debug-buffer loop)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let ((text (apply #'format args)))
          (goto-char (point-max))
          (insert (format-time-string "%T: ") text)
          (newline)
          ;; Allow the convenient sexp (error "%s" (asyncloop-log loop "msg"))
          text)))))

(cl-defstruct (asyncloop (:constructor asyncloop-create)
                         (:copier nil))
  funs
  starttime
  (remainder nil)
  (chomp-is-scheduled nil)
  (just-launched nil)
  (debug-buffer nil)
  (last-idle-value 0)
  (interrupt-counter 0))

(defmacro asyncloop-with-slots (slots obj &rest body)
  "Like EIEIO's `with-slots', but for our struct type."
>>>>>>> 8a7c2e9 (Bugfix)
  (declare (indent 2))
  `(cl-symbol-macrolet
       ,(cl-loop
         for slot in slots
         collect `(,slot (,(intern (concat "asyncloop-" (symbol-name slot)))
                          ,obj)))
     ,@body))

(defvar asyncloop-objects nil
  "Alist identifying unique asyncloop objects.
Expected format:
  '((ID1 . OBJECT1)
    (ID2 . OBJECT2)
    ...)")

(defun asyncloop-clock-funcall (loop fn)
<<<<<<< HEAD
  "Run FN, and print time elapsed to LOOP's debug buffer."
=======
  "Run FN, then print elapsed time to LOOP's debug buffer."
>>>>>>> 8a7c2e9 (Bugfix)
  (let ((fn-name (if (symbolp fn) fn "lambda"))
        (then (current-time))
        (result (funcall fn loop)))
    (asyncloop-log loop
      "Took %.2fs: %s: %s" (float-time (time-since then)) fn-name result)))

(defun asyncloop-cancel (loop)
<<<<<<< HEAD
  "Stop asyncloop LOOP from executing further functions.
Ensure the loop will restart fresh on the next call to
`asyncloop-run'.

\(Tip: If you're a novice Elisp programmer, be aware that calling
this in the middle of a function body won't interrupt the rest of
it; will still complete normally.  If you want a nonlocal exit,
look up `cl-block' and `cl-return'.)"
  (asyncloop-with-slots (id remainder just-launched cancelled) loop
    (named-timer-cancel id)
    (setf just-launched nil)
    (setf remainder nil)
    (setf cancelled t)))

(defun asyncloop-reset-all ()
  "Cancel all asyncloops and wipe `asyncloop-objects'.
Mainly for debugging."
  (interactive)
  (ignore-errors
    (cl-loop for cell in asyncloop-objects
             do (asyncloop-cancel (cdr cell))))
  (setq asyncloop-objects nil))

(defun asyncloop-chomp (loop)
  "Call the next function in the asyncloop LOOP.
Then schedule another invocation of `asyncloop-chomp'.

Note that `asyncloop-chomp' must be called indirectly via
`named-timer-run'.  This mandate helps preserve the expected
behavior from the chain of timers, since `named-timer-run'
cancels any call that may have been pending, avoiding
double-calls."
  (asyncloop-with-slots (id remainder last-idle-value just-launched starttime cancelled) loop
    (setf just-launched nil)
    ;; The check `(not cancelled)' should always return true, but programmers
    ;; could make the mistake of repopulating `remainder' after a call to
    ;; `asyncloop-cancel' inside FUNC, thus we check both.
    (when (and remainder (not cancelled))
      (let ((func (pop remainder)))
        (condition-case err
            (progn
              ;; The real work happens here.
              (asyncloop-clock-funcall loop func)
              ;; Schedule the next step.
              (if (and remainder (not cancelled))
                  (let ((idled-time (or (current-idle-time) 0)))
                    (if (time-less-p last-idle-value idled-time)
                        ;; User hasn't done any I/O since last chomp, so proceed
                        ;; immediately to the next call.
                        (named-timer-run id 0 nil #'asyncloop-chomp loop)
                      ;; User just did I/O, so free a moment to respond to it.
                      (named-timer-idle-run id 1.0 nil #'asyncloop-chomp loop))
                    (setf last-idle-value idled-time))))

          ;; TODO: Maybe make available an "on-interrupt" function -- better than
          ;; "on-interrupt-discovered"?
          (t
           (asyncloop-log loop "Asyncloop interrupted because: %s" err)
           ;; Restore state so we don't skip a function just b/c it failed
           (push func remainder)
=======
  "Stop asyncloop LOOP from executing more queued functions.
Ensure the loop will restart fresh on the next call to
`asyncloop-run'."
  (setf (asyncloop-remainder loop) nil))

(defun asyncloop-chomp (loop)
  "Call the next function in the asyncloop LOOP.
Then schedule another invocation of `asyncloop-chomp'."
  (asyncloop-with-slots (remainder chomp-is-scheduled last-idle-value just-launched starttime) loop
    (setf just-launched nil)
    (setf chomp-is-scheduled nil)
    (when remainder
      (let ((func (pop remainder)))
        (condition-case err
            (progn
              ;; Do the real work
              (asyncloop-clock-funcall loop func)
              ;; Schedule the next step
              (when remainder
                (setf chomp-is-scheduled t)
                (let ((idled-time (or (current-idle-time) 0)))
                    (if (time-less-p last-idle-value idled-time)
                        ;; User hasn't done any I/O since last chomp, so proceed
                        ;; immediately to the next call.
                        (run-with-timer 0 nil #'asyncloop-chomp loop)
                      ;; User just did I/O, so free a moment for Emacs to
                      ;; respond to it.  Because it's on an idle timer, this
                      ;; moment may be extended indefinitely.
                      (run-with-idle-timer 1.0 nil #'asyncloop-chomp loop))
                    (setf last-idle-value idled-time))))

          ;; TODO: an interrupt-counter for automatic self-disabling
          ;; TODO: Is behavior sane on non-error signal?
          (t
           (setf chomp-is-scheduled nil)
           (asyncloop-log loop "Asyncloop interrupted because: %s" err)
           ;; Don't ever skip a function just b/c it failed.  This line will
           ;; probably never affect anything, but if it does, it results in more
           ;; correctness.
           (push func remainder)
           
>>>>>>> 8a7c2e9 (Bugfix)
           (when (eq (car err) 'error)
             (setf remainder nil)
             (error "Function %S failed: %s" func (cdr err)))))))

<<<<<<< HEAD
    (when (or cancelled (null remainder))
      (asyncloop-log loop
        "Loop %s, total wall-time %.2fs"
        (if cancelled "cancelled" "finished")
        (float-time (time-since starttime))))))

;; TODO: Maybe draw a flowchart and refactor, fully defining all states
;; TODO: Implement a timeout such that user doesn't need to call `asyncloop-reset-all'.
;;;###autoload
(cl-defun asyncloop-run (funs &key on-interrupt-discovered debug launch-message id)
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

(Though you'll likely not need it, you can supply a custom symbol
ID, which also predetermines the name of the associated debug
buffer: \"*ID*\", and the timer in `named-timer-table'.)

If the previous loop with the same identifier seems to have been
interrupted and left in an incomplete state, call the optional
function ON-INTERRUPT-DISCOVERED, then resume the loop, picking
up where it was left off.  To reiterate, this doesn't happen when
the interrupt happens, only the next time `asyncloop-run' itself
is triggered.

\(Tip: It's not unlikely to get spurious interrupts because the
user types C-g at inopportune times, but it's also possible it's
intentional, and we must always respect a C-g.  If you're having
problems with insistent restarts, you could set
ON-INTERRUPT-DISCOVERED to `asyncloop-cancel' to get some
breathing room and watch the loop restart in full, which should
help you debug what's going on.  The problem is likely
appropriately solved with a sanity check at the start of every
function in FUNS.\)

All the functions inside FUNS and those provided in the optional
arguments are passed one argument: the loop object, which holds
all of this metadata.  This lets you inspect and manipulate the
running loop by passing the object to any of:
=======
    (when (null remainder)
      (let ((elapsed (float-time (time-since starttime))))
        (asyncloop-log loop "Loop finished, total wall-time %.2fs" elapsed)))))

;;;###autoload
(cl-defun asyncloop-run (funs &key on-interrupt-discovered debug-buffer-name)
  "Attempt to run the series of functions in list FUNS.
Execute them in sequence, but pause on user activity to keep
Emacs feeling snappy.

The loop as a whole refuses to start twice with the same input if
the last invocation has not yet completed.  That means that if,
perhaps via a hook that triggers often, you call this several
times in a short timeframe, only the first invocation is likely
to do anything, and the rest will no-op in favor of letting the
already running loop finish.

If the previous loop with the same input seems to have been
interrupted by an error and left in an incomplete state, first
call the optional function ON-INTERRUPT-DISCOVERED, then resume
the loop, picking up where it was left off.  To reiterate, this
recovery won't happen at the moment the loop is interrupted, only
the next time you execute `asyncloop-run' itself (which could be
a long while or never, depending on what hook you put it on).

If you're having problems with insistent restarts, you could set
ON-INTERRUPT-DISCOVERED to `asyncloop-cancel' to get some
breathing room and watch the loop restart in full, which should
help you debug what's going on.  The problem is likely
appropriately solved with a sanity check at the start of most/all
functions in FUNS.

All the functions in FUNS, as well as the optional
ON-INTERRUPT-DISCOVERED function, are passed one argument: the
loop object, which holds some metadata.  This lets those
functions inspect and manipulate the running loop by passing the
object on to any of:
>>>>>>> 8a7c2e9 (Bugfix)

- `asyncloop-cancel'
- `asyncloop-remainder'
- `asyncloop-log'

<<<<<<< HEAD
It does not matter what the functions in FUNS return, but the
debug buffer prints the return values, so by returning something
interesting \(ideally a string constructed by `format'), you can
improve your debugging experience.

To have a function in FUNS repeat itself until some condition is
met \(in the style of a while-loop), have it push itself onto the
result of `asyncloop-remainder'.  As always with while-loop
patterns, take a moment to ensure that there's no way it will
repeat forever.  If it's meant to decrement a counter by
`cl-decf' or consume a list one item at a time by `pop', consider
doing that before anything else in the function body.

If a loop does end up repeating forever, you can stop it with
\\[asyncloop-reset-all].

Optional argument DEBUG controls whether or not to create a
buffer of debug messages \(named according to ID\).

Optional argument LAUNCH-MESSAGE is a string with which to enrich
debug messages: it'll be printed when the loop starts."
  (declare (indent defun))
  (unless id
    (setq id (intern (concat "asyncloop-"
                             (number-to-string
                              (abs (sxhash (list on-interrupt-discovered
                                                 funs))))))))
  (let ((loop (or (alist-get id asyncloop-objects)
                  (setf (alist-get id asyncloop-objects)
                        (asyncloop-create
                         :id id
                         :funs funs
                         :debug debug)))))
    ;; (pcase-let ((`(,remainder ,last-idle-value ,funs ,just-launched ,starttime ,cancelled) loop))
    (asyncloop-with-slots (remainder last-idle-value funs just-launched starttime cancelled) loop
      ;; Ensure that being triggered by several concomitant hooks won't spam
      ;; the debug buffer, or worse, start multiple loops (somehow happens
      ;; -- maybe if a timer is at 0 seconds, it can't be cancelled?).
      (if just-launched
          (when (> (float-time (time-since just-launched)) 300)
            (error "Asyncloop seems stuck, please file a bug: %S" id))
        (setf just-launched (current-time))
        (setf cancelled nil)

        (cond
         ((or (member (named-timer-get id) timer-list)
              (member (named-timer-get id) timer-idle-list))
=======
To have a function in FUNS repeat itself until some condition is
met (in the style of a while-loop), have it push itself onto the
result of `asyncloop-remainder', effectively shoving itself back
onto the front of the queue of things-to-execute:

  (unless some-condition
    (push #'this-function (asyncloop-remainder loop)))

As always with while-loop patterns, take a moment to ensure that
there's no way it will repeat forever.  If it's meant to
decrement a counter by `cl-decf' or consume a list one item at a
time by `pop', consider doing that before anything else in the
function body that could hit a bug.

Optional string DEBUG-BUFFER-NAME controls whether to create a
buffer of debug messages, and if so, its name.

It does not matter what the functions in FUNS return, but the
debug buffer prints the return values, so by returning something
interesting (I suggest a short string constructed by `format'),
you can improve your debugging experience."
  (declare (indent defun))
  ;; Name it as a deterministic hash of the input, ensuring that
  ;; next call with the same input can recognize that it was
  ;; already called.
  (let* ((id (abs (sxhash (list funs on-interrupt-discovered debug-buffer-name))))
         (loop
          (or (alist-get id asyncloop-objects)
              (progn
                (cl-assert funs)
                ;; causes error for some reason
                ;; (dolist (fun funs)
                ;;   (cl-assert (functionp fun)))
                (setf (alist-get id asyncloop-objects)
                      (asyncloop-create
                       :funs funs
                       :debug-buffer
                       (when debug-buffer-name
                         (with-current-buffer (get-buffer-create debug-buffer-name)
                           (set (make-local-variable 'window-point-insertion-type) t)
                           (current-buffer)))))))))
    (asyncloop-with-slots (remainder chomp-is-scheduled last-idle-value just-launched starttime) loop
      ;; Ensure that being triggered by several concomitant hooks won't spam
      ;; the debug buffer, or worse, start multiple loops (somehow happens
      ;; -- maybe if a timer is at 0 seconds, it can't be cancelled?).
      (unless just-launched
        (setf just-launched t)
        (cond
         (chomp-is-scheduled
>>>>>>> 8a7c2e9 (Bugfix)
          (asyncloop-log loop "Already running, letting it continue"))

         ((and remainder (not (equal remainder funs)))
          (when on-interrupt-discovered
            (asyncloop-clock-funcall loop on-interrupt-discovered))
<<<<<<< HEAD
          ;; In case the ON-INTERRUPT-DISCOVERED function sets CANCELLED
          (if cancelled
              (progn
                (setf just-launched nil)
                (asyncloop-log loop "Cancelled by ON-INTERRUPT-DISCOVERED"))
            (if remainder
                (progn
                  (asyncloop-log loop "Loop had been interrupted, resuming.  Left to run: %S" remainder)
                  (setf last-idle-value 0)
                  (named-timer-run id 0 nil #'asyncloop-chomp loop))
              (error "CANCELLED and REMAINDER shouldn't both be nil now"))))
=======
          (if remainder
              (progn
                (asyncloop-log loop "Loop had been interrupted, resuming.  Left to run: %S" remainder)
                (setf last-idle-value 0)
                (run-with-timer 0 nil #'asyncloop-chomp loop))
            (asyncloop-log loop "Cancelled by ON-INTERRUPT-DISCOVERED")))
>>>>>>> 8a7c2e9 (Bugfix)

         (t
          ;; Launch anew the full loop
          (setf remainder funs)
          (setf last-idle-value 0)
          (setf starttime (current-time))
<<<<<<< HEAD
          (if launch-message
              (asyncloop-log loop "Loop started. %s" launch-message)
            (asyncloop-log loop "Loop started"))
          (cl-assert (null cancelled))
          (if remainder
              (named-timer-run id 0 nil #'asyncloop-chomp loop)
            (error "CANCELLED and REMAINDER shouldn't both be nil now"))))))))
=======
          (asyncloop-log loop "Loop started")
          (run-with-timer 0 nil #'asyncloop-chomp loop)))))))
>>>>>>> 8a7c2e9 (Bugfix)

(provide 'asyncloop)

;;; asyncloop.el ends here
