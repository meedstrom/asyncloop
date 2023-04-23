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
;; Version: 0.2.0-pre
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/asyncloop
;; Package-Requires: ((emacs "28.1") (named-timer "0.1"))

;;; Commentary:

;; Use `asyncloop-run' to call a series of functions without hanging Emacs.

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

(defun asyncloop-log (loop &rest args)
  "Log a message to the debug buffer associated with LOOP.
Arguments ARGS are the arguments for `format'."
  (declare (indent 1))
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
  (on-cancel nil)
  (per-stage nil)
  starttime
  (cancelled nil)
  debug)

(defmacro asyncloop-with-slots (slots obj &rest body)
  "Like `with-slots' but for a struct rather than an eieio class.
That is to say, in similar fashion as `map-let', bind slot names
SLOTS within the asyncloop object OBJ, and execute BODY.  Unlike
`map-let', allow modifying these slots directly with `setf',
`push' etc.  This also means that accessing their values takes
them not from a memory-copy of the object, but the object itself,
allowing changes made by child function calls to propagate back
to the object accessed by the caller because they're the same
object."
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
  "Run FN, and print time elapsed to LOOP's debug buffer."
  (let ((fn-name (if (symbolp fn) fn "lambda"))
        (then (current-time))
        (result (funcall fn loop)))
    (asyncloop-log loop
      "Took %.2fs: %s: %s" (float-time (time-since then)) fn-name result)))

(defun asyncloop-cancel (loop)
  "Stop asyncloop LOOP from executing further functions.
Ensure the loop will restart fresh on the next call to
`asyncloop-run'.  Finally, call the associated ON-CANCEL function
if one was specified \(see `asyncloop-run').

\(Tip: If you're a novice Elisp programmer, be aware that calling
this in the middle of a function body won't interrupt the rest of
it; will still complete normally.  If you want a nonlocal exit,
look up `cl-block' and `cl-return'.)"
  (asyncloop-with-slots (id remainder on-cancel just-launched cancelled) loop
    (named-timer-cancel id)
    (setf just-launched nil)
    (setf remainder nil)
    (setf cancelled t)
    (when on-cancel
      (asyncloop-clock-funcall loop on-cancel))))

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
  (asyncloop-with-slots (id remainder last-idle-value per-stage just-launched starttime cancelled) loop
    (setf just-launched nil)
    (when per-stage
      (funcall per-stage loop))
    ;; The check `(not cancelled)' is not essential, but programmers could make
    ;; the mistake of repopulating `remainder' after a call to
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
           (when (eq (car err) 'error)
             (setf remainder nil)
             (error "Function %S failed: %s" func (cdr err)))))))

    (when (or cancelled (null remainder))
      (asyncloop-log loop
        "Loop %s, total wall-time %.2fs"
        (if cancelled "cancelled" "finished")
        (float-time (time-since starttime))))))

;; TODO: Maybe draw a flowchart and refactor, eliminating as many special cases
;; as possible -- it occurs to me that on-start, per-stage and on-cancel COULD
;; all be part of someone's `funs'.  The presence of these keywords can
;; encourage someone to write better code, but you could write something
;; educational in the README instead.
;; TODO: Consider whether to instruct users to wrap their function bodies in
;; while-no-input.
;; TODO: Implement a timeout such that user doesn't need to call `asyncloop-reset-all'.
;;;###autoload
(cl-defun asyncloop-run
    (funs &key on-interrupt-discovered per-stage on-start on-cancel origin debug
          (id (intern (concat "asyncloop-"
                              (number-to-string
                               (abs (sxhash (list funs
                                                  on-interrupt-discovered
                                                  per-stage
                                                  on-start
                                                  on-cancel)))))))
          &aux (loop (or (alist-get id asyncloop-objects)
                         (setf (alist-get id asyncloop-objects)
                               (asyncloop-create
                                :id id
                                :funs funs
                                :per-stage per-stage
                                :on-cancel on-cancel
                                :debug debug)))))
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
breathing room and watch the loop restart in full, which should
help you debug what's going on.  The problem is likely
appropriately solved with a sanity check in PER-STAGE or
elsewhere.\)

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

Optional argument ORIGIN is a string with which to enrich debug
messages: when the loop starts, it'll say \"Loop started from
ORIGIN\".

Optional argument DEBUG controls whether or not to create a
buffer of debug messages \(named according to ID\).

Changing DEBUG or ORIGIN will not change the ID generated."
  (declare (indent defun))
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
        (asyncloop-log loop "Already running, letting it continue"))

       ((and remainder (not (equal remainder funs)))
        (when on-interrupt-discovered
          (asyncloop-clock-funcall loop on-interrupt-discovered))
        ;; In case on-interrupt-discovered sets cancelled
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

       (t
        ;; Launch anew the full loop
        (setf remainder funs)
        (setf last-idle-value 0)
        (setf starttime (current-time))
        (if origin
            (asyncloop-log loop "Loop started from %s" origin)
          (asyncloop-log loop "Loop started"))
        (when on-start
          (asyncloop-clock-funcall loop on-start))
        (if cancelled
            (progn
              (setf just-launched nil)
              (asyncloop-log loop "Cancelled by ON-START"))
          (if remainder
              (named-timer-run id 0 nil #'asyncloop-chomp loop)
            (error "CANCELLED and REMAINDER shouldn't both be nil now"))))))))

(provide 'asyncloop)

;;; asyncloop.el ends here
