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
;; Version: 0.3.2
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/asyncloop
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Use `asyncloop-run' to call a series of functions without hanging Emacs.

;; TODO: orderless-initialisms to ignore ^dei-

;;; Code:

(require 'cl-lib)

(defvar asyncloop-debug-buffer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] #'asyncloop-keyboard-quit)
    (define-key map [remap abort-recursive-edit] #'asyncloop-keyboard-quit)
    (define-key map [remap doom/escape] #'asyncloop-keyboard-quit)
    map))

(defun asyncloop-log (loop &rest args)
  "Log a message to the debug buffer associated with LOOP.
Arguments ARGS are the arguments for `format'."
  (declare (indent 1))
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

(defun asyncloop-clock-funcall (loop fn)
  "Run FN, then print elapsed time to the debug buffer of LOOP."
  (let ((fn-name (if (symbolp fn) fn "lambda"))
        (then (current-time))
        (result (funcall fn loop)))
    (asyncloop-log loop
      "Took %.2fs: %s: %s" (float-time (time-since then)) fn-name result)))

(defun asyncloop-cancel (loop)
  "Stop asyncloop LOOP from executing more queued functions.
Ensure the loop will restart fresh on the next call to
`asyncloop-run'."
  (setf (asyncloop-remainder loop) nil)
  (asyncloop-log loop "Cancelled"))

(defun asyncloop-chomp (loop)
  "Call the next function in the asyncloop LOOP.
Then schedule another invocation of `asyncloop-chomp'."
  (asyncloop-with-slots (remainder chomp-is-scheduled last-idle-value just-launched starttime) loop
    (setf just-launched nil)
    (setf chomp-is-scheduled nil)
    (when remainder
      (let ((user-given-function (pop remainder)))
        ;; Do the real work
        (asyncloop-clock-funcall loop user-given-function)
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
            (setf last-idle-value idled-time)))))
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
recovery will not happen at the moment the loop is interrupted,
only the next time you execute `asyncloop-run' itself (which could be a
long while or never, depending on what hook you put it on).

If you are having problems with insistent restarts, you could set
ON-INTERRUPT-DISCOVERED to `asyncloop-cancel' to get some breathing room
and watch the loop restart in full, which should help you debug
what is going on.  The problem is likely appropriately solved
with a sanity check at the start of most/all functions in FUNS.

All the functions in FUNS, as well as the optional
ON-INTERRUPT-DISCOVERED function, are passed one argument: the
loop object, which holds some metadata.  This lets those
functions inspect and manipulate the running loop by passing the
object on to any of:

- `asyncloop-cancel'
- `asyncloop-remainder'
- `asyncloop-log'

To have a function in FUNS repeat itself until some condition is
met (in the style of a while-loop), have it push itself onto the
result of `asyncloop-remainder', effectively shoving itself back onto the
front of the queue of things-to-execute:

  (unless some-condition
    (push #\\='this-function (asyncloop-remainder loop)))

As always with while-loop patterns, take a moment to ensure that
there is no way it will repeat forever.  If it is meant to
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
                           (use-local-map asyncloop-debug-buffer-keymap)
                           (current-buffer)))))))))
    (asyncloop-with-slots (remainder chomp-is-scheduled last-idle-value just-launched starttime) loop
      ;; Ensure that being triggered by several concomitant hooks won't spam
      ;; the debug buffer, or worse, start multiple loops (somehow happens
      ;; -- maybe if a timer is at 0 seconds, it can't be cancelled?).
      (unless just-launched
        (setf just-launched t)
        (cond
         (chomp-is-scheduled
          (asyncloop-log loop "Already running, letting it continue"))

         ((and remainder (not (equal remainder funs)))
          (when on-interrupt-discovered
            (asyncloop-clock-funcall loop on-interrupt-discovered))
          (if remainder
              (progn
                (asyncloop-log loop
                  "Loop had been interrupted, resuming.  Functions left to run: %S" remainder)
                (setf last-idle-value 0)
                (run-with-timer 0 nil #'asyncloop-chomp loop))
            (asyncloop-log loop "Cancelled by ON-INTERRUPT-DISCOVERED")))

         (t
          ;; Launch anew the full loop
          (setf remainder funs)
          (setf last-idle-value 0)
          (setf starttime (current-time))
          (asyncloop-log loop "Loop started")
          (run-with-timer 0 nil #'asyncloop-chomp loop)))))))

;; More descriptive name
(defalias 'asyncloop-run-function-queue #'asyncloop-run)

(defun asyncloop-reset-all ()
  "Cancel all asyncloops and wipe `asyncloop-objects'.
Mainly for debugging."
  (interactive)
  (ignore-errors
    (cl-loop for cell in asyncloop-objects
             do (asyncloop-cancel (cdr cell))))
  (setq asyncloop-objects nil))

(defun asyncloop-keyboard-quit ()
  "Wrapper for `keyboard-quit', that also interrupts the loop."
  (interactive)
  (asyncloop-reset-all)
  (if (minibufferp)
      (abort-recursive-edit)
    (keyboard-quit)))

(provide 'asyncloop)

;;; asyncloop.el ends here
