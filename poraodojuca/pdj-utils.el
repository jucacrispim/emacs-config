;; Utilities for pdj hooks.

(require 'multi-term)


(defcustom pdj:kill-all-buffers-hooks (list)
  "Hooks to be used when killing all buffers.")

(defcustom pdj:multi-term-switch-to-buffer t
  "Indicates if we should switch to the terminal buffer")


(defun pdj:multi-term (&optional dedicated)
  "Create new term buffer.

The difference from this function to multi-term's function is that here we pop-to-buffer instead of switch-to-buffer"

  (interactive)
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (multi-term-get-buffer nil dedicated))
    (setq multi-term-buffer-list (nconc multi-term-buffer-list
					(list term-buffer)))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (when pdj:multi-term-switch-to-buffer
      (pop-to-buffer term-buffer))))


(defun pdj:run-in-term (command &optional term-name)
  "Runs `command' in a terminal."

  (interactive)

  (let ((multi-term-program-switches (list "-tc" command))
	(multi-term-close-on-finish nil)
	(multi-term-dedicated-buffer-name term-name))
    (pdj:multi-term t)))


(defun pdj:execute-on-project-directory (func &rest args)
  "Changes `default-directory' to `pdj:project-directory' and
   executes `func'"

  (if (not (equal pdj:project-directory nil))
      (let ((default-directory pdj:project-directory))
	(apply func args))
    (progn
      (message "No pdj:project-directory. Executing on default-directory")
      (apply func args))))


(defun pdj:run-in-term-on-project-directory (command &optional term-name)
  "Runs `command' in a terminal, changing `default-directory' to
   `pdj:project-directory'"

  (interactive)

  (pdj:execute-on-project-directory 'pdj:run-in-term command term-name)
  nil)


(defun pdj:deferred-process (command)
  "A deferred wrapper of `pdj:run-in-term-on-project-directory'.
Return a deferred object. The process name and buffer name of the
argument of the `pdj:run-in-term-on-project-directory' are generated
by this function automatically. The next deferred object receives stdout
string from the command process."
  (deferred:process-gen 'pdj:run-in-term-on-project-directory command))


(defun pdj:compile-on-project-directory (compile-command)
  "Runs the `compile' function, using `compile-command', on
   pdj:project-directory"

  (pdj:execute-on-project-directory 'compile compile-command))


(defun pdj:run-tests (&optional test-args)
  "Runs tests using `pdj:test-command'. If test-args, concat it to
   the test command."

  (interactive)

  (defvar pdj--test-command)

  (if pdj:test-command
      (let ((pdj--test-command pdj:test-command))
	(unless (equal test-args nil)
	  (setq pdj--test-command (concat pdj--test-command
					  (concat " " test-args))))

	(pdj:compile-on-project-directory pdj--test-command))

    (message "No pdj:test-command. You have to customize this.")))


(defun pdj:check-coverage ()
  "Performs tests coverage check using `pdj:coverage-command'"

  (interactive)

  (if pdj:coverage-command
      (pdj:compile-on-project-directory pdj:coverage-command)
    (message "No pdj:coverage-command. You have to customize this.")))


(defun pdj:shift-region (distance)
  "Shifts region to right if `distance' is positive, left if negative."

  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))


(defun pdj:shift-region-right ()
  "Changes region one character to the right."
  (interactive)
  (pdj:shift-region 1))


(defun pdj:shift-region-left ()
  "Changes region one character to the left."
  (interactive)
  (pdj:shift-region -1))


(defun pdj:mark-whole-line()
  "Marks the line where the cursor is."
  (interactive)

  (end-of-line)
  (set-mark (line-beginning-position)))


(defun pdj:delete-trailing-whitespace ()
  "Saves excursion and executes `delete-trailing-whitespace'"

  (save-excursion
    (delete-trailing-whitespace)))


(defun pdj:kill-all-buffers ()
  "Kill all buffers except  *scratch* and displays splash screen."

  (interactive)

  (let (buffers2kill
	(kill-buffer-query-functions (delq 'process-kill-buffer-query-function
					   kill-buffer-query-functions)))

    ;; the hooks
    (mapc (lambda  (func) (apply func nil)) pdj:kill-all-buffers-hooks)
    (delete-other-windows)
    (setq bufferlist (buffer-list))
    (mapc 'kill-buffer (dolist (buffer bufferlist buffers2kill)
			 (unless (equal buffer '*scratch*)
			   (setq buffers2kill (cons buffer buffers2kill)))))
    (setq pdj:--custom-already-loaded '())
    (display-splash-screen)))

(defun pdj:ask (prompt &optional default)
  "Prompts the user with `prompt' and returns the value"


  (defvar pdj:--ask-out nil)

  (let (pdj:--ask-out)
    (if default
	(setq prompt (concat prompt " (" default "): "))
      (setq prompt (concat prompt ": ")))
    (while (or (equal pdj:--ask-out "") (not pdj:--ask-out))
      (setq pdj:--ask-out (read-string prompt nil nil default)))
    pdj:--ask-out))

(defun pdj:read-file (path)
  "Returns the contents of a file as a string."

  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(provide 'pdj-utils)


(defun pdj:save-no-hooks ()
  "Saves the current buffer without applying any save hooks"

  (interactive)

  (let ((before-save-hook nil))
    (save-buffer)))
