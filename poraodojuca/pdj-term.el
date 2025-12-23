;; -*- lexical-binding: t; -*-

(require 'vterm)

(defcustom pdj:vterm-buffer-name "*vterm*"
  "Default name for created Vterm buffers."
  :type 'string
  :group 'pdj)


(defun pdj:vterm (&optional dedicated)
  "Create new vterm buffer.

If DEDICATED is non-nil, create a uniquely named buffer.
Use `pop-to-buffer' to display the buffer."
  (interactive)
  (let ((vterm-buffer-name (if dedicated
                               (generate-new-buffer-name pdj:vterm-buffer-name)
                             pdj:vterm-buffer-name)))
    (pop-to-buffer (vterm vterm-buffer-name))))


(defun pdj:run-in-term (command &optional term-name)
  "Run COMMAND in a new vterm buffer.

If TERM-NAME is provided, use it as the buffer name."
  (interactive)
  (let ((vterm-buffer-name (or term-name
                               (generate-new-buffer-name pdj:vterm-buffer-name))))
    (pop-to-buffer (vterm vterm-buffer-name))
    (vterm-send-string command)
    (vterm-send-return)))


(defun pdj:--dummy-pop-to-buffer(buffer-or-name &optional action norecord)
  "don't pop to anywhere"

  (let* ((buffer (window-normalize-buffer-to-switch-to buffer-or-name)))
    (set-buffer buffer)
    buffer))

(defun pdj:run-in-term-on-background (command &optional term-name)
  "Run COMMAND in a new vterm buffer in background, without poping to buffer.

If TERM-NAME is provided, use it as the buffer name."
  (interactive "P")

  (let ((vterm-buffer-name (or term-name
                               (generate-new-buffer-name pdj:vterm-buffer-name))))
    (vterm--internal #'pdj:--dummy-pop-to-buffer vterm-buffer-name)
    (vterm-send-string command)
    (vterm-send-return)))


(defun pdj:shell-command (command &optional buffer-name)
  "Executes `command' in a shell. Unlike pdj:run-in-term it is blocking"
  (interactive)

  (let ((cmd-buffer (if buffer-name (get-buffer-create buffer-name)
		     (get-buffer-create "*pdj:shell-command*"))))
    (shell-command command cmd-buffer cmd-buffer)))


(defun pdj:term-keyboard-hooks()
  (global-set-key (kbd "C-ŧ") 'pdj:vterm))


(defun pdj:term-setup()
  (pdj:term-keyboard-hooks))

(provide 'pdj-term)
