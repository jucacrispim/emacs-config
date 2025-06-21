(require 'multi-term)

(defcustom pdj:multi-term-switch-to-buffer t
  "Indicates if we should switch to the terminal buffer")

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


(defun pdj:multi-term (&optional dedicated)
  "Create new term buffer.

The difference from this function to multi-term's function is that here we
pop-to-buffer instead of switch-to-buffer"

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


;; (defun pdj:run-in-term (command &optional term-name)
;;   "Runs `command' in a terminal."

;;   (interactive)

;;   (let ((multi-term-program-switches (list "-tc" command))
;; 	(multi-term-close-on-finish nil)
;; 	(multi-term-dedicated-buffer-name term-name))
;;     (pdj:multi-term t)))


(defun pdj:shell-command (command &optional buffer-name)
  "Executes `command' in a shell. Unlike pdj:run-in-term it is blocking"
  (interactive)

  (let ((cmd-buffer (if buffer-name (get-buffer-create buffer-name)
		     (get-buffer-create "*pdj:shell-command*"))))
    (shell-command command cmd-buffer cmd-buffer)))


(defun pdj:multi-term-hooks ()
  "Customizations to multi-term.
  * Sets `multi-term-program' to /usr/bin/zsh
  * Disables yas-minor-mode
  * Sets `term-paste' to `C-y' on `term-raw-map'"

  (setq multi-term-program "/usr/bin/zsh")
  (yas-minor-mode -1)
  (define-key term-raw-map (kbd "C-y") 'term-paste))

(defun pdj:term-keyboard-hooks()
  (global-set-key (kbd "C-ŧ") 'pdj:vterm))


(defun pdj:term-setup()
  (pdj:term-keyboard-hooks)
  (add-hook 'term-mode-hook 'pdj:multi-term-hooks))

(provide 'pdj-term)
