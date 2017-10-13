;; mode for .feature files used by behave test tool.

(require 'pdj-python)


(defcustom pdj:behave-command "behave"
  "Command to run behave test tool")

(defcustom pdj:behave-buffer-name "behave"
  "Name of the output buffer for behave tests.")

(defvar pdj:feature-mode-hook nil)

(defvar pdj:feature-keywords
  '(("Given\\|When\\|And\\|Then" . font-lock-keyword-face)
    ("Feature\\|Scenario" . font-lock-function-name-face))

  "Keywords for behave's feature file.")


(defun pdj:feature-file ()
  "The path for the current file"

  (buffer-file-name))


(defun pdj:feature-run-test-file ()

  (interactive)

  (defvar pdj--behave-command)
  (defvar pdj--rel-dir)
  (setq pdj--rel-dir (replace-regexp-in-string
		      pdj:project-directory "" (buffer-file-name)))

  (let ((pdj--behave-command (concat pdj:behave-command
				     (concat " " pdj--rel-dir))))
    (pdj:execute-on-project-directory
     'pdj:run-in-term pdj--behave-command pdj:behave-buffer-name)))


(defun pdj:feature-run-test-dir ()

  (interactive)

  (defvar pdj--behave-command)
  (defvar pdj--rel-dir)
  (setq pdj--rel-dir (replace-regexp-in-string "\\." "/" (pdj:py-package)))

  (let ((pdj--behave-command (concat pdj:behave-command
				     (concat " " pdj--rel-dir))))
    (pdj:execute-on-project-directory
     'pdj:run-in-term pdj--behave-command pdj:behave-buffer-name)))


(defun pdj:feature-post-py-hooks ()

  ;; we need to set this here because we want to run it
  ;; after the pdj-python and pdj-feature hooks
  (pdj:py-deactivate)
  (pdj:py-venv-hooks)
  (remove-hook 'before-save-hook 'py-autopep8-buffer)
  (local-set-key (kbd "C-c m") 'pdj:feature-run-test-file)
  (local-set-key (kbd "C-c p") 'pdj:feature-run-test-file))


(define-derived-mode pdj:feature-mode python-mode "pdj:feature"
  "Major mode for editing feature files used in behave test tool."
  (setq-local  font-lock-defaults '(pdj:feature-keywords))
  (add-hook 'pdj:feature-mode-hook 'pdj:feature-post-py-hooks))


(provide 'pdj-feature)
