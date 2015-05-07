(require 'virtualenvwrapper)
(require 'flycheck-pyflakes)

(setq venv-location (expand-file-name "~/.virtualenvs/"))
(setq python-environment-directory venv-location)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(global-set-key (kbd "C-#") 'py-autopep8)

(defun jedi-venv-hooks ()
  (hack-local-variables)
  (when (boundp 'project-venv-name)
    (venv-workon project-venv-name)
    (setq python-environment-default-root-name project-venv-name))
    ;;(setq project-venv-path (concat venv-location project-venv-name))
    ;;(setq project-python-bin (concat project-venv-path "/bin/python ")))

  (jedi:setup))


(add-hook 'python-mode-hook (lambda ()
			      (jedi-venv-hooks)))

(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

(defun delete-whitespace-on-save ()
  (save-excursion
    (delete-trailing-whitespace)))

(add-hook 'write-file-hooks (lambda () (delete-whitespace-on-save)))
