;; venv hacks

(require 'virtualenvwrapper)
(provide 'pdj-venv)

(defun pdj:venv-mkvirtualenv (pyexec &rest names)
  "Create new virtualenvs NAMES using PYEXEC. If venv-location is a single
directory, the new virtualenvs are made there; if it is a list of
directories, the new virtualenvs are made in the current
default-directory."
  ;; this is changed because I want to be able to pass a executable
  ;; to virtualenv without being prompted

  (interactive)
  (venv--check-executable)
  (let ((parent-dir (if (stringp venv-location)
                        (file-name-as-directory
                         (expand-file-name venv-location))
                      default-directory))
        (python-exe-arg (concat "--python=" pyexec))

        (names (if names names
                 (list (read-from-minibuffer "New virtualenv: ")))))
    ;; map over all the envs we want to make
    (--each names
      ;; error if this env already exists
      (when (-contains? (venv-get-candidates) it)
        (error "A virtualenv with this name already exists!"))
      (run-hooks 'venv-premkvirtualenv-hook)
      (shell-command (concat "virtualenv " python-exe-arg " " parent-dir it))
      (when (listp venv-location)
        (add-to-list 'venv-location (concat parent-dir it)))
      (venv-with-virtualenv it
                            (run-hooks 'venv-postmkvirtualenv-hook))
      (when (called-interactively-p 'interactive)
        (message (concat "Created virtualenv: " it))))
    ;; workon the last venv we made
    (venv-workon (car (last names)))))
