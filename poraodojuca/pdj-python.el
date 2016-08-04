;; Hooks for Python.
;; Requires: pdj-utils, pdj-common, virtualenvwrapper, flycheck and radon

(require 'pdj-common)
(require 'pdj-utils)
(require 'virtualenvwrapper)
(require 'flycheck)
(require 'radon)

;; Customizable vars

(defcustom pdj:py-test-command "python setup.py test -q"
  "Command to run Python tests")

(defcustom pdj:venv-name nil
  "Virtualenv name for be used in the buffer. Customize this via .dir-locals.el")

(defcustom pdj:py-debug-buffer-name "py-debug"
  "Name of the buffer for python tests.")

(defcustom pdj:py-autopep8 nil
  "Indicates if we should enable autopep8 on save")


(defun pdj:py-reset-customvars ()
  "Returns custom var to theirs defalut values."

  (setq pdj:py-test-command "python setup.py test -q")
  (setq pdj:venv-name nil)
  (setq pdj:py-debug-buffer-name "py-debug")
  (setq pdj:py-autopep8 nil))


(defun pdj:py-set-test-command ()
  "Sets `pdj:test-command' to `pdj:py-test-command' if it is not defined."

  (hack-local-variables)

  (unless pdj:test-command
    (setq pdj:test-command pdj:py-test-command)))


(defun pdj:add-project-dir-to-python-path ()
  "Adds `pdj:project-directory' to the environment variable PYTHONPATH."

  (when pdj:project-directory
    (setenv "PYTHONPATH" pdj:project-directory)))


(defun pdj:py-remove-project-dir-from-python-path ()
  "Removes `pdj:project-directory' from the environment variable PYTHONPATH."

  (setenv "PYTHONPATH" nil))


(defun pdj:enable-autopep8 ()
  "Adds py-autopep8-enable-on-save hook to python-mode-hook if pdj:py-autopep8"

  (if pdj:py-autopep8
      (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)))


;; Interactive funcs

(defun pdj:py-package ()
  "The python package for the current buffer"

  (interactive)

  (defvar pdj--rel-dir)
  (defvar pdj--py-package)
  (defvar pdj--clean-buffer-name)

  (setq pdj--clean-buffer-name (replace-regexp-in-string "<\.*>" ""
							 (buffer-name)))
  (setq pdj--rel-dir (replace-regexp-in-string
		      pdj:project-directory "" (buffer-file-name)))
  (setq pdj--py-package (replace-regexp-in-string "/" "." pdj--rel-dir))
  (setq pdj--py-package (replace-regexp-in-string
			 (concat "." pdj--clean-buffer-name) ""
			 pdj--py-package))
  pdj--py-package)


(defun pdj:py-module ()
  "The python module for the current buffer"

  (defvar pdj--rel-dir)
  (defvar pdj--py-module)

  (interactive)

  (setq pdj--rel-dir (replace-regexp-in-string
		      pdj:project-directory "" (buffer-file-name)))
  (setq pdj--py-module (replace-regexp-in-string "/" "." pdj--rel-dir))
  (setq pdj--py-module (replace-regexp-in-string ".py" "" pdj--py-module))
  pdj--py-module)


(defun pdj:py-test-suite-under-cursor ()
  "The full qualified name for the test suite under the cursor"

  (defvar pdj--test-suite)

  (interactive)
  (save-excursion
    (if (equal (thing-at-point 'line) "\n")
	(forward-line -1))
    (setq pdj--test-suite (concat (pdj:py-module) (concat "." (which-function))))
    pdj--test-suite))


(defun pdj:run-ipython ()
  "Runs python shell using ipython and lets default-directory as
   `pdj:project-directory'."

  (interactive)

  (setq python-shell-interpreter "ipython")

  (pdj:execute-on-project-directory
   'run-python python-shell-interpreter nil nil))


(defun pdj:py-insert-ipdb-at-point ()
  "Inserts an 'import ipdb;ipdb;set_trace()' at point."

  (interactive)

  (defvar pdj--ipdb "import ipdb;ipdb.set_trace()")

  (unless (string-match-p pdj--ipdb (thing-at-point 'line))
    (if (equal (thing-at-point 'line) "\n")
	(progn
	  (indent-for-tab-command)
	  (insert pdj--ipdb)
	  (beginning-of-line))
      (end-of-line)
      (insert "\n")
      (indent-for-tab-command)
      (insert pdj--ipdb)
      (beginning-of-line))))

(defun pdj:py-debug-tests (&optional insert-ipdb)
  "Run tests on a terminal. if `insert-ipdb', inserts ipdb at point"
  (interactive)

  (defvar pdj--debug-command)

  ;; command to debug one specific method
  (setq pdj--debug-command pdj:py-test-command)
  (setq pdj--debug-command (concat (concat pdj--debug-command " --test-suite=")
				   (pdj:py-test-suite-under-cursor)))

  (if insert-ipdb
      (pdj:py-insert-ipdb-at-point))

  (let ((before-save-hook nil))
    (save-buffer))

  (pdj:execute-on-project-directory
   'pdj:run-in-term pdj--debug-command pdj:py-debug-buffer-name))

(defun pdj:py-debug-tests-with-insert-ipdb ()
  "Run tests on a terminal but does not insert ipdb at point."

  (interactive)
  (pdj:py-debug-tests t))


(defun pdj:py-run-tests (&optional test-suite)
  "Run tests. If `test-suite' only tests from this suite will be executed."

  (interactive)

  (defvar pdj--test-command)

  (if pdj:test-command
      (let ((pdj--test-command pdj:test-command))
	(unless (equal test-suite nil)
	  (setq pdj--test-command (concat
				   pdj--test-command
				   (concat " --test-suite=" test-suite))))

	(pdj:execute-on-project-directory
	 'compile pdj--test-command))

    (message "No pdj:test-command. You have to customize this.")))


(defun pdj:py-run-tests-all ()
  "Runs all tests from the project"
  (interactive)

  (pdj:py-run-tests))


(defun pdj:py-run-tests-package ()
  "Run tests for the package of the current buffer."

  (interactive)

  (pdj:py-run-tests (pdj:py-package)))


(defun pdj:py-run-tests-module ()
  "Run tests for the current buffer."

  (interactive)
  (pdj:py-run-tests (pdj:py-module)))


(defun pdj:py-run-test-suite ()
  "Run test suite under the cursor."

  (interactive)
  (pdj:py-run-tests (pdj:py-test-suite-under-cursor)))


;; menu functions
(defun pdj:py-switch-to-shell ()
  "Switches to Python shell. If it was not started, starts it."

  (interactive)
  (unless (python-shell-get-process)
      (pdj:run-ipython))
  (python-shell-switch-to-shell))


(defun pdj:py-eval-buffer ()
  "Evaluates the current buffer in a Python shell."

  (interactive)
  (unless (python-shell-get-process)
      (pdj:run-ipython))
  (python-shell-send-buffer)
  (pdj:py-switch-to-shell))


(defun pdj:py-eval-region (start end)
  "Evaluates the marked region in a Python shell."

  (interactive "r")
  (unless (python-shell-get-process)
      (pdj:run-ipython))
  (python-shell-send-region start end)
  (pdj:py-switch-to-shell))

(defun pdj:py-eval-defun (arg)
  "Evaluates defun ina Python shell."

  (interactive "P")
  (unless (python-shell-get-process)
      (pdj:run-ipython))
  (python-shell-send-defun arg)
  (pdj:py-switch-to-shell))


(defun pdj:py-eval-file (file-name &optional process temp-file-name)
  "Evaluates a file in a Python shell."


  (interactive "fFile to send: ")
  (unless (python-shell-get-process)
      (pdj:run-ipython))
  (python-shell-send-file file-name process temp-file-name)
  (pdj:py-switch-to-shell))

;; :enable for test or debug suite menu.
(defun pdj:--is-test-suite ()
  "Indicates if the suite under cursor is a test suite."

  (defvar pdj--func-name)

  (setq pdj--func-name (car(last (split-string (which-function) "\\."))))
  (if (or (equal 0 (string-match-p "test" pdj--func-name))
	  (string-match-p "class" (thing-at-point 'line))) t))

;; Menus

(defun pdj:py-create-menu ()
  "Recriates the Python menu changing some of its elements."

  (interactive)
  ;; removing python.el menu
  (define-key python-mode-map [menu-bar Python] nil)

  ;; new python menu
  (define-key python-mode-map [menu-bar pdj-python]
    (cons "Python" (make-sparse-keymap "Python")))

  ;; shift region stuff
  (define-key python-mode-map [menu-bar pdj-python shift-region-left]
    '(menu-item "Shift region left" python-indent-shift-left
		:enable mark-active
		:help "Shift region left by a single indentation step"))

  (define-key-after python-mode-map [menu-bar pdj-python shift-region-right]
    '(menu-item "Shift region right" python-indent-shift-left
		:enable mark-active
		:help "Shift region left by a single indentation step")
    'shift-region-left)

  (define-key-after python-mode-map [menu-bar pdj-python first-separator]
    '(menu-item "--") 'shift-region-right)

  ;; start/end/definitions of functions/methods/classes
  (define-key-after python-mode-map [menu-bar pdj-python start-of-defun]
    '(menu-item "Start of def/class" beginning-of-defun
		:help "Go to start of outermost definition around point")
    'first-separator)

  (define-key-after python-mode-map [menu-bar pdj-python end-of-defun]
    '(menu-item "End of def/class" end-of-defun
		:help "Go to end of definition around point")
    'start-of-defun)

  (define-key-after python-mode-map [menu-bar pdj-python mark-def-class]
    '(menu-item "Mark def/class" mark-defun
		:help "Mark outermost definition around point")
    'end-of-defun)

  (define-key-after python-mode-map [menu-bar pdj-python jump-def-class]
    '(menu-item "Jump to def/class" imenu
		:help "Mark outermost definition around point")
    'mark-def-class)

  (define-key-after python-mode-map [menu-bar pdj-python second-separator]
    '(menu-item "--") 'jump-def-class)

  ;; shell related stuff
  (define-key-after python-mode-map [menu-bar pdj-python switch-to-shell]
    '(menu-item "Switch to Python shell" pdj:py-switch-to-shell
		:help "Switch to inferior Python process.")
    'second-separator)

  (define-key-after python-mode-map [menu-bar pdj-python py-eval-buffer]
    '(menu-item "Eval buffer" pdj:py-eval-buffer
		:help "Eval buffer in inferior Python session")
    'py-eval-string)

  (define-key-after python-mode-map [menu-bar pdj-python py-eval-region]
    '(menu-item "Eval region" pdj:py-eval-region
		:enable mark-active
		:help "Eval region in inferior Python session")
    'py-eval-buffer)

  (define-key-after python-mode-map [menu-bar pdj-python py-eval-defun]
    '(menu-item "Eval def/class" pdj:py-eval-defun
		:help "Eval defun in inferior Python session")
    'py-eval-region)

  (define-key-after python-mode-map [menu-bar pdj-python py-eval-file]
    '(menu-item "Eval file" pdj:py-eval-file
		:help "Eval defun in inferior Python session")
    'py-eval-defun)

  (define-key-after python-mode-map [menu-bar pdj-python third-separator]
    '(menu-item "--") 'py-eval-file)

  ;; testing
  (defvar menu-bar-pdj-python-testing (make-sparse-keymap "Testing"))

  (define-key-after menu-bar-pdj-python-testing [test-suite]
    '(menu-item "Test suite" pdj:py-run-test-suite
		:enable (progn  (pdj:--is-test-suite))
		:help "Run test suite under cursor")
    'third-separator)

  (define-key-after menu-bar-pdj-python-testing [test-module]
    '(menu-item "Test module" pdj:py-run-tests-module
		:help "Run current test module")
    'test-suite)

  (define-key-after menu-bar-pdj-python-testing [test-all]
    '(menu-item "All tests" pdj:py-run-tests-all
		:help "Run all tests")
    'tests-module)

  (define-key-after python-mode-map [menu-bar pdj-python testing]
    (list 'menu-item "Testing" menu-bar-pdj-python-testing))

  ;; Debug
  (defvar menu-bar-pdj-python-debug (make-sparse-keymap "Debug"))

  (define-key menu-bar-pdj-python-debug [debug-test-suite]
    '(menu-item "Debug test suite" pdj:py-debug-tests-with-insert-ipdb
		:enable (progn  (which-function))
		:help "Inserts an ipdb at point and run test suite on debug frame"))

  (define-key-after menu-bar-pdj-python-debug [debug-test-suite-no-ipdb]
    '(menu-item "Debug test suite (no ipdb)" pdj:py-debug-tests
		:enable (progn  (which-function))
		:help "Runs test suite on debug frame")
    'debug-test-suite)

  (define-key-after python-mode-map [menu-bar pdj-python debug]
    (list 'menu-item "Debug" menu-bar-pdj-python-debug)))


;; Hooks

(defun pdj:py-venv-hooks ()
  "Hooks for virtualenv. Activates virtualenv for the current buffer."

  (hack-local-variables)

  ;; project-venv-name came from .dir-locals.el
  (if pdj:venv-name
      (progn
	(venv-workon pdj:venv-name)
	(setq python-environment-default-root-name pdj:venv-name))
    (message "No pdj:venv-name. Do you have a proper .dir-locals.el?")))


(defun pdj:py-deactivate-venv-hooks ()
  "Undo the hooks for virtualenv"

  (venv-deactivate)
  (setq python-environment-default-root-name nil))


(defun pdj:py-jedi-hooks ()
  "Hooks for jedi. Setup jedi on python buffers. Jedi activates auto-complete
   by itself."
  ;; (setq jedi:setup-keys t)
  (setq jedi:key-show-doc (kbd "C-c D"))
  (setq jedi:complete-on-dot t)
  (jedi:setup))


(defun pdj:py-deactivate-jedi-hooks ()
  "Stops the jedi server."

  (when (boundp 'jedi:stop-server)
    (jedi:stop-server)))


(defun pdj:py-ac-hooks()
  "Removes `ac-source-words-in-same-mode-buffers' from `ac-sources'"

  (delq 'ac-source-words-in-same-mode-buffers ac-sources))


(defun pdj:py-deactivate-ac-hooks ()
  "Adds `ac-source-words-in-same-mode-buffers' to `ac-sources'"

  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers))


(defun pdj:radon-hooks ()
  "Sets radon limit to B."
  (setq radon-analize-args '("-n" "B")))


(defun pdj:py-flycheck-hooks ()
  "Enables flycheck and disables pylint checker"

  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))


(defun pdj:py-deactivate-flycheck-hooks ()
  "Removes `python-pylint' from the `flycheck-disabled-checkers'."

  (delq 'python-pylint flycheck-disabled-checkers))


(defun pdj:py-keyboard-hooks ()
  "Custom key bindings. The following bindings are done here:

   * The bindings from pdj:prog-keyboard-hooks
   * `C-c p' - pdj:py-run-tests-package
   * `C-c s' - pdj:py-run-test-suite
   * `C-c m' - 'pdj:py-run-tests-module
   * `C-c d' - 'pdj:py-debug-tests
   * `C-c C-d' - pdj:py-debug-tests-with-insert-ipdb
   * `C-c x' - 'radon"

  (pdj:prog-keyboard-hooks)
  (local-set-key (kbd "C-c p") 'pdj:py-run-tests-package)
  (local-set-key (kbd "C-c s") 'pdj:py-run-test-suite)
  (local-set-key (kbd "C-c m") 'pdj:py-run-tests-module)
  (local-set-key (kbd "C-c d") 'pdj:py-debug-tests)
  (local-set-key (kbd "C-c C-d") 'pdj:py-debug-tests-with-insert-ipdb)
  (local-set-key (kbd "C-c x") 'radon)
  (local-set-key (kbd "C-c C-z") 'pdj:py-switch-to-shell))


(defun pdj:py-all-hooks ()
  "Enables all pdj:py hooks. They are:

  * pdj:py-venv-hooks
  * pdj:py-jedi-hooks
  * pdj:py-keyboard-hooks
  * pdj:py-flycheck-hooks
  * pdj:py-ac-hooks
  * pdj:radon-hooks"

  (pdj:py-venv-hooks)
  (pdj:py-jedi-hooks)
  (pdj:py-keyboard-hooks)
  (pdj:py-flycheck-hooks)
  (pdj:py-ac-hooks)
  (pdj:radon-hooks)
  (pdj:py-set-test-command)
  (pdj:add-project-dir-to-python-path)
  (pdj:py-create-menu)
  (pdj:enable-autopep8))


(defun pdj:py-setup ()
  "Adds pdj:py-all-hooks and py-autopep8-enable-on-save to python-mode-hook"

  (add-hook 'python-mode-hook 'pdj:py-all-hooks))


(defun pdj:py-deactivate ()
  "Undo everything done for python."

  (pdj:py-reset-customvars)
  (pdj:py-deactivate-venv-hooks)
  (pdj:py-deactivate-jedi-hooks)
  (pdj:py-deactivate-ac-hooks)
  (pdj:py-deactivate-flycheck-hooks)
  (pdj:py-remove-project-dir-from-python-path))


(provide 'pdj-python)
