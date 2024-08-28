;; Hooks for Python.
(require 'cl)
(require 'pdj-common)
(require 'pdj-utils)
(require 'virtualenvwrapper)
(require 'flycheck)
(require 'radon)
(require 'pdj-realgud)



(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (python-mode . lsp-deferred))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)


;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (python-mode . yas-minor-mode))

;; Customizable vars

(defcustom pdj:py-test-command "python setup.py test -q"
  "Command to run Python tests")

(defcustom pdj:py-test-suite-prefix "--test-suite="
  "Prefix to use when running a specific test suite.")

(defcustom pdj:venv-name nil
  "Virtualenv name to be used in the buffer. Customize this via .dir-locals.el")

(defcustom pdj:py-debug-buffer-name "py-debug"
  "Name of the buffer for python tests.")

(defcustom pdj:py-autopep8 nil
  "Indicates if we should enable autopep8 on save")

(defcustom pdj:py-custom-keywords
  '(("async def\\|async for\\|await " . font-lock-keyword-face))

  "Custom keywords for Python language. Default are for Python3.5 async stuff")

(defcustom pdj:py-pip-command "pip"
  "Command used to install python packages")

(defcustom pdj:py-requirements-file "requirements.txt"
  "File that contains a list of dependencies to install with pip.")


(defcustom pdj:py-package-command "python -m build"
  "Commands to create a python package.")

(defcustom pdj:py-upload-to-pypi-command "twine upload"
  "Command used to upload a distribution file to PyPI")

(defcustom pdj:py-dist-dir "dist/"
  "Directory where distribution packages are created.")

(defcustom pdj:py-test-under-cursor-fn nil
  "A function that returns a string with the test under cursor. If nil
`pdj:py-test-suite-under-cursor' will be used.")

(defcustom pdj:py-bootstrap-buffer-name "py-bootstrap-pip"
  "Buffer name for installing packages with pip")


(defun pdj:py-reset-customvars ()
  "Returns custom var to theirs defalut values."

  (setq pdj:py-test-command "python setup.py test -q")
  (setq pdj:py-test-suite-prefix "--test-suite=")
  (setq pdj:venv-name nil)
  (setq pdj:py-debug-buffer-name "py-debug")
  (setq pdj:py-autopep8 nil)
  (setq pdj:py-custom-keywords
	'(("async def\\|async for\\|await\\|async with" .
	   font-lock-keyword-face))))


(defun pdj:py-set-test-command ()
  "Sets `pdj:test-command' to `pdj:py-test-command' if it is not defined."

  (hack-local-variables)

  (unless pdj:test-command
    (setq pdj:test-command pdj:py-test-command))

  (setq realgud:pdb-command-name pdj:test-command)

  (if (boundp 'pdj:test-suite-prefix)
      (setq pdj:py-test-suite-prefix pdj:test-suite-prefix)))


(defun pdj:add-project-dir-to-python-path ()
  "Adds `pdj:project-directory' to the environment variable PYTHONPATH."

  (when pdj:project-directory
    (setenv "PYTHONPATH" pdj:project-directory)))


(defun pdj:py-remove-project-dir-from-python-path ()
  "Removes `pdj:project-directory' from the environment variable PYTHONPATH."

  (setenv "PYTHONPATH" nil))

(defun pdj:enable-autopep8 ()
  "Adds py-autopep8-mode hook to python-mode-hook if pdj:py-autopep8"

  (if pdj:py-autopep8
      (add-hook 'python-mode-hook 'py-autopep8-mode)))

(defun pdj:pylsp-hooks ()
    (setq lsp-pylsp-plugins-pydocstyle-enabled nil))

(defun pdj:disable-autopep8 ()
  "Removes py-autopep8-mode from python-mode-hook"

  (remove-hook 'python-mode-hook 'py-autopep8-mode))


(defun pdj:py-set-faces ()
  "Set face styles for python"

  (add-hook 'python-mode-hook
	    (lambda ()
	      (font-lock-add-keywords
	       nil
	       '(("^[[:space:]]*\\(@[^(#[:space:]\n]*\\)" 1
		  'font-lock-type-face))))))



(defun pdj:py-pip-install (what)
  "Installs `what' using pip"

   (pdj:run-in-term-on-project-directory
    (concat pdj:py-pip-command " install " what) pdj:py-bootstrap-buffer-name))

(defun pdj:py-pip-install-blocking (what)
  "Install `what' using pip. Blocking version. Useful for chaining"
  (pdj:shell-command-on-project-directory
   (concat pdj:py-pip-command " install " what) pdj:py-bootstrap-buffer-name))


(defun pdj:py-install-requirements ()
  "Install the dependencies of a project."

  (if pdj:py-requirements-file
      (pdj:py-pip-install (concat "-r " pdj:py-requirements-file
				  ;; these are used by emacs.
				  " flake8 autopep8"))
    (error "No requirements file.")))


(defun pdj:py-install-requirements-blocking ()
  "Install the dependencies of a project. Blocking version. Useful for chaining"

  (if pdj:py-requirements-file
      (pdj:py-pip-install-blocking (concat "-r " pdj:py-requirements-file))
    (error "No requirements file.")))


(defun pdj:py-bootstrap ()
  "Installs the project dependencies and the pyls server."

  (defvar pdj:py--instal-server-cmd "pip install 'python-lsp-server[all]'")

  (pdj:add-project-dir-to-python-path)
  (pdj:py-venv-hooks)

  (deferred:$
    (deferred:next
      (lambda ()
	(pdj:py-install-requirements)))

    (deferred:nextc it
      (lambda ()
	(pdj:run-in-term-on-project-directory pdj:py--instal-server-cmd
					      "py-bootstrap")))))


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

(defun pdj:py-dir ()
    "The directory for the current buffer"

  (interactive)

  (defvar pdj--rel-dir)
  (defvar pdj--clean-buffer-name)

  (setq pdj--clean-buffer-name (replace-regexp-in-string "<\.*>" ""
							 (buffer-name)))
  (setq pdj--rel-dir (replace-regexp-in-string
		      pdj:project-directory "" (buffer-file-name)))
  (setq pdj--rel-dir (replace-regexp-in-string
			 (concat "/" pdj--clean-buffer-name) ""
			 pdj--rel-dir))
  pdj--rel-dir)


(defun pdj:py-module ()
  "The python module for the current buffer"

  (defvar pdj--rel-dir)
  (defvar pdj--py-module)

  (interactive)

  (setq pdj--rel-dir (replace-regexp-in-string
		      pdj:project-directory "" (buffer-file-name)))
  (setq pdj--py-module (replace-regexp-in-string "/" "." pdj--rel-dir))
  (setq pdj--py-module (replace-regexp-in-string ".py" "" pdj--py-module))
  (string-trim pdj--py-module))


(defun pdj:py-file ()
  "Relative path for the file displayed in the current buffer"

  (defvar pdj--rel-dir)

  (setq pdj--rel-dir (replace-regexp-in-string
		      pdj:project-directory "" (buffer-file-name)))
  pdj--rel-dir)


(defun pdj:pytest-func ()
  "The test function name formatted for use in pytest cmd line."

  (replace-regexp-in-string "\\." "::" (which-function)))


(defun pdj:py-test-suite-under-cursor ()
  "The full qualified name for the test suite under the cursor"

  (defvar pdj--test-suite)

  (interactive)
  (save-excursion
    (if (equal (thing-at-point 'line) "\n")
	(forward-line -1))
    (if (string-equal pdj:test-command "pytest")
	(setq pdj--test-suite (concat (pdj:py-file) "::" (pdj:pytest-func)))
      (setq pdj--test-suite (concat (pdj:py-module)
				    (concat "." (which-function)))))
    (string-trim pdj--test-suite)))


(defun pdj:run-ipython ()
  "Runs python shell using ipython and lets default-directory as
   `pdj:project-directory'."

  (interactive)

  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "--simple-prompt --pprint")

  (pdj:execute-on-project-directory
   'run-python python-shell-interpreter nil nil))



(defun pdj:py-debug-test ()
  "Run the current test under cursor in a realgud:pdb-delayed session.
If `insert-breakpoint', inserts a breakpoint at point."
  (interactive)

  (let* ((initial-debugger python-shell-interpreter)
	 (actual-debugger "pdb")
	 (test-suite-fn (or pdj:py-test-under-cursor-fn
			    'pdj:py-test-suite-under-cursor))
	 (test-suite (apply test-suite-fn ()))
	 (cmd-str (concat pdj:test-command " " pdj:py-test-suite-prefix
			  test-suite))
	 (cmd-args (split-string-and-unquote cmd-str))
	 ;; XXX: python gets registered as the interpreter rather than
	 ;; a debugger, and the debugger position (nth 1) is missing:
	 ;; the script-args takes its place.
	 (parsed-args (pdb-parse-cmd-args cmd-args))
	 (script-args (nth 1 parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
    	  (cl-remove-if 'nil (realgud:flatten parsed-args))))
    (pdj:realgud-run-process actual-debugger script-name parsed-cmd-args
			     'realgud:pdb-minibuffer-history)))


(defun pdj:py-breakpoint-at-point ()
  "Inserts a 'breakpoint()' at point."

  (defvar pdj--py-breakpoint "breakpoint()")

  (unless (string-match-p pdj--py-breakpoint (thing-at-point 'line))
    (if (equal (thing-at-point 'line) "\n")
	(progn
	  (indent-for-tab-command)
	  (insert pdj--py-breakpoint)
	  (beginning-of-line))
      (end-of-line)
      (insert "\n")
      (indent-for-tab-command)
      (insert pdj--py-breakpoint)
      (beginning-of-line)))

  (let ((before-save-hook nil))
    (save-buffer)))


(defun pdj:py-debug-test-with-breakpoint ()
  "Inserts a breakpoint at point and executes `pdj:py-debug-test'"
  (interactive)
  (pdj:py-breakpoint-at-point)
  (pdj:py-debug-test))


(defun pdj:py-run-tests (&optional test-suite)
  "Run tests. If `test-suite' only tests from this suite will be executed."

  (interactive)

  (setq pdj:test-suite-prefix pdj:py-test-suite-prefix)
  (pdj:run-test-suite test-suite))


(defun pdj:py-run-tests-all ()
  "Runs all tests from the project"
  (interactive)

  (pdj:py-run-tests))


(defun pdj:py-run-tests-package ()
  "Run tests for the package of the current buffer."

  (interactive)

  (if (string-equal pdj:test-command "pytest")
      (pdj:run-tests (pdj:py-dir))
    (pdj:py-run-tests (pdj:py-package))))


(defun pdj:py-run-tests-module ()
  "Run tests for the current buffer."

  (interactive)
  (if (string-equal pdj:test-command "pytest")
      (pdj:py-run-tests (pdj:py-file))
    (pdj:py-run-tests (pdj:py-module))))


(defun pdj:py-run-test-suite ()
  "Run test suite under the cursor."

  (interactive)
  (let ((test-suite-fn (or pdj:py-test-under-cursor-fn
			   'pdj:py-test-suite-under-cursor)))
    (pdj:py-run-tests (apply test-suite-fn ()))))


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


(defun pdj:py-upload-to-pypi ()
  "Creates a package and uploads to pypi"

  (interactive)

  ;; here will the the upload to pypi command plus the file name
  (defvar pdj:--upload-file-command nil)
  ;; the package that will be created
  (defvar pdj:--new-package-file nil)
  ;; buffer output for create package. Used to know the package name.
  (defvar pdj:--buf-out nil)
  (defvar pdj:--packaging-buffer-name "Packaging")

  ;; sentinel to the create package process.
  ;; when the process finishes without errors upload the file to pypi
  (defun pdj:--upload-package (process event)
    (if (equal event "finished\n")
	(progn
	  (setq pdj:--buf-out (buffer-substring-no-properties 1 (buffer-size)))
	  (string-match "creating \\(.*/\\)"
			(buffer-substring-no-properties 1 (buffer-size)))
	  ;; the file name like myproject-0.1.tar.gz
	  (setq pdj:--new-package-file
		(concat (replace-regexp-in-string " " "" (match-string 1))
			".tar.gz"))
	  ;; file name like dist/myproject-0.1.tar.gz
	  (setq pdj:--new-package-file
		(concat pdj:py-dist-dir pdj:--new-package-file))

	  ;; upload command like twine upload dist/myproject-0.1.tar.gz
	  (setq pdj:--upload-file-command
		(concat pdj:py-upload-to-pypi-command
			(concat " " pdj:--new-package-file)))

	  (pdj:run-in-term-on-project-directory pdj:--upload-file-command
	  					pdj:--packaging-buffer-name)
	  (setq pdj:--new-package-file nil))))

  ;; creating the package
  (pdj:run-in-term-on-project-directory pdj:py-package-command
					pdj:--packaging-buffer-name)

  (set-process-sentinel (get-process pdj:--packaging-buffer-name)
			'pdj:--upload-package))


;; :enable for test or debug suite menu.
(defun pdj:--is-test-suite ()
  "Indicates if the suite under cursor is a test suite."

  (defvar pdj--func-name)

  (setq pdj--func-name (car(last (split-string (which-function) "\\."))))
  (if (or (equal 0 (string-match-p "test" pdj--func-name))
	  (string-match-p "class" (thing-at-point 'line))) t))

;; Menus

(defun pdj:py-create-menu (keymap)
  "Recriates the Python menu changing some of its elements."

  (when (eq keymap nil)
    (setq keymap python-mode-map))
  (interactive)
  ;; removing python.el menu
  (define-key keymap [menu-bar Python] nil)

  ;; new python menu
  (define-key keymap [menu-bar pdj-python]
    (cons "Python" (make-sparse-keymap "Python")))

  ;; shift region stuff
  (define-key keymap [menu-bar pdj-python shift-region-left]
    '(menu-item "Shift region left" python-indent-shift-left
		:enable mark-active
		:help "Shift region left by a single indentation step"))

  (define-key-after keymap [menu-bar pdj-python shift-region-right]
    '(menu-item "Shift region right" python-indent-shift-right
		:enable mark-active
		:help "Shift region left by a single indentation step")
    'shift-region-left)

  (define-key-after keymap [menu-bar pdj-python first-separator]
    '(menu-item "--") 'shift-region-right)

  ;; start/end/definitions of functions/methods/classes
  (define-key-after keymap [menu-bar pdj-python start-of-defun]
    '(menu-item "Start of def/class" beginning-of-defun
		:help "Go to start of outermost definition around point")
    'first-separator)

  (define-key-after keymap [menu-bar pdj-python end-of-defun]
    '(menu-item "End of def/class" end-of-defun
		:help "Go to end of definition around point")
    'start-of-defun)

  (define-key-after keymap [menu-bar pdj-python mark-def-class]
    '(menu-item "Mark def/class" mark-defun
		:help "Mark outermost definition around point")
    'end-of-defun)

  (define-key-after keymap [menu-bar pdj-python jump-def-class]
    '(menu-item "Jump to def/class" imenu
		:help "Mark outermost definition around point")
    'mark-def-class)

  (define-key-after keymap [menu-bar pdj-python second-separator]
    '(menu-item "--") 'jump-def-class)

  ;; shell related stuff
  (define-key-after keymap [menu-bar pdj-python switch-to-shell]
    '(menu-item "Switch to Python shell" pdj:py-switch-to-shell
		:help "Switch to inferior Python process.")
    'second-separator)

  (define-key-after keymap [menu-bar pdj-python py-eval-buffer]
    '(menu-item "Eval buffer" pdj:py-eval-buffer
		:help "Eval buffer in inferior Python session")
    'py-eval-string)

  (define-key-after keymap [menu-bar pdj-python py-eval-region]
    '(menu-item "Eval region" pdj:py-eval-region
		:enable mark-active
		:help "Eval region in inferior Python session")
    'py-eval-buffer)

  (define-key-after keymap [menu-bar pdj-python py-eval-defun]
    '(menu-item "Eval def/class" pdj:py-eval-defun
		:help "Eval defun in inferior Python session")
    'py-eval-region)

  (define-key-after keymap [menu-bar pdj-python py-eval-file]
    '(menu-item "Eval file" pdj:py-eval-file
		:help "Eval defun in inferior Python session")
    'py-eval-defun)

  (define-key-after keymap [menu-bar pdj-python third-separator]
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

  (define-key-after keymap [menu-bar pdj-python testing]
    (list 'menu-item "Testing" menu-bar-pdj-python-testing))

  ;; Debug
  (defvar menu-bar-pdj-python-debug (make-sparse-keymap "Debug"))

  (define-key menu-bar-pdj-python-debug [debug-test-suite]
    '(menu-item "Debug test suite" pdj:py-debug-test-with-breakpoint
		:enable (progn  (which-function))
		:help "Inserts a breakpoint at point and run test suite on debug frame"))

  (define-key-after menu-bar-pdj-python-debug [debug-test-suite-no-ipdb]
    '(menu-item "Debug test suite (no breakpoint)" pdj:py-debug-test
		:enable (progn  (which-function))
		:help "Runs test suite on debug frame")
    'debug-test-suite)

  (define-key-after keymap [menu-bar pdj-python debug]
    (list 'menu-item "Debug" menu-bar-pdj-python-debug))

  ;; Packaging
  (defvar menu-bar-pdj-python-packaging (make-sparse-keymap "Packaging"))

  (define-key menu-bar-pdj-python-packaging [upload-to-pypi]
    '(menu-item "Upload to PyPI" pdj:py-upload-to-pypi
		:help "Uploads the package to PyPI"))

  (define-key-after keymap [menu-bar pdj-python packaging]
    (list 'menu-item "Packaging" menu-bar-pdj-python-packaging)))


;; Hooks

(defun pdj:py-add-custom-keywords ()
  "Adds custom keywords, defined in `pdj:py-custom-keywords', for Python."

  (font-lock-add-keywords 'python-mode pdj:py-custom-keywords))


(defun pdj:py-remove-custom-keywords ()
  "Removes custom keywords from Python."

  (font-lock-remove-keywords 'python-mode pdj:py-custom-keywords))


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


(defun pdj:py-activate-on-pyproject ()
  (when (string= (buffer-name) "pyproject.toml")
    (pdj:py-all-hooks)
    (pdj:load-custom-commands)))

(defun pdj:py-keyboard-hooks ()
  "Custom key bindings. The following bindings are done here:

   * The bindings from pdj:prog-keyboard-hooks
   * `C-c p' - pdj:py-run-tests-package
   * `C-c s' - pdj:py-run-test-suite
   * `C-c m' - 'pdj:py-run-tests-module
   * `C-c d' - 'pdj:py-debug-test
   * `C-c C-d' - pdj:py-debug-test-with-breakpoint
   * `C-c x' - 'radon"

  (pdj:prog-keyboard-hooks)
  (local-set-key (kbd "C-c p") 'pdj:py-run-tests-package)
  (local-set-key (kbd "C-c s") 'pdj:py-run-test-suite)
  (local-set-key (kbd "C-c m") 'pdj:py-run-tests-module)
  (local-set-key (kbd "C-c d") 'pdj:py-debug-test)
  (local-set-key (kbd "C-c C-d") 'pdj:py-debug-test-with-breakpoint)
  (local-set-key (kbd "C-c x") 'radon)
  (local-set-key (kbd "C-c C-z") 'pdj:py-switch-to-shell))


(defun pdj:py-all-hooks ()
  "Enables all pdj:py hooks. They are:

  * pdj:py-venv-hooks
  * pdj:py-keyboard-hooks
  * pdj:py-flycheck-hooks
  * pdj:py-ac-hooks
  * pdj:radon-hooks
  * pdj:py-create-menu
  * pdj:py-add-custom-keywords
  * pdj:py-set-faces
  * pdj:enable-autopep8"


  (pdj:py-venv-hooks)
  (pdj:py-keyboard-hooks)
  (pdj:py-flycheck-hooks)
  (pdj:py-ac-hooks)
  (pdj:radon-hooks)
  (pdj:py-set-test-command)
  (pdj:add-project-dir-to-python-path)
  (pdj:py-create-menu python-mode-map)
  (pdj:py-set-faces)
  (pdj:enable-autopep8)
  (pdj:pylsp-hooks)
  (pdj:py-add-custom-keywords))


(defun pdj:py-setup ()
  "Adds pdj:py-all-hooks to python-mode-hook and pdj:py-activate-on-pyproject
 to conf-toml-mode-hook"

  (add-hook 'python-mode-hook 'pdj:py-all-hooks)
  (add-hook 'conf-toml-mode-hook 'pdj:py-activate-on-pyproject))


(defun pdj:py-deactivate ()
  "Undo everything done for python."

  (pdj:py-reset-customvars)
  (pdj:py-deactivate-venv-hooks)
  (pdj:py-deactivate-ac-hooks)
  (pdj:py-deactivate-flycheck-hooks)
  (pdj:disable-autopep8)
  (pdj:py-remove-project-dir-from-python-path)
  (pdj:py-remove-custom-keywords))


(provide 'pdj-python)
