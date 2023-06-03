;; module that handles the creation of new 'projects'

(require 'pdj-utils)
(require 'pdj-python)
(require 'pdj-venv)

(defcustom pdj:prj-default-projects-dir (expand-file-name "~/mysrc/")
  "Default directory for projects.")

(defcustom pdj:prj-templates-dir (expand-file-name
				  "~/.emacs.d/poraodojuca/templates/")
  "Directory where the templates are stored.")

;;;;;;;;;;;;;;;;;;
;; Common stuff ;;
;;;;;;;;;;;;;;;;;;

(defun pdj:prj-get-template-file (fname)

  (setq pdj:prj--template-path nil)

  (setq pdj:prj--template-file-name fname)
  (setq pdj:prj--template-path (concat pdj:prj-templates-dir fname)))


(defun pdj:prj-write-dir-locals (type vars dest-dir)
  "Writes a .dir-locals.el file in `dest-dir'. `type' is used
to decide which template use as base and replace `vars' in the
template."

  (setq pdj:prj--template-contents nil)
  (setq pdj:prj--template-key nil)
  (setq pdj:prj--template-value nil)
  (setq pdj:prj--ret nil)

  (setq pdj:prj--template-contents (pdj:read-file
				    (pdj:prj-get-template-file
				     (concat type "-" "dir-locals.el.tmpl"))))

  (dolist (element vars pdj:prj--ret)
    (setq pdj:prj--template-key (car element))
    (setq pdj:prj--template-value (car (cdr element)))
    (setq pdj:prj--template-contents (replace-regexp-in-string
				      pdj:prj--template-key
				      pdj:prj--template-value
				      pdj:prj--template-contents t)))

  (setq pdj:prj--ret (concat dest-dir "/" ".dir-locals.el"))

  (write-region pdj:prj--template-contents nil pdj:prj--ret)
  pdj:prj--ret)


(defun pdj:prj-create-readme-file (readme-msg dest-dir)
  "Creates a README file in `dest-dir' using `readme-msg' as its
content."

  (setq pdj:prj--readme-path (concat dest-dir "/" "README"))

  (write-region readme-msg nil pdj:prj--readme-path))

;;;;;;;;;;;;;;;;
;; go project ;;
;;;;;;;;;;;;;;;;

(defun pdj:prj-create-go-project ()
  "Creates a new go project. Creates the project directory and a
.dir-locals.el file."

  (interactive)

  (setq pdj:prj--project-dir nil)
  (setq pdj:prj--project-name nil)
  (setq pdj:prj--test-command nil)
  (setq pdj:prj--custom-commands nil)
  (setq pdj:prj--template-vars nil)
  (setq pdj:prj--dir-locals-file nil)
  (setq pdj:prj--import-base nil)

  ;; ask info

  (setq pdj:prj--project-name (pdj:ask "Project name"))
  (setq pdj:prj--project-dir (pdj:ask "Project directory"
				      (concat pdj:prj-default-projects-dir
					      pdj:prj--project-name "/")))
  (setq pdj:prj--import-base (pdj:ask "Import base" nil))
  (setq pdj:prj--test-command (pdj:ask "Test command" "go test ./... -v"))
  (setq pdj:prj--coverage-command (pdj:ask
				   "Coverage command"
				   "make coverage"))
  (setq pdj:prj--custom-commands (symbol-name
				  (y-or-n-p "Use custom commands?")))

  ;; create project dir
  (unless (file-exists-p pdj:prj--project-dir)
    (make-directory pdj:prj--project-dir t))

  ;; set variables to replace on templates
  (setq pdj:prj--template-vars
	`(("{{PROJECT-DIRECTORY}}" ,pdj:prj--project-dir)
	  ("{{TEST-COMMAND}}" ,pdj:prj--test-command)
	  ("{{COVERAGE-COMMAND}}" ,pdj:prj--coverage-command)
	  ("{{PROJECT-NAME}}" ,pdj:prj--project-name)
	  ("{{IMPORT-BASE}}" ,pdj:prj--import-base)))

  (if pdj:prj--custom-commands
      (push `("{{CUSTOM-COMMANDS}}" ,pdj:prj--custom-commands)
	    pdj:prj--template-vars))


  ;; write .dir-locals.el
  (setq pdj:prj--dir-locals-file (pdj:prj-write-dir-locals
				  "go"
				  pdj:prj--template-vars
				  pdj:prj--project-dir))

  ;; save as safe values
  (push `(pdj:project-directory . ,pdj:prj--project-dir)
	safe-local-variable-values)

  (push `(pdj:test-command . ,pdj:prj--test-command)
	safe-local-variable-values)


  (put 'safe-local-variable-values 'customized-value
       (list (custom-quote (symbol-value 'safe-local-variable-values))))

  (customize-save-variable
   'safe-local-variable-values safe-local-variable-values)

  ;; remove useless stuff from .dir-locals
  (set-buffer (find-file-noselect pdj:prj--dir-locals-file))
  (while (re-search-forward "(pdj:custom-commands . \"nil\")" nil t)
    (replace-match ""))

  ;; creating readme
  (pdj:prj-create-readme-file "My cool Go project"
			      pdj:prj--project-dir)

  ;; creating Makefile
  (unless (file-exists-p (concat pdj:prj--project-dir "/Makefile" ))
    (pdj:prj--go-write-makefile pdj:prj--template-vars
				pdj:prj--project-dir))

  ;; creating scripts stuff
  (unless (file-exists-p (concat pdj:prj--project-dir "/scripts"))
    (make-directory (concat pdj:prj--project-dir "/scripts" ) t))

  (unless (file-exists-p (concat pdj:prj--project-dir "/scripts/env.sh"))
    (pdj:prj--go-write-env-sh pdj:prj--template-vars
			      (concat pdj:prj--project-dir "/scripts/")))

  (unless (file-exists-p (concat pdj:prj--project-dir "/scripts/check_coverage.sh"))
    (pdj:prj--go-write-check-coverage-sh pdj:prj--template-vars
					 (concat pdj:prj--project-dir, "/scripts/"))))


(defun pdj:prj--go-write-makefile (vars dest-dir)
  "Writes a Makefile at `dest-dir'. `vars' are the variables we should replace
in the template."

  (setq pdj:prj--template-contents nil)
  (setq pdj:prj--template-key nil)
  (setq pdj:prj--template-value nil)
  (setq pdj:prj--ret nil)

  (setq pdj:prj--template-contents (pdj:read-file
				    (pdj:prj-get-template-file "Makefile.tmpl")))


  (dolist (element vars pdj:prj--ret)
    (setq pdj:prj--template-key (car element))
    (setq pdj:prj--template-value (car (cdr element)))
    (setq pdj:prj--template-contents (replace-regexp-in-string
				      pdj:prj--template-key
				      pdj:prj--template-value
				      pdj:prj--template-contents t)))

  (setq pdj:prj--ret (concat dest-dir "/" "Makefile"))

  (write-region pdj:prj--template-contents nil pdj:prj--ret)
  pdj:prj--ret)


(defun pdj:prj--go-write-env-sh (vars dest-dir)
  "Writes a env.sh at `dest-dir'. `vars' are the variables we should replace
in the template."

  (setq pdj:prj--template-contents nil)
  (setq pdj:prj--template-key nil)
  (setq pdj:prj--template-value nil)
  (setq pdj:prj--ret nil)

  (setq pdj:prj--template-contents (pdj:read-file
				    (pdj:prj-get-template-file "env-go.sh.tmpl")))

  (dolist (element vars pdj:prj--ret)
    (setq pdj:prj--template-key (car element))
    (setq pdj:prj--template-value (car (cdr element)))
    (setq pdj:prj--template-contents (replace-regexp-in-string
				      pdj:prj--template-key
				      pdj:prj--template-value
				      pdj:prj--template-contents t)))

  (setq pdj:prj--ret (concat dest-dir "env.sh"))

  (write-region pdj:prj--template-contents nil pdj:prj--ret)
  pdj:prj--ret)


(defun pdj:prj--go-write-check-coverage-sh (vars dest-dir)
  "Writes a env.sh at `dest-dir'. `vars' are the variables we should replace
in the template."

  (setq pdj:prj--template-contents nil)
  (setq pdj:prj--template-key nil)
  (setq pdj:prj--template-value nil)
  (setq pdj:prj--ret nil)

  (setq pdj:prj--template-contents (pdj:read-file
				    (pdj:prj-get-template-file "check_coverage-go.sh.tmpl")))

  (dolist (element vars pdj:prj--ret)
    (setq pdj:prj--template-key (car element))
    (setq pdj:prj--template-value (car (cdr element)))
    (setq pdj:prj--template-contents (replace-regexp-in-string
				      pdj:prj--template-key
				      pdj:prj--template-value
				      pdj:prj--template-contents t)))

  (setq pdj:prj--ret (concat dest-dir "check_coverage.sh"))

  (write-region pdj:prj--template-contents nil pdj:prj--ret)
  pdj:prj--ret)


;;;;;;;;;;;;;;;;;;;;
;; python project ;;
;;;;;;;;;;;;;;;;;;;;

(defun pdj:prj-create-python-project ()
  "Creates a new python project. Creates the project directory  and a setup
file if they do not exist, creates a virtual env and install the dependencies
listed in a requirements file using pip."

  (interactive)

  (setq pdj:prj--project-dir nil)
  (setq pdj:prj--project-name nil)
  (setq pdj:prj--venv-name nil)
  (setq pdj:prj--venv-py-version nil)
  (setq pdj:prj--requirements-file nil)
  (setq pdj:prj--test-command nil)
  (setq pdj:prj--coverage-command nil)
  (setq pdj:prj--py-autopep8 nil)
  (setq pdj:prj--custom-commands nil)
  (setq pdj:prj--template-vars nil)
  (setq pdj:prj--dir-locals-file nil)
  (setq pdj:prj--py-main-package-path nil)
  (setq pdj:prj--test-suite-prefix nil)

  ;; first we ask the info we need.
  (setq pdj:prj--project-name (pdj:ask "Project name"))
  (setq pdj:prj--project-dir (pdj:ask "Project directory"
				   (concat pdj:prj-default-projects-dir
					   pdj:prj--project-name "/")))
  (setq pdj:prj--venv-name (pdj:ask "Venv name" pdj:prj--project-name))
  (setq pdj:prj--venv-py-version (pdj:ask "Python version" "python3"))
  (setq pdj:prj--requirements-file (pdj:ask
				   "Requirements file" "requirements.txt"))
  (setq pdj:prj--test-command (pdj:ask "Test command" "pytest"))
  (setq pdj:prj--test-suite-prefix (pdj:ask
				   "Test suite prefix"
				   " "))

  (setq pdj:prj--coverage-command (pdj:ask
				   "Coverage command"
				   "sh ./build-scripts/check_coverage.sh"))
  (setq pdj:prj--py-autopep8 (symbol-name (y-or-n-p "Use autopep8?")))
  (setq pdj:prj--custom-commands (symbol-name
				  (y-or-n-p "Use custom commands?")))

  (setq pdj:prj--template-vars
	`(("{{PROJECT-DIRECTORY}}" ,pdj:prj--project-dir)
	  ("{{VENV-NAME}}" ,pdj:prj--venv-name)
	  ("{{TEST-COMMAND}}" ,pdj:prj--test-command)
	  ("{{TEST-SUITE-PREFIX}}" ,pdj:prj--test-suite-prefix)
	  ("{{PROJECT-NAME}}" ,pdj:prj--project-name)
	  ("{{COVERAGE-COMMAND}}" ,pdj:prj--coverage-command)))

  (if pdj:prj--py-autopep8
      (push `("{{PY-AUTOPEP8}}" ,pdj:prj--py-autopep8)
	    pdj:prj--template-vars))

  (if pdj:prj--custom-commands
      (push `("{{CUSTOM-COMMANDS}}" ,pdj:prj--custom-commands)
	    pdj:prj--template-vars))

  ;; now we have everthing needed, lets start.
  ;; creating the project dir
  (unless (file-exists-p
	   (concat pdj:prj--project-dir "/" pdj:prj--project-name))
    (make-directory (concat pdj:prj--project-dir "/" pdj:prj--project-name) t))

  ;; here we write a .dir-locals.el file
  (setq pdj:prj--dir-locals-file (pdj:prj-write-dir-locals
				  "py"
				  pdj:prj--template-vars
				  pdj:prj--project-dir))

  ;; setting local safe vars
  (push `(pdj:project-directory . ,pdj:prj--project-dir)
	safe-local-variable-values)
  (push `(pdj:venv-name . ,pdj:prj--venv-name)
	safe-local-variable-values)
  (push `(pdj:test-command . ,pdj:prj--test-command)
	safe-local-variable-values)
  (push `(pdj:coverage-command . ,pdj:prj--coverage-command)
	safe-local-variable-values)
  (push `(pdj:py-autopep8 . ,pdj:prj--py-autopep8)
	safe-local-variable-values)
  (push `(pdj:test-suite-prefix . ,pdj:prj--test-suite-prefix)
	safe-local-variable-values)
  (put 'safe-local-variable-values 'customized-value
       (list (custom-quote (symbol-value 'safe-local-variable-values))))

  ;; saving stuff
  (customize-save-variable
   'safe-local-variable-values safe-local-variable-values)

  ;; removing stuff that was not setted.
  (set-buffer (find-file-noselect pdj:prj--dir-locals-file))
  (while (re-search-forward "(pdj:py-autopep8 . \"nil\")" nil t)
    (replace-match ""))

  (set-buffer (find-file-noselect pdj:prj--dir-locals-file))
  (while (re-search-forward "(pdj:custom-commands . \"nil\")" nil t)
    (replace-match ""))

  ;; creating the pyproject.toml file
  (unless (file-exists-p (concat pdj:prj--project-dir "pyproject.toml"))
    (pdj:prj-write-pyproject-toml pdj:prj--template-vars pdj:prj--project-dir))


  ;; creating readme
  (pdj:prj-create-readme-file "My awesome Python project"
			      pdj:prj--project-dir)

  ;; creating requirements
  (unless (file-exists-p
	   (concat pdj:prj--project-dir pdj:prj--requirements-file))
    (pdj:prj-create-py-requirements-file pdj:prj--requirements-file
					 pdj:prj--project-dir))


  ;; creating the virtualenv
  (unless (member pdj:prj--venv-name (venv-get-candidates))
    (pdj:venv-mkvirtualenv pdj:prj--venv-py-version pdj:prj--venv-name))

  (venv-workon pdj:prj--venv-name)
  ;; installing the requirements of the project
  (setq pdj:py-requirements-file pdj:prj--requirements-file)
  (hack-local-variables)
  (unless (boundp 'pdj:project-directory)
    (setq pdj:project-directory pdj:prj--project-dir))

  (pdj:py-bootstrap))


(defun pdj:prj-write-setup-py (vars dest-dir)
  "Writes a setup.py file in `dest-dir'. `vars' are the
variables we should replace in the template."

  (setq pdj:prj--template-contents nil)
  (setq pdj:prj--template-key nil)
  (setq pdj:prj--template-value nil)
  (setq pdj:prj--ret nil)

  (setq pdj:prj--template-contents (pdj:read-file
				    (pdj:prj-get-template-file "setup.py.tmpl")))

  (dolist (element vars pdj:prj--ret)
    (setq pdj:prj--template-key (car element))
    (setq pdj:prj--template-value (car (cdr element)))
    (setq pdj:prj--template-contents (replace-regexp-in-string
				      pdj:prj--template-key
				      pdj:prj--template-value
				      pdj:prj--template-contents t)))

  (setq pdj:prj--ret (concat dest-dir "/" "setup.py"))

  (write-region pdj:prj--template-contents nil pdj:prj--ret)
  pdj:prj--ret)

(defun pdj:prj-write-pyproject-toml (vars dest-dir)
  "Writes a pyproject.toml file in `dest-dir'. `vars' are the
variables we should replace in the template."

  (setq pdj:prj--template-contents nil)
  (setq pdj:prj--template-key nil)
  (setq pdj:prj--template-value nil)
  (setq pdj:prj--ret nil)

  (setq pdj:prj--template-contents (pdj:read-file
				    (pdj:prj-get-template-file "pyproject.toml.tmpl")))

  (dolist (element vars pdj:prj--ret)
    (setq pdj:prj--template-key (car element))
    (setq pdj:prj--template-value (car (cdr element)))
    (setq pdj:prj--template-contents (replace-regexp-in-string
				      pdj:prj--template-key
				      pdj:prj--template-value
				      pdj:prj--template-contents t)))

  (setq pdj:prj--ret (concat dest-dir "/" "pyproject.toml"))

  (write-region pdj:prj--template-contents nil pdj:prj--ret)
  pdj:prj--ret)


(defun pdj:prj-create-py-requirements-file (req-fname dest-dir)
  "Creates an empty requirements file for a python project."

  (setq pdj:prj--new-requirements-file (concat dest-dir "/" req-fname))

  (write-region "# put your requirements here\n" nil
		pdj:prj--new-requirements-file))



;;;;;;;;;;
;; menu ;;
;;;;;;;;;;

(defun pdj:prj-create-menu ()
  "Creates a Development menu with pdj:prj functions."

  (interactive)

  ;; new Project menu
  (define-key-after global-map [menu-bar pdj-prj]
    (cons "Project" (make-sparse-keymap "Project")) 'Tools)

  ;; add projects
  (setq menu-bar-pdj-prj-add (make-sparse-keymap "Add"))

  (define-key menu-bar-pdj-prj-add [add-go]
    '(menu-item "Go project" pdj:prj-create-go-project
		:help "Adds an existing dir as a go project"))

  (define-key menu-bar-pdj-prj-add [add-python]
    '(menu-item "Python project" pdj:prj-create-python-project
		:help "Adds an existing dir as a python project"))

  (define-key global-map [menu-bar pdj-prj add]
    (list 'menu-item "Add" menu-bar-pdj-prj-add)))


(defun pdj:prj-setup ()

  (pdj:prj-create-menu))


(provide 'pdj-project)
