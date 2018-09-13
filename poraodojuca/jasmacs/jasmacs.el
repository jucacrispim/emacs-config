(require 'pdj-venv)

(defcustom jasmacs:environment-name "jasmacs"
  "The name for the environment that will be used by the jasmacs server")

(defcustom jasmacs:theme "dark"
  "The theme for the html report of jasmacs")

(defcustom jasmacs:server-port "2345"
  "The port for the jasmacs server to listen")

(defcustom jasmacs:use-dark-theme t
  "Indicates the jasmacs server should use the dark css theme.")

(defcustom jasmacs:jasmine-yaml-path nil
  "The path for your jasmine.yml file. If nil will look for it it
in your project root dir. May be customized via .dir-locals.el too")

(defcustom jasmacs:py-exec "/usr/bin/python3"
  "The path for the python executable")

(defcustom jasmacs:buffer-name "jasmacs"
  "The name of the buffer used to display the jasmacs output")

(defcustom jasmacs:environment-dir
  (expand-file-name "~/.emacs.d/.python-environments")
  "The directory inside which we create the jasmacs environment")

(defconst jasmacs:code-directory (file-name-directory
				  (or load-file-name buffer-file-name)))

(defconst jasmacs:install-server-command
  (concat "pip install " jasmacs:code-directory "pylib/"))

(defconst jasmacs:start-server-command "jasmacs server")

(defvar jasmacs:--orig-venv-name)


(defun jasmacs:--create-py-env ()
  (deferred:$
    (deferred:next
      (let ((venv-location jasmacs:environment-dir))
	(setq jasmacs:--orig-venv-name venv-current-name)
	(pdj:venv-mkvirtualenv jasmacs:py-exec jasmacs:environment-name))
      (if jasmacs:--orig-venv-name
	  (venv-workon jasmacs:--orig-venv-name)
	(venv-deactivate)))))

(defun jasmacs:--run-in-py-env (cmd)
  (deferred:$
    (deferred:next
      (unless (get-buffer-process "*jasmacs*")
	(let ((venv-location jasmacs:environment-dir))
	(setq jasmacs:--orig-venv-name venv-current-name)
	(venv-workon jasmacs:environment-name)
	(setq pdj:multi-term-switch-to-buffer nil)
	(pdj:run-in-term-on-project-directory cmd jasmacs:buffer-name)
	(setq pdj:multi-term-switch-to-buffer t))
      (if jasmacs:--orig-venv-name
	  (venv-workon jasmacs:--orig-venv-name)
	(venv-deactivate))))))

(defun jasmacs:install-server ()
  (interactive)

  (deferred:$
    (jasmacs:--create-py-env)

    (deferred:nextc it
      (lambda ()
	(message (concat "Installing Jasmacs with "
			 jasmacs:install-server-command))
	(jasmacs:--run-in-py-env jasmacs:install-server-command)))))

(defun jasmacs:start-server ()
  (interactive)

  (hack-local-variables)

  (defvar jasmacs:--jasmine-yaml-path)
  (defvar jasmacs:--start-server-command)

  (if jasmacs:jasmine-yaml-path
      (progn ()
	     (setq jasmacs:--jasmine-yaml-path jasmacs:jasmine-yaml-path)
	     )
    (setq jasmacs:--jasmine-yaml-path (concat pdj:project-directory
					      "jasmine.yml")))

  (setq jasmacs:--start-server-command (concat jasmacs:start-server-command
					       " -c " jasmacs:--jasmine-yaml-path
					       " -p " jasmacs:server-port
					       " -r " pdj:project-directory))
  (when jasmacs:use-dark-theme
    (setq jasmacs:--start-server-command (concat jasmacs:--start-server-command
						 " -t dark")))

  (message (concat "Starting Jasmacs with" jasmacs:--start-server-command))
  (deferred:$
    (jasmacs:--run-in-py-env jasmacs:--start-server-command)))


(defun jasmacs:install-if-needed-and-start ()
  (if (not (file-exists-p (concat jasmacs:environment-dir "/"
			     jasmacs:environment-name)))
      (lambda ()
	(deferred:$
	  (jasmacs:install-server)

	  (deferred:nextc it
	    (jasmacs:start-server))))
    (jasmacs:start-server)))

(defun jasmacs:tests-url (&optional rest)
  (interactive)

  (setq jasmacs:--tests-url (concat "http://localhost:" jasmacs:server-port))
  (when rest
    (setq jasmacs:--tests-url (concat "?" rest)))
  jasmacs:--tests-url)


(defun jasmacs:run-all-tests ()
  (interactive)

  (xwidget-webkit-browse-url (jasmacs:tests-url)))


(defun jasmacs:keyboard-hooks ()
  (local-set-key (kbd "C-c p") 'jasmacs:run-all-tests))


(defun jasmacs:setup ()
  (jasmacs:keyboard-hooks)
  (jasmacs:install-if-needed-and-start))


(provide 'jasmacs)
