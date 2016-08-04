;; Install required packages for pdj config

(require 'package)
(package-initialize)

(defcustom pdj:first-run-file "~/.emacs.d/firstrun"
  "File that indicates if pdj:boostrap was already done.")


(defun pdj:print (msg)
  "Print to buffer with a new line"
  (insert (concat msg "\n")))


(defun pdj:install-if-needed (pkgrequire)
  "Installs a package if it is not already installed."

  (unless (require pkgrequire nil 'noerror)
    (insert (concat (concat "Obtaining "  (symbol-name pkgrequire)) "\n"))
    (package-install pkgrequire)))


(defun pdj:bootstrap ()

  (interactive)


  (unless (file-exists-p pdj:first-run-file)
    (setq output-buffer (get-buffer-create "*Emacs church service*"))

    (switch-to-buffer output-buffer)
    (add-to-list 'package-archives
    		 '("melpa" . "http://melpa.org/packages/"))

    (setq emacs-banner-bootstrap (create-image "splash.svg"))

    (insert-image emacs-banner-bootstrap)
    (pdj:print "\n")
    (pdj:print "Blessings, my son. Welcome to the Emacs church.")
    (pdj:print "You need to acquire some canons. Be patient.\n")

    (package-refresh-contents)

    (pdj:install-if-needed 'virtualenvwrapper)
    (pdj:install-if-needed 'auto-complete)
    (pdj:install-if-needed 'jedi)
    (pdj:install-if-needed 'yasnippet)
    (pdj:install-if-needed 'color-theme)
    (pdj:install-if-needed 'browse-kill-ring)
    (pdj:install-if-needed 'py-autopep8)
    (pdj:install-if-needed 'flycheck)
    (pdj:install-if-needed 'magit)
    (pdj:install-if-needed 'buffer-move)

    (pdj:print "\nAll canons obtained.")
    (pdj:print "Happy hacking and may St. Ignutius bless you.")
    (append-to-file "first run ok" nil pdj:first-run-file)))


(provide 'pdj-bootstrap)
