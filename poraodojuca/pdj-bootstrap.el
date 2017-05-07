;; Install required packages for pdj config

(require 'package)
(require 'teletype)

;;;(package-initialize)

(defcustom pdj:first-run-file "~/.emacs.d/firstrun"
  "File that indicates if pdj:boostrap was already done.")


(defun pdj:print (msg)
  "Print to buffer with animation"
  (teletype-text msg 0.1 0.5))


(defun pdj:install-if-needed (pkgrequire)
  "Installs a package if it is not already installed."

  (if (require pkgrequire nil t)
      (pdj:print (concat (concat
			 (symbol-name pkgrequire) " already prensent.") "\n"))

    (progn
      (pdj:print (concat (concat
			  "Obtaining "  (symbol-name pkgrequire)) "\n"))
      (package-install pkgrequire))))


(defun pdj:boostrap-is-done ()
  (file-exists-p pdj:first-run-file))


(defun pdj:bootstrap ()

  (interactive)

  (unless (file-exists-p pdj:first-run-file)
    (setq output-buffer (get-buffer-create "*Emacs church service*"))
    (switch-to-buffer output-buffer)

    (setq emacs-banner-bootstrap (create-image "splash.svg"))
    (insert-image emacs-banner-bootstrap)
    (sit-for 1)

    (setq msg (concat "\n\n" "Blessings, my son. Welcome to the Emacs church."
		      "\nYou ought to acquire some canons. Be patient.\n\n"))
    (pdj:print msg)

    (add-to-list 'package-archives
    		 '("melpa" . "http://melpa.org/packages/"))

    (pdj:print "Fetching index canonice...\n\n")
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

    (pdj:print "\nAll canons acquired.")
    (pdj:print "\nHappy hacking and may St. Ignutius be with you.")
    (append-to-file "first run ok" nil pdj:first-run-file)))


(provide 'pdj-bootstrap)