;; Install required packages for pdj config

(require 'package)
(require 'teletype)
(require 'pdj-utils)

;; (package-initialize)

(defcustom pdj:first-run-file "~/.emacs.d/firstrun"
  "File that indicates if pdj:boostrap was already done.")



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
		      "\nThou shalt to acquire knowledge through some canons."
		      " Be patient.\n\n"))
    (pdj:print msg)

    (add-to-list 'package-archives
     		 '("melpa" . "https://melpa.org/packages/"))

    (pdj:print "Fetching index canonice...\n\n")
    (package-initialize)
    (package-refresh-contents)

    (pdj:install-if-needed 'virtualenvwrapper)
    (pdj:install-if-needed 'auto-complete)
    (pdj:install-if-needed 'yasnippet)
    (pdj:install-if-needed 'yasnippet-snippets)
    (pdj:install-if-needed 'color-theme-modern)
    (pdj:install-if-needed 'browse-kill-ring)
    (pdj:install-if-needed 'py-autopep8)
    (pdj:install-if-needed 'flycheck)
    (pdj:install-if-needed 'magit)
    (pdj:install-if-needed 'js2-mode)
    (pdj:install-if-needed 'tern)
    (pdj:install-if-needed 'tern-auto-complete)
    (pdj:install-if-needed 'tern-context-coloring)
    (pdj:install-if-needed 'buffer-move)
    (pdj:install-if-needed 'markdown-mode)
    (pdj:install-if-needed 'markdown-preview-mode)
    (pdj:install-if-needed 'dockerfile-mode)
    (pdj:install-if-needed 'eterm-256color)
    (pdj:install-if-needed 'realgud)
    (pdj:install-if-needed 'go-mode)
    (pdj:install-if-needed 'dap-mode)
    (pdj:install-if-needed 'use-package)
    (pdj:install-if-needed 'lsp-mode)
    (pdj:install-if-needed 'lsp-ui)
    (pdj:install-if-needed 'company)
    (pdj:install-if-needed 'company-lsp)
    (pdj:install-if-needed 'exec-path-from-shell)


    (pdj:print "\nAll canons acquired.")
    (pdj:print "\nHappy hacking and may St. Ignutius be with you.")
    (append-to-file "first run ok" nil pdj:first-run-file)))


(provide 'pdj-bootstrap)
