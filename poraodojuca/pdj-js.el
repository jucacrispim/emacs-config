;; Hooks for javascript

(require 'tern)
(require 'js2-mode)
(require 'tern-auto-complete)



(defcustom pdj:js-custom-constants
  '(("prototype\\|__proto__" . font-lock-constant-face))

  "Custon highlight syntax for js")


(defcustom pdj:js-tern-command
  (list "~/.emacs.d/node_modules/tern/bin/tern")

  "Command to start tern server. A list with bin path and arguments")

(defun pdj:js-set-tern-dir ()
  "Sets the tern project dir."

  (hack-local-variables)
  (if (pdj:project-directory)
      (setq tern-project-dir pdj:project-directory)))


(defun pdj:js-setup ()
  "Setup for js code

  * Enables js2-mode
  * Enables auto-complete-mode and runs tern-ac-setup
  * Sets the custom faces
  * sets tern related sutff
  * Sets `js-indent-level' to 2"


  (font-lock-add-keywords 'js2-mode pdj:js-custom-constants)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq tern-command pdj:js-tern-command)
  (add-hook 'js2-mode-hook
	    (lambda ()
	      (setq js-indent-level 2)
	      (tern-mode t)
	      (auto-complete-mode t)
	      (tern-ac-setup))))


(provide 'pdj-js)
