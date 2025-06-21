(require 'pdj-bootstrap)

;; Custom vars used in the pdj- modules.

(defcustom pdj:test-command nil
  "Command used to run tests. You must customize this. Maybe via .dir-locals.el?")

(defcustom pdj:test-suite-prefix nil
  "Command line switch to run specific test. You must customize this. Maybe via .dir-locals.el?")

(defcustom pdj:project-directory nil
  "Root directory for your current project. You must customize this via .dir-locals.el")

(defcustom pdj:coverage-command nil
  "Command to run coverage tests. You must customize this. Maybe via .dir-locals.el?")

(defcustom pdj:fullscreen nil
  "If not nil toggle to fullscreen on start using `toggle-fullscreen'.")

(defcustom pdj:appearance t
  "If not nil the color theme and other appearence stuff will be used.")

(defcustom pdj:custom-commands nil
  "Loads a file with custom commands for a project if not nil")


(defun pdj:--do-setup ()
  (require 'pdj-term)
  (require 'pdj-common)
  (require 'pdj-python)
  (require 'pdj-elisp)
  (require 'pdj-js)
  (require 'pdj-project)
  (require 'pdj-feature)
  (require 'pdj-slang)
  (require 'pdj-appearance)
  (require 'pdj-go)
  (require 'pdj-eaf)

  (if pdj:appearance (pdj:appearance-setup))

  (add-to-list 'package-archives
    	       '("melpa" . "http://melpa.org/packages/"))

  (pdj:common-setup)
  (pdj:term-setup)
  (pdj:py-setup)
  (pdj:el-setup)
  (pdj:js-setup)
  (pdj:prj-setup)
  (pdj:go-setup)
  (pdj:eaf-setup)
  ;; doing it by last so everything we do in the custom-commnd.el
  ;; overwrites the default stuff.
  (add-hook 'prog-mode-hook 'pdj:load-custom-commands)
  (add-hook 'text-mode-hook 'pdj:load-custom-commands)

  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  (add-to-list 'pdj:kill-all-buffers-hooks 'pdj:py-deactivate))


(defun pdj:setup ()
  "Sets up all stuff needed for pdj's emacs."

  (server-start)
  (setq ring-bell-function 'ignore)

  (if (pdj:boostrap-is-done)
      (pdj:--do-setup)
    ;; we wait here all stuff to be loaded
    ;; otherwise we can't switch-to-buffer
    (run-with-timer 1 nil (lambda ()
			    (pdj:bootstrap)
			    (pdj:--do-setup)))))


(provide 'pdj)
