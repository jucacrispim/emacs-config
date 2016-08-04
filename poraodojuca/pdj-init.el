(require 'pdj-common)
(require 'pdj-python)
(require 'pdj-elisp)
(require 'pdj-appearance)
(require 'pdj-js)

;; Custom vars used in the pdj- modules.

(defcustom pdj:test-command nil
  "Command used to run tests. You must customize this. Maybe via .dir-locals.el?")

(defcustom pdj:project-directory nil
  "Root directory for your current project. You must customize this via .dir-locals.el")

(defcustom pdj:coverage-command nil
  "Command to run coverage tests. You must customize this. Maybe via .dir-locals.el?")

(defcustom pdj:fullscreen nil
  "If not nil toggle to fullscreen on start using `toggle-fullscreen'.")

(defcustom pdj:appearance nil
  "If not nil the color theme and other appearence stuff will be used.")


(defun pdj:setup ()
  (pdj:common-setup)
  (pdj:py-setup)
  (pdj:el-setup)
  (if pdj:appearance (pdj:appearance-setup))
  (pdj:js-setup)
  (server-start))


(provide 'pdj)
