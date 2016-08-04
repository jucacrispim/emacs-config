;;; radon.el --- Analize code complexity of your python files

;; This code is in public domain

;; Author: Juca Crispim
;; URL:
;; Version: 0.1
;;
;;; Commentary
;;
;; Radon is a tool that analizes your Python source files and reports
;; some metrics about code complexity. For mor information, take a look
;; at radon's documentation: https://radon.readthedocs.org/.
;;
;; radon.el simply invokes the radon installed in your system (works
;; well with virtualenvwrapper.el!) and shows you the output (if any)
;; in the *radon* buffer.
;;
;; Usage
;;
;; radon.el provides `radon'
;;
;;; Code:

(defvar radon-analysis-type)
(defvar radon-analysis-args)
(defvar radon-output-buffer)

(setq radon-analysis-type "cc")
(setq radon-analysis-args '(""))
(setq radon-output-buffer "*radon*")

(defun radon ()
  "Runs radon against the current buffer"

  (interactive)
  (defvar radon-args-string)
  (defvar radon-cmd-line)
  (defvar radon-output)
  (defvar radon-cmd-line-list)

  (setq radon-args-string
	(mapconcat 'identity radon-analysis-args " "))

  (setq radon-cmd-line-list
	(list "radon" radon-analysis-type radon-args-string buffer-file-name))
  (setq radon-cmd-line
	(mapconcat 'identity radon-cmd-line-list " "))
  (setq radon-output (shell-command-to-string radon-cmd-line))

  (unless (string-equal radon-output "")
    (with-output-to-temp-buffer radon-output-buffer
      (princ radon-output))))

(provide 'radon)


;;; radon.el ends here
