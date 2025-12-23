(require 'eaf-browser)

(defcustom pdj:ablog-tupi-bin "tupi" "the tupi binary")
(defcustom pdj:ablog-tupi-buffer-name "ablog-tupi" "buffer name for the tupi process")
(defcustom pdj:ablog-build-dir "_website/build"
  "build directory for the blog html")
(defcustom pdj:ablog-build-cmd "make build" "command to build the blog html")
(defcustom pdj:ablog-build-buffer-name "ablog-build" "buffer name for the blog build")
(defcustom pdj:ablog-tupi-url "http://localhost:8080" "url serving the blog html")

(setq pdj:--ablog-tupi-started nil)


(defun pdj:ablog-set-start-tupi-cmd ()
  (hack-local-variables)

  (setq pdj:ablog-start-tupi-cmd (concat pdj:ablog-tupi-bin " "
					 " -root "
					 pdj:project-directory
					 pdj:ablog-build-dir
					 " -default-to-index")))

(defun pdj:ablog-start-tupi()
  "Starts a tupi instance in a vterm buffer"
  (unless (equal pdj:--ablog-tupi-started t)
    (pdj:run-in-term-on-project-directory-on-background
     pdj:ablog-start-tupi-cmd
     pdj:ablog-tupi-buffer-name)

    (setq pdj:--ablog-tupi-started t)))

(defun pdj:ablog-setup-venv ()
  (hack-local-variables)
  (venv-workon pdj:venv-name))


(defun pdj:ablog-build ()
  "Builds the blog html and calls the apropriate callback for success or error"

  (setq pdj:--ablog-build-script (format" %s
if [[ \"$?\" == \"0\" ]]
then
  vterm_cmd ablog-build-success %s
else
  vterm_cmd ablog-build-fail
fi" pdj:ablog-build-cmd (pdj:ablog-web-path)))

  (pdj:run-in-term-on-project-directory-on-background
   pdj:--ablog-build-script
   pdj:ablog-build-buffer-name))


(defun pdj:ablog-build-success (webpath)
  "Open or refresh the web browser after the blog html is built"

  (let ((kill-buffer-query-functions nil))
    (kill-buffer pdj:ablog-build-buffer-name))
  (setq pdj:--ablog-post-url (concat pdj:ablog-tupi-url webpath))

  (eaf-open-browser-other-window pdj:--ablog-post-url)
  (eaf-py-proxy-insert_or_refresh_page)
  (other-window 1))

(defun pdj:ablog-build-fail ()
  "Pops to the ablog build buffer"

  (pop-to-buffer pdj:ablog-build-buffer-name))

(defun pdj:ablog-web-path  ()
  "Returns the path to be used in the url for the current post buffer"

  (setq pdj--rel-dir (replace-regexp-in-string
		      pdj:project-directory "" (buffer-file-name)))

  (setq pdj--rel-dir (replace-regexp-in-string
		      ".rst" "/" pdj--rel-dir))

  (setq pdj:--ablog-web-path (concat "/" pdj--rel-dir)))


(defun pdj:ablog-set-eval-cmds ()
  "Adds pdj:ablog-build-success and pdj:ablog-build-fail to vterm-eval-cmds list"

  (push (list "ablog-build-fail" 'pdj:ablog-build-fail) vterm-eval-cmds)
  (push (list "ablog-build-success" 'pdj:ablog-build-success) vterm-eval-cmds))


(defun pdj:ablog-all-hooks()
  (hack-local-variables)
  (venv-workon pdj:venv-name)
  (pdj:ablog-set-start-tupi-cmd)
  (pdj:ablog-set-eval-cmds)
  (pdj:ablog-start-tupi))

(defun pdj:ablog-setup()

  (add-hook 'rst-mode-hook 'pdj:ablog-all-hooks)
  (add-hook 'after-save-hook (lambda ()
			       (when (eq major-mode 'rst-mode)
				 (pdj:ablog-build)))))


(provide 'pdj-ablog)
