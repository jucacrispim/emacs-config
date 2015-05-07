;; Instala as dependências para as configurações

(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     ;;'("marmalade" . "http://marmalade-repo.org/packages/")
	     '("melpa" . "http://melpa.org/packages/"))

(defun pdj-install-if-needed (pkgrequire)
  "Instala um pacote se ele já não está instalado"

  (unless (require pkgrequire nil 'noerror)
    (princ (concat (concat "Installing "  (symbol-name pkgrequire)) "\n"))
    (package-install pkgrequire)))

(defun pdj-print (msg)
  "Print to buffer with a new line"
  (princ (concat msg "\n")))

(setq first-run-file "~/.emacs.d/firstrun")
(unless (file-exists-p first-run-file)
  (setq output-buffer "*emacs bootstrap*")
  (switch-to-buffer output-buffer)

  (with-output-to-temp-buffer output-buffer
    ;;(with-current-buffer (switch-to-buffer output-buffer)
    (pdj-print "Looks like it's your first run of emacs with this config.")
    (pdj-print "Lets install some nice stuff here!\n")
    (pdj-print "First, updating repos...")

    (package-refresh-contents)

    (pdj-install-if-needed 'multi-term)
    (pdj-install-if-needed 'virtualenvwrapper)
    (pdj-install-if-needed 'auto-complete)
    (pdj-install-if-needed 'jedi)
    (pdj-install-if-needed 'yasnippet)
    (pdj-install-if-needed 'color-theme)
    (pdj-install-if-needed 'smartparens)
    (pdj-install-if-needed 'browse-kill-ring)
    (pdj-install-if-needed 'py-autopep8)
    (pdj-install-if-needed 'flycheck-pyflakes)

    (pdj-print "Everything installed!"))

  (append-to-file "first run ok" nil first-run-file))
