;; Aqui é a configuração do python
;; o jedi - http://tkf.github.io/emacs-jedi/

(require 'python)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


(require 'virtualenv)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

;; jedi
(autoload 'jedi:setup "jedi" nil t)

;; configurando delete-trainling-whitespace antes de salver
(defun delete-whitespace-on-save ()
  (save-excursion
    (delete-trailing-whitespace)))

(add-hook 'write-file-hooks (lambda () (delete-whitespace-on-save)))



;; pychecker pyflakes
