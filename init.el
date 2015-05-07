;; Arquivo de configuração do emacs
;; Você precisa dos seguintes complementos para isso tudo funcionar
;; multi-term.el
;; python-mode.el
;; Pymacs.el
;; color-theme.el
;; auto-complete.el
;; yasnippet.el
;;
;; Precisa também instalar alguns pacotes python
;; pep8 e pyflakes.
;;
;; Para o virtualenv funcionar direito, seus ambientes
;; precisam ficar em ~/.virtualenvs

;; Sempre adicionar uma linha em branco ao final do arquivo
(setq require-final-newline t)
(setq inhibit-startup-screen t)

;; Identação com espaços. Tamanho do TAB é 4 espaços.
(setq-default ident-tabs-mode nil)
(setq tabify nil)
'(tab-width 4)

;; Adicionando diretório com arquivos de configuração do emacs.
(add-to-list 'load-path "~/.emacs.d/my-lisp")

(load-library "my-bootstrap")
(load-library "my-news")
(load-library "my-autocomplete")
(load-library "my-python")
(load-library "my-js")
(load-library "my-yasnippet")
(load-library "my-keyboard")
(load-library "my-fullscreen")
(load-library "my-term")
(load-library "my-appearance")

(defun load-my-appearance (&optional frame)
  (load-library "my-appearance"))

;; pro emacs --daemon
;; não rola. uso um alias no shell
;;
(add-hook 'after-make-frame-functions 'load-my-appearance)

;; sql-mode para .migratidion
(add-to-list 'auto-mode-alist '("\\.migration\\'" . sql-mode))

;; python-mode para toxicbuild.conf
(add-to-list 'auto-mode-alist '("\\toxicbuild.conf\\'" . python-mode))

;; Mostrando espaços em branco inúteis
(setq whitespace-style '(face trailing))

(electric-pair-mode t)
(show-paren-mode t)
(setq show-paren-style 'expression)
(smartparens-global-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((project-venv-name . "mongomotor")
     (project-venv-name . "toxicbuild")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
