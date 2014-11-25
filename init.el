;; Arquivo de configuração do emacs
;; Você precisa dos seguintes complementos para isso tudo funcionar
;; multi-term.el
;; python-mode.el
;; virtualenv.el
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
(add-to-list 'load-path "~/.emacs.d/lisp")

(load-library "my-el-get")
(load-library "my-news")
(load-library "my-autocomplete")
(load-library "my-python")
(load-library "my-js")
(load-library "my-yasnippet")
(load-library "my-keyboard")
(load-library "my-fullscreen")
(load-library "my-term")
(load-library "my-appearance")

;; sql-mode para .migratidion
(add-to-list 'auto-mode-alist '("\\.migration\\'" . sql-mode))

;; python-mode para toxicbuild.conf
(add-to-list 'auto-mode-alist '("\\toxicbuild.conf\\'" . python-mode))

;; Mostrando espaços em branco inúteis
(setq whitespace-style '(face trailing))

(require 'smartparens)
(smartparens-mode)
