;; Arquivo de configuração do emacs

;; Sempre adicionar uma linha em branco ao final do arquivo
(setq require-final-newline t)

;; Identação com espaços. Tamanho do TAB é 4 espaços.
(setq-default ident-tabs-mode nil)
(setq tabify nil)
'(tab-width 4)

;; Adicionando diretório com arquivos de configuração do emacs.
(add-to-list 'load-path "~/.emacs.d")

;; Adicionando diretório com pacotes elisp de terceiros
(add-to-list 'load-path "~/.emacs.d/third-party")

;; combinação pra copiar região <AltGr>-e
(global-set-key (kbd "€") 'copy-region-as-kill)


;; Trocando atalho pra dynamic expansion
(global-set-key (kbd "M-;") 'dabbrev-expand)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/usr/bin/zsh")

(load-library "my-appearance")
(load-library "my-python")
(load-library "my-yasnippet")
(load-library "my-autocomplete")

;; sql-mode para .migration
(add-to-list 'auto-mode-alist '("\\.migration\\'" . sql-mode))

;; Desativando a função (muito esperta, por sinal) de definir se
;; split-window horizontal ou vertical
(setq split-height-threshold nil)
(setq split-width-threshold 0)


;; Ajustando tamanho do frame
;; C-J
;;(set-frame-width (selected-frame) 100)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; n3 mode (rdf, n3, turtle)
(autoload 'n3-mode "n3-mode" "Major mode for OWL or N3 files" t)
(add-hook 'n3-mode-hook
          'turn-on-font-lock)

(add-to-list 'auto-mode-alist '("\\.n3\\'" . n3-mode))
(add-to-list 'auto-mode-alist '("\\.owl\\'" . n3-mode))
(add-to-list 'auto-mode-alist '("\\.ttl\\'" . n3-mode))


;; Mostrando espaços em branco inúteis

(setq whitespace-style '(face trailing))


(setq tex-dvi-view-command "xdvi")

(setq exec-path (append exec-path '("/usr/bin/gs")))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(python-check-command "~/.emacs.d/pychecker.sh")
 '(python-check-command-args (quote (""))))
