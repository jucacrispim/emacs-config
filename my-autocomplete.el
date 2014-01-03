(require 'auto-complete)

;; Usando C-n e C-p para escolher as opções do auto-complete
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; mudando a cor do menu do autocomplete
(set-face-background 'ac-selection-face "black")

;; O auto-complete deixa o tab como tecla dele, então vamos voltar ao normal
(defun set-defult-tab-action ()
  (interactive)
  (indent-for-tab-command))

(global-set-key "\t" 'set-defult-tab-action)

;; quantidade de caracteres que vai começar o auto-complete
(set (make-local-variable 'ac-auto-start) 4)
