;; arquivos com as configurações de aparência

;; Ligando sintax-hilighting (font-lock-mode)
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; Ligando a 'corzinha' na seleção de texto.
(setq transient-mark-mode t)

;; Retirando o menu
(menu-bar-mode -1)

;; Retirando tool-bar
(tool-bar-mode -1)

;; Retirando scroll bar
(scroll-bar-mode -1)

;; Mostrar linha e coluna
(setq line-number-mode t)
(setq column-number-mode t)


(load-library "my-color-theme")
;;(run-with-timer 0.1 nil 'color-theme-poraodojuca)
(color-theme-poraodojuca)

(set-default-font "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
