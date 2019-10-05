(require 'pdj-theme)


(defun toggle-fullscreen ()
  "Toggle fullscreen, menu-bar and scroll-bar"
  (interactive)
  (if menu-bar-mode (menu-bar-mode -1) (menu-bar-mode 1))
  (if scroll-bar-mode (scroll-bar-mode -1) (scroll-bar-mode 1))
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
					 'fullboth)))

(global-set-key [(meta return)] 'toggle-fullscreen)


(defun pdj:appearance-setup ()
  "Sets the following appearence configs:

  * Enables font-lock-mode (syntax highlight)
  * Enables transient-mark-mode (region selection color)
  * Hides tool bar
  * Enables line and column number
  * Sets default font to Inconsolata
  * Enables pdj-color-theme
  * Set ac-selection-face to black"

  (when (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))

  (setq transient-mark-mode t)
  (tool-bar-mode -1)
  (setq line-number-mode t)
  (setq column-number-mode t)
  (set-frame-font "Inconsolata 12" nil t)
  (setq eterm-256color-disable-bold nil)
  ;; (run-with-timer 0.01 nil 'color-theme-poraodojuca)
  (load-theme 'pdj)
  (if pdj:fullscreen (toggle-fullscreen)))


(provide 'pdj-appearance)
