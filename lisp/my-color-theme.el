(require 'color-theme)
(color-theme-initialize)

(defun color-theme-poraodojuca()

  (interactive)
  (setq term-default-bg-color "black")
  (setq term-default-fg-color "green2")
  (color-theme-install
    '(color-theme-poraodojuca
      (
       (foreground-color . "green2")
       (background-color . "black")
       (mouse-color . "green")
       (cursor-color . "green")
       (border-color . "green")
       (background-mode . "dark")
      )
      (font-lock-comment-face ((t (:weight medium :foreground "#dc0000"))))
      (font-lock-constant-face ((t (:foreground "magenta3"))))
      (font-lock-string-face ((t (:foreground "OldLace"))))
      (font-lock-type-face ((t (:foreground "cyan4"))))
      (font-lock-function-name-face ((t (:bold t :weight bold :foreground "LightSkyBlue"))))
      (font-lock-keyword-face ((t (:weight bold :foreground "Cyan"))))
     )
  )
)

(provide 'color-theme-poraodojuca)

(run-with-timer 1 nil 'color-theme-poraodojuca)
