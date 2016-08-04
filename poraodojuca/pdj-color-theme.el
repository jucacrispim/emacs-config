(require 'color-theme)
(color-theme-initialize)

(require 'xterm-256color)

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
      (font-lock-function-name-face
       ((t (:bold t :weight bold :foreground "LightSkyBlue"))))
      (font-lock-keyword-face ((t (:weight bold :foreground "Cyan"))))
      (show-paren-match-face ((t (:background "gray11"))))
      (magit-diff-added-highlight
       ((t (:background "SeaGreen3" :weight bold :foreground "green4"))))
      (magit-diff-removed-highlight
       ((t (:background "LightSalmon1" :weight bold :foreground "red3"))))
      (magit-diff-added ((t (:foreground "green4"))))
      (magit-diff-removed ((t (:foreground "red3"))))
      (magit-section-highlight ((t (:background "gray16"))))
      (magit-section-heading
       ((t (:background "gray16" :weight bold :foreground "LavenderBlush1"))))
      (magit-diff-file-heading-highlight
       ((t (:background "gray16"))))
      (ac-selection-face ((t ( :background "black"))))
     )
  )
)

(provide 'pdj-color-theme)
