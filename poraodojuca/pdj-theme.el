(require 'color-theme-modern)
;; (color-theme-initialize)

;; (require 'xterm-256color)

(deftheme pdj
  "Porão do Juca theme")

(custom-theme-set-faces
 'pdj

 '(default ((t (:background "black" :foreground "green2"))))
 '(mouse ((t (:foreground "green"))))
 '(cursor ((t (:background "green"))))
 '(border ((t (:foreground "green"))))
 '(js2-function-param ((t (:foreground "lightgoldenrod"))))
 '(js2-object-property ((t (:foreground "OldLace"))))
 '(js2-object-property-access ((t (:foreground "green2"))))
 '(font-lock-comment-face ((t (:weight medium :foreground "#dc0000"))))
 '(font-lock-constant-face ((t (:foreground "magenta3"))))
 '(font-lock-string-face ((t (:foreground "OldLace"))))
 '(font-lock-function-name-face
   ((t (:bold t :weight bold :foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "Cyan"))))
 '(font-lock-type-face ((t (:foreground "cyan4"))))
 '(show-paren-match-face ((t (:background "gray11"))))
 '(magit-diff-added-highlight
   ((t (:background "SeaGreen3" :weight bold :foreground "green4"))))
 '(magit-diff-removed-highlight
   ((t (:background "LightSalmon1" :weight bold :foreground "red3"))))
 '(magit-diff-added ((t (:foreground "green4"))))
 '(magit-diff-removed ((t (:foreground "red3"))))
 '(magit-section-highlight ((t (:background "gray16"))))
 '(magit-section-heading
   ((t (:background "gray16" :weight bold :foreground "LavenderBlush1"))))
 '(magit-diff-file-heading-highlight
   ((t (:background "gray16"))))
 '(eterm-256color-black ((t (:background "black" :foreground "black"))))
 '(eterm-256color-green ((t (:background "green2" :foreground "green2"))))
 '(eterm-256color-white ((t (:background "white" :foreground "white"))))
 '(eterm-256color-yellow ((t (:background "yellow" :foreground "yellow"))))
 '(eterm-256color-red ((t (:background "red" :foreground "red"))))
 '(ac-selection-face ((t ( :background "black")))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'pdj)
