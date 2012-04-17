;; -*- emacs-lisp -*-
;;
;;
;; name: color-theme-retro-green
;; date: Fri Sep 24 2010 16:44:35 GMT+0400 (MSD)
;;
;;
;; To use this theme save this code into a
;; file named color-theme-retro-green.el and place it
;; in a directory in your load-path
;;
;;    (require 'color-theme-retro-green)
;;    (color-theme-retro-green)
;;


(require 'color-theme)

(defun color-theme-retro-green ()
  "Generated with http://color-theme-select.heroku.com/ on Fri Sep 24 2010 16:44:35 GMT+0400 (MSD)  
Plain green on black faces for those longing for the good old days."
  (interactive)
  (color-theme-install
    '(color-theme-retro-green
      (
       (foreground-color . "green")
       (background-color . "black")
       (mouse-color . "green")
       (cursor-color . "green")
       (border-color . "green")
       (background-mode . "dark")
      )
      (
      )
      (bold  ((t (:bold t))))
      (italic  ((t (:underline t))))
      (bold-italic  ((t (:bold t))))
      (underline  ((t (:underline t))))
      (highlight  ((t (:foreground "black" :background "green" :inverse t))))
      (region  ((t (:foreground "black" :background "green" :inverse t))))
      (secondary-selection  ((t (:foreground "black" :background "green" :inverse t))))
     )
  )
)

(provide 'color-theme-retro-green)
