;; Hooks for javascript


(defun pdj:js-setup ()
  "* Sets `js-indent-level' to 2"

  (add-hook 'js-mode-hook
	    '(lambda ()
	       (setq js-indent-level 2))))


(provide 'pdj-js)
