;; multi-term
(require 'multi-term)
(setq multi-term-program "/usr/bin/zsh")

(defun mark-whole-line()
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(add-hook 'term-mode-hook
	  (lambda()
	    (yas-minor-mode -1)
	    (define-key term-raw-map (kbd "C-y") 'term-paste)
	    (define-key term-raw-map (kbd "C-æ") 'mark-whole-line)))
;;	    (define-key term-raw-map (kbd "C-'") 'term-send-raw-meta)))
