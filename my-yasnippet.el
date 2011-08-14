;; Adicionando yasnippet
;; yasnippet é usado para inserir trechos de código 
(add-to-list 'load-path
	     "~/.emacs.d/third-party/yasnippet")

(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/third-party/yasnippet/snippets")
