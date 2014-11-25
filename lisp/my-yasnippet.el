;; Adicionando yasnippet
;; yasnippet é usado para inserir trechos de código
;;(add-to-list 'load-path
;;	     "~/.emacs.d/third-party/yasnippet")

(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas-global-mode t)
;;snippets padrão do yasnippet
;;(yas/load-directory "~/.emacs.d/third-party/yasnippet/snippets")

;; ;; meus snippets
;; ;; todos os bindings dos meus snippets começam com C-c C-b
;; 1) Python
;;     i == import ipdb;ipdb.set_trace()
;;     c == #-*- coding: utf-8 -*-
;;     h == #!/usr/bin/env python
;;          #-*- coding: utf-8 -*-
(yas/load-directory "~/.emacs.d/my-snippets/")
