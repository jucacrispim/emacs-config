(add-to-list 'load-path "~/.emacs.d/poraodojuca")
(add-to-list 'load-path "~/.emacs.d/third")
(add-to-list 'load-path "~/.emacs.d/third/multi-term-20150220.1320-pdj/")

(require 'pdj)

(add-hook 'window-setup-hook 'pdj:setup)
