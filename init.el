
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/poraodojuca")
(add-to-list 'load-path "~/.emacs.d/poraodojuca/jasmacs")
(add-to-list 'load-path "~/.emacs.d/third")
(add-to-list 'load-path "~/.emacs.d/third/multi-term-20150220.1320-pdj/")
(load-library "debian-init")

(require 'pdj)

(add-hook 'window-setup-hook 'pdj:setup)


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
