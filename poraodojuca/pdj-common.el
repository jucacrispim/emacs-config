;; Global hooks and key bindings used in all languages.
;; Requires pdj-utils, buffer-move, multi-term and yasnippet

(require 'auto-complete)
(require 'buffer-move)
(require 'multi-term)
(require 'yasnippet)
(require 'pdj-utils)


;; Global key bindings

(defun pdj:common-keyboard-hooks()
  "Custom global key bindings. The following bindings are done here:

  * `C-s-up' - Moves buffer up
  * `C-s-down' - Moves buffer down
  * `C-s-left' - Moves buffer left
  * `C-s-right' - Moves buffer right
  * `C-S-right' - Shift region to right
  * `C-S-left' - Shift region to left
  * `€' - Copy region as kill
  * `C-2' - Set mark
  * `M-;' - `dabbrev-expand'
  * `C-ŧ' - `multi-term'
  * `“' - `scroll-down-command'
  * `C-æ' - `mark-whole-line'
  * `C-ß' - `mark-whole-buffer'
  * `M-i' - `browse-kill-ring'
  * `đ' - `forward-word'
  * `”' - `backward-word'
  * `C-c c' - `comment-region'
  * `C-c u' - `uncomment-region'
  * `C-c gs' - `magit-status'
  * `meta-return' - `menu-bar-mode'
  * `C-c C-x k' - `pdj:kill-all-buffers'"


  (global-set-key (kbd "<C-s-up>")     'buf-move-up)
  (global-set-key (kbd "<C-s-down>")   'buf-move-down)
  (global-set-key (kbd "<C-s-left>")   'buf-move-left)
  (global-set-key (kbd "<C-s-right>")  'buf-move-right)

  (global-set-key [C-S-right] 'pdj:shift-region-right)
  (global-set-key [C-S-left] 'pdj:shift-region-left)

  (global-set-key (kbd "€") 'copy-region-as-kill)
  (global-set-key (kbd "C-2") 'set-mark-command)
  (global-set-key (kbd "M-;") 'dabbrev-expand)
  (global-set-key (kbd "C-ŧ") 'multi-term)
  (global-set-key (kbd "“") 'scroll-down-command)
  (global-set-key (kbd "C-æ") 'pdj:mark-whole-line)
  (global-set-key (kbd "C-ß") 'mark-whole-buffer)
  (global-set-key (kbd "M-i") 'browse-kill-ring)
  (global-set-key (kbd "đ") 'forward-word)
  (global-set-key (kbd "”") 'backward-word)
  (global-set-key (kbd "C-c c") 'comment-region)
  (global-set-key (kbd "C-c u") 'uncomment-region)
  (global-set-key (kbd "C-c gs") 'magit-status)
  (global-set-key (kbd "C-c C-x k") 'pdj:kill-all-buffers))


(defun pdj:prog-keyboard-hooks ()
  "Keybindings for prog-mode. The following bindings are done here:

  * `C-c t' - 'pdj:run-tests
  * `C-c v' - pdj:check-coverage"

  (local-set-key (kbd "C-c t") 'pdj:run-tests)
  (local-set-key (kbd "C-c v") 'pdj:check-coverage))


(defun pdj:load-custom-commands ()
  "Loads a file with custom commands for a project."

  (interactive)
  (hack-local-variables)

  (if pdj:project-directory
      (progn
	(defvar pdj--custom-file (concat pdj:project-directory
					 "custom-commands.el"))
	(if (and pdj:custom-commands (file-exists-p pdj--custom-file))
	    (load-file pdj--custom-file)))))


(defun pdj:multi-term-hooks ()
  "Customizations to multi-term.
  * Sets `multi-term-program' to /usr/bin/zsh
  * Disables yas-minor-mode
  * Sets `term-paste' to `C-y' on `term-raw-map'"

  (setq multi-term-program "/usr/bin/zsh")
  (yas-minor-mode -1)
  (define-key term-raw-map (kbd "C-y") 'term-paste))


(defun pdj:ac-common-hooks ()
  "Customizations to auto complete.
  * Lets tab for 'indent-for-tab-command
  * Sets 'ac-next to `C-n' on `ac-complete-mode-map'
  * Sets 'ac-previous to `C-p' on `ac-complete-mode-map'
  * Sets yas/load-directory to ~/.emacs.d/my-snippets"

  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  (yas-global-mode t)
  (global-set-key "\t" 'indent-for-tab-command)
  (setq ac-auto-start 3)
  (yas/load-directory "~/.emacs.d/my-snippets/"))


(defun pdj:add-comments-menu-item ()
  "Adds menu comments related items to edit menu."

  (interactive)
  (define-key global-map [menu-bar edit comment-region]
    '(menu-item "Comment Region" comment-region
		:enable mark-active))
  (define-key-after global-map [menu-bar edit uncomment-region]
    '(menu-item "Uncomment Region" uncomment-region
		:enable mark-active) 'comment-region)
  (define-key-after global-map [menu-bar edit edit-comments-separator]
    '(menu-item "--") 'uncomment-region))


(defun pdj:common-setup ()
  (pdj:common-keyboard-hooks)
  (pdj:ac-common-hooks)
  (pdj:add-comments-menu-item)
  (add-hook 'write-file-hooks 'pdj:delete-trailing-whitespace)
  (add-hook 'term-mode-hook 'pdj:multi-term-hooks)
  (add-hook 'prog-mode-hook 'pdj:load-custom-commands)
  (add-to-list 'auto-mode-alist '("\\.migration\\'" . sql-mode))
  (add-to-list 'auto-mode-alist '("toxicbuild.conf\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.feature\\'" . pdj:feature-mode))
  ;; enable which-function-mode
  (which-function-mode t)
  ;; displaying useless whitespaces
  (setq whitespace-style '(face trailing))
  ;; eletric-pair and parens
  (electric-pair-mode t)
  (show-paren-mode t)
  (setq show-paren-style 'expression)
  ;; setting tab to use 4 spaces
  (setq-default ident-tabs-mode nil)
  (setq tabify nil)
  ;; browse-kill-ring window behavior
  (setq browse-kill-ring-quit-action 'save-and-restore)
  ;; do not show duplicate items in history
  (setq browse-kill-ring-display-duplicates nil)
  '(tab-width 4)
  ;; always use a final blank line
  (setq require-final-newline t))


(provide 'pdj-common)
