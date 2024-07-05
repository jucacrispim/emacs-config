(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(require 'dap-dlv-go)


(defcustom pdj:go-test-command "go test ./..."
  "Command to run Go tests")

(defun pdj:go-run-test-suite ()

  (interactive)

  (setq pdj:--current-go-func (car(split-string (which-function) " ")))
  (setq pdj:--test-suite pdj:--current-go-func)

  (pdj:run-test-suite pdj:--test-suite))


(defun pdj:go-set-test-command ()
  "Sets `pdj:test-command' to `pdj:go-test-command' if it is not defined."

  (hack-local-variables)

  (unless pdj:test-command
    (setq pdj:test-command pdj:go-test-command))

  (if (boundp 'pdj:test-suite-prefix)
      (setq pdj:py-test-suite-prefix pdj:test-suite-prefix)))

(defun pdj:go-debug-test()
  (interactive)

  (setq args (list :type "go"
        :request "launch"
        :name "Test subtest"
        :mode "test"
        :skip-debug-session nil
        :program nil
        :args nil
        :env nil))
  (dap-debug args)
  (other-window 1)
  (dap-hydra))


(defun pdj:go-set-tab-width ()
  (setq tab-width 4))


(defun pdj:go-set-test-suite-prefix ()
  (setq pdj:test-suite-prefix "-run "))


(defun pdj:go-func-name-at-line ()
  (setq pdj:--go-defun (split-string (pdj:line-contents)))
  (car (split-string (nth 1 pdj:--go-defun) "(")))


;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun pdj:lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; removes breakpoints and close the hydra
(defun pdj:go-debug-terminated-hooks (session)
  (dap-breakpoint-delete-all)
  (dap-hydra/nil)
  (kill-buffer "*Go test function server log*")
  (kill-buffer "*Go test function out*"))

(defun pdj:go-imenu-hooks ()
  (setq imenu-auto-rescan t)
  (setq imenu-prev-index-position-function 'beginning-of-defun)
  (setq imenu-extract-index-name-function 'pdj:go-func-name-at-line))


(defun pdj:go-keyboard-hooks ()
  "Custom key bindings. The following bindings are done here:

   * The bindings from pdj:prog-keyboard-hooks
   * `C-c s' - pdj:go-run-test-suite
   * `C-c d' - pdj:go-debug-test
   * `C-c b' - dap-breakpoint-add"

  (pdj:prog-keyboard-hooks)
  (local-set-key (kbd "C-c s") 'pdj:go-run-test-suite)
  (local-set-key (kbd "C-c d") 'pdj:go-debug-test)
  (local-set-key (kbd "C-c b") 'dap-breakpoint-add))


(defun pdj:go-setup ()
  (add-hook 'go-mode-hook 'pdj:go-set-test-suite-prefix)
  (add-hook 'go-mode-hook 'pdj:go-keyboard-hooks)
  (add-hook 'go-mode-hook 'pdj:go-imenu-hooks)
  (add-hook 'go-mode-hook 'pdj:go-set-tab-width)
  (add-hook 'go-mode-hook #'pdj:lsp-go-install-save-hooks)
  (add-hook 'dap-terminated-hook #'pdj:go-debug-terminated-hooks))


(provide 'pdj-go)
