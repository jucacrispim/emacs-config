(defun pdj:el-ac-hooks ()
  "Activates auto-complete-mode if it is not activated yet."

  (unless auto-complete-mode
    (auto-complete-mode)))


(defun pdj:el-keyboard-hooks ()
  "Custom key bindings for elisp code. The following bindings are
   done here:

   * `C-c C-d' - 'edebug-defun
   * `C-c C-c' - 'eval-buffer
   * `C-c C-r' - 'eval-region"

  (local-set-key (kbd "C-c C-d") 'edebug-defun)
  (local-set-key (kbd "C-c C-c") 'eval-buffer)
  (local-set-key (kbd "C-c C-r") 'eval-region))


(defun pdj:el-all-hooks ()
  "Enables all pdj:el hooks. They are:

  * pdj:el-keyboard-hooks
  * pdj:el-ac-hooks"

  (pdj:el-keyboard-hooks)
  (pdj:el-ac-hooks))


(defun pdj:el-setup ()
  (add-hook 'emacs-lisp-mode-hook 'pdj:el-all-hooks))


(provide 'pdj-elisp)
