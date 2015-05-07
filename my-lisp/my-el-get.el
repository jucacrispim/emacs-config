(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (message "Looks like it's your first run on this config.")
  (message "Lets install some nice stuff here!")
  (message "Installing el-get...")
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))


(unless (require 'multi-term nil 'noerror)
  (message "Installing multi-term...")
  (el-get-install "multi-term"))

(unless (require 'auto-complete nil 'noerror)
  (message "Installing auto-complete"))

;;(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)
