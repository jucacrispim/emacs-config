;; configs for email

(require 'mu4e)


(defun pdj:mail-keyboard-hooks ()
  "Custom keybinding to write/compose email"

  (global-set-key (kbd "C-c m") 'mu4e-compose-new)
  (global-set-key (kbd "C-x m") 'mu4e))

(defun pdj:mail-setup ()
  (setq send-mail-function 'smtpmail-send-it)
  (pdj:mail-keyboard-hooks))


(provide 'pdj-mail)
