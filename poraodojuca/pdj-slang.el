;; config for the sol chatbot mini-language

(defvar pdj:slang-mode-hook nil)

(defvar pdj:slang-keywords
  '(("DIGA \\|SAY \\|PERGUNTE \\|ASK \\|SE \\|IF \\|SENÃO\\|ELSE" . font-lock-keyword-face)
    ;; captures things before equal signs like `a = 1'
    ("\\([[:alpha:]_]+\\)\\s-*=" . (1 font-lock-variable-name-face))
    ("{\\([[:alpha:]_]+\\)}" . (1 font-lock-variable-name-face))
    ("\\([[:alpha:]_]+\\)(.*)" . (1 font-lock-function-name-face)))

  "Keywords for behave's feature file.")

;; key map for pdj:slang-modoe
(defvar pdj:slang-mode-map (make-sparse-keymap) "Keymap for pdj:slang-mode")


(defun pdj:slang-post-py-hooks ()

  (local-set-key (kbd "C-x C-s") 'pdj:save-no-hooks))


(define-derived-mode pdj:slang-mode prog-mode "pdj:slang"
  "Major mode for editing .slang files used to create chatbots."

  (setq-local  font-lock-defaults '((pdj:slang-keywords) nil t))
  (add-hook 'pdj:slang-mode-hook 'pdj:slang-post-py-hooks)
  (use-local-map pdj:slang-mode-map))


(provide 'pdj-slang)
