(setq py-xenon-max-absolute "A")

(defun py-xenon ()
  "Runs xenon on buffer to look for code complexities"

  (interactive)
  (setq py-xenon-output-buffer "*py-xenon*")
  (setq py-xenon-cmd (concat
		      (concat "xenon -b " py-xenon-max-absolute)
		      (concat " " buffer-file-name)))
  (setq py-xenon-output (shell-command-to-string py-xenon-cmd))
  (princ py-xenon-cmd)
  (unless (string-equal py-xenon-output "")
      (with-output-to-temp-buffer py-xenon-output-buffer
	(princ py-xenon-output))))

(provide 'py-xenon)
