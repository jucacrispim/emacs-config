(require 'realgud)


(defun pdj:realgud-run-process(debugger-name script-filename cmd-args
					     minibuffer-history
					     &optional no-reset)
  "Runs `realgud-exec-shell' with DEBUGGER-NAME SCRIPT-FILENAME
and CMD-ARGS. If this succeeds, we save CMD-ARGS in command-buffer
for use if we want to restart.  If we don't succeed in running
the program, we will switch to the command buffer which shows
details of the error. The command buffer or nil is returned.

DEBUGGER-NAME is used in selecting the tracking mode inside the
command buffer. The debugger name and SCRIPT-FILENAME are used in
selecting a buffer name for the command buffer.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset.

The only difference in this function from the original one is that
here instead of switch-to-buffer to the debug buffer we open a new window
above the current one and pop-to-buffer."
  (let ((cmd-buf))
    (setq cmd-buf
	  (apply 'pdj:realgud-exec-shell debugger-name script-filename
		 (car cmd-args) no-reset (cdr cmd-args)))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case?
    (let* ((process (get-buffer-process cmd-buf)))
      (if (and process (eq 'run (process-status process)))
	  (progn
	    (unless (> (length (window-list)) 2)
	      (split-window-below 30))

	    (pop-to-buffer cmd-buf)
	    (realgud:track-set-debugger debugger-name)
	    (realgud-cmdbuf-info-in-debugger?= 't)
	    (realgud-cmdbuf-info-cmd-args= cmd-args)
	    (when cmd-buf
	      (pop-to-buffer cmd-buf)
	      (when realgud-cmdbuf-info
		(let* ((info realgud-cmdbuf-info)
		       (cmd-args (realgud-cmdbuf-info-cmd-args info))
		       (cmd-str  (mapconcat 'identity  cmd-args " ")))
		  (if (boundp 'starting-directory)
		      (realgud-cmdbuf-info-starting-directory= starting-directory))
		  (set minibuffer-history
		       (cl-remove-duplicates
			(cons cmd-str (eval minibuffer-history)) :from-end)
		       ))
		)))
	;; else
	(progn
	  (if cmd-buf (pop-to-buffer cmd-buf))
	  (message "Error running command: %s" (mapconcat 'identity cmd-args " "))
	  )
	)
      )
    cmd-buf
    )
  )

(defun pdj:realgud-exec-shell (debugger-name script-filename program
				      &optional no-reset &rest args)
  "Run the specified SCRIPT-FILENAME in under debugger DEBUGGER-NAME a
comint process buffer. ARGS are the arguments passed to the
PROGRAM.  At the moment, no piping of input is allowed.

SCRIPT-FILENAME will have local variable `realgud-script-info' set
which contains the debugger name and debugger process-command
bunffer.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset.

The difference here is that we use `pdj:project-directory'  as the
current-directory for the command"

  (let* ((non-nil-filename (or script-filename "+No filename+"))
	 (default-directory pdj:project-directory)
	 (current-directory
	  (or pdj:project-directory (file-name-directory non-nil-filename)
	      default-directory "./"))
	 (cmdproc-buffer-name
	  (replace-regexp-in-string
	   "\s+" "\s"
	   (format "*%s %s shell*"
		   (file-name-nondirectory debugger-name)
		   (file-name-nondirectory non-nil-filename))))
	 (cmdproc-buffer (get-buffer-create cmdproc-buffer-name))
	 (realgud-buf (current-buffer))
	 (cmd-args (cons program args))
	 (process (get-buffer-process cmdproc-buffer)))

    (with-current-buffer cmdproc-buffer
      ;; If the found command buffer isn't for the same debugger
      ;; invocation command, rename that and start a new one.
      ;;
      ;; For example: "bashdb /tmp/foo" does not match "bashdb
      ;; /etc/foo" even though they both canonicalize to the buffer
      ;; "*bashdb foo shell*"
      (when (and (realgud-cmdbuf?)
		 (not
		  (equal cmd-args
			 (realgud-cmdbuf-info-cmd-args realgud-cmdbuf-info))
		  ))
	(rename-uniquely)
	(setq cmdproc-buffer (get-buffer-create cmdproc-buffer-name))
	(setq process nil)
	))

    (if (and process (eq 'run (process-status process)))
        cmdproc-buffer
      (with-current-buffer cmdproc-buffer
	(and (realgud-cmdbuf?) (not no-reset) (realgud:reset))
	(make-local-variable 'starting-directory)
	(setq starting-directory current-directory)

	(insert "Current directory: " current-directory "\n")
 	(insert "Command: " (mapconcat 'identity cmd-args " ") "\n")


	;; For term.el
	;; (term-mode)
	;; (set (make-local-variable 'term-term-name) realgud-term-name)
	;; (make-local-variable 'realgud-parent-buffer)
	;; (setq realgud-parent-buffer realgud-buf)

	;; For comint.el.
	(comint-mode)

	;; Making overlay-arrow-variable-list buffer local has to be
	;; done after running commint mode. FIXME: find out why and if
	;; this reason is justifyable. Also consider moving this somewhere
	;; else.
	(make-local-variable 'overlay-arrow-variable-list)
	(make-local-variable 'realgud-overlay-arrow1)
	(make-local-variable 'realgud-overlay-arrow2)
	(make-local-variable 'realgud-overlay-arrow3)

	(condition-case failure
	    (comint-exec cmdproc-buffer debugger-name program nil args)
	  (error
	   (let ((text (format "%S\n" failure)))
	     (insert text)
	     (message text)(sit-for 1)
 	     text)))

 	(setq process (get-buffer-process cmdproc-buffer))

	(if (and process (eq 'run (process-status process)))
	  (let ((src-buffer)
		(cmdline-list (cons program args)))
	    ;; is this right?
	    (when (and script-filename (file-exists-p script-filename)
		       (not (realgud:binary script-filename)))
	      (setq src-buffer (find-file-noselect script-filename))
	      (point-max)
	      (realgud-srcbuf-init src-buffer cmdproc-buffer))
	    (process-put process 'buffer cmdproc-buffer))
	  ;; else
	  (let ((text
		 (format
		  "Failed to invoke debugger %s on program %s with args %s\n"
		  debugger-name program (mapconcat 'identity args " "))))
	    (with-current-buffer cmdproc-buffer (insert text))
	    (message text)
	  ))
    cmdproc-buffer))))


(provide 'pdj-realgud)
