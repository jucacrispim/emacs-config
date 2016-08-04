(add-to-list 'load-path "~/.emacs.d/poraodojuca")
(add-to-list 'load-path "~/.emacs.d/third")
(add-to-list 'load-path "~/.emacs.d/third/multi-term-20150220.1320-pdj/")


(require 'pdj)
(setq pdj:appearance t)
(pdj:setup)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(safe-local-variable-values
   (quote
    ((pdj:venv-name . "toxicbuild")
     (pdj:project-directory . "/home/juca/mysrc/toxicbuild/")
     (pdj:coverage-command . "./build-scripts/check_coverage.sh toxicbuild 100")
     (pdj:py-autopep8 . "t")
     (pdj:venv-name . "mongomotor")
     (pdj:project-directory . "/home/juca/mysrc/mongomotor/")
     (pdj:coverage-command . "./build-scripts/check_coverage.sh")
     (pdj:venv-name . "prosco")
     (pdj:project-directory . "/home/juca/mysrc/prosco/")
     (pdj:test-command . "python pyromanager.py test")
     (pdj:venv-name . "pyrocumulus")
     (pdj:project-directory . "/home/juca/mysrc/pyrocumulus/")
     (pdj:py-test-command . "python pyromanager.py test --settings=settings.test")
     (pdj:coverage-command . "./check_coverage.sh")
     (pdj:venv-name . "jaobi")
     (pdj:project-directory . "/home/juca/mysrc/jaobi/")
     (pdj:coverage-command . "./build-scripts/check_coverage.sh jaobi 100")))))
