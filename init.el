
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   '(tern-context-coloring yasnippet-snippets browse-kill-ring flycheck-pycheckers pylint yaml-mode js2-mode tern tern-auto-complete yasnippet virtualenvwrapper py-autopep8 magit jedi flycheck buffer-move))
 '(safe-local-variable-values
   '((pdj:test-suite-prefix . "")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "pylint-mongoengine")
     (pdj:project-directory . "/home/juca/mysrc/pylint-mongoengine/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "jasmindark")
     (pdj:project-directory . "/home/juca/mysrc/jasmindark/")
     (pdj:py-test-suite-prefix . "")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "./build-scripts/check_coverage.sh weedkarma_cms 100")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "weedkarma_cms")
     (pdj:project-directory . "/home/juca/mysrc/weedkarma_cms/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "weedkarma")
     (pdj:project-directory . "/home/juca/mysrc/weedkarma/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "asyncamqp")
     (pdj:project-directory . "/home/juca/mysrc/asyncamqp/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "asyncblink")
     (pdj:project-directory . "/home/juca/mysrc/asyncblink/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "ml")
     (pdj:project-directory . "/home/juca/mysrc/ml/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "mongomotor")
     (pdj:project-directory . "/home/juca/mysrc/mongomotor/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh mongomotor")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "mongomotor")
     (pdj:project-directory . "/home/juca/mysrc/mongomotor/")
     (jasmacs:jasmine-yaml-path . "tests/js-unit/jasmine.yml")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python pyromanager.py test")
     (pdj:venv-name . "pyrocumulus")
     (pdj:custom-commands . "t")
     (pdj:project-directory . "/home/juca/mysrc/pyrocumulus/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python pyromanager.py test --settings=settings.test")
     (pdj:venv-name . "araponga")
     (pdj:project-directory . "/home/juca/mysrc/araponga/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "taskspeedometer")
     (pdj:project-directory . "/home/juca/mysrc/taskspeedometer/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "taskspeedometer")
     (pdj:project-directory . "/home/juca/mysrc/taskspeedometer/")
     (pdj:py-autopep8 . "nil")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "fs-staticworkflow3")
     (pdj:project-directory . "/home/juca/mysrc/fs/staticworkflow")
     (pdj:py-autopep8 . "nil")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "ptbkp")
     (pdj:project-directory . "/home/juca/mysrc/ptbkp/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh toxicbuild 100")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "toxicbuild")
     (pdj:project-directory . "/home/juca/mysrc/toxicbuild/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "toxicbuild")
     (pdj:project-directory . "/home/juca/mysrc/toxicbuild/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "toxicbuild")
     (pdj:project-directory . "/home/juca/mysrc/toxicbuild/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "toxicbuild")
     (pdj:project-directory . "/home/juca/mysrc/toxicbuild/")
     (pdj:py-autopep8 . "t")
     (pdj:coverage-command . "sh ./build-scripts/check_coverage.sh")
     (pdj:test-command . "python setup.py test")
     (pdj:venv-name . "toxicbuild")
     (pdj:project-directory . "/home/juca/mysrc/toxicbuild/")
     (pdj:project-directory . "~/mysrc/ptbkp/")
     (pdj:venv-name . "toxicbuild")
     (pdj:project-directory . "/home/juca/mysrc/toxicbuild/")
     (pdj:venv-name . "ptbkp")
     (pdj:project-directory . "/home/juca/mysrc/ptbkp/")
     (pdj:test-command . "python setup.py test")
     (pdj:coverage-command . "./build-scripts/check_coverage.sh toxicbuild 100")
     (pdj:coverage-command . "./build-scripts/check_coverage.sh pylint_mongoengine 100")
     (pdj:py-autopep8 . "t")
     (pdj:venv-name . "mongomotor")
     (pdj:project-directory . "/home/juca/mysrc/mongomotor/")
     (pdj:coverage-command . "./build-scripts/check_coverage.sh")
     (pdj:venv-name . "prosco")
     (pdj:project-directory . "/home/juca/mysrc/prosco/")
     (pdj:test-command . "python pyromanager.py test")
     (pdj:test-command . "python manage.py test")
     (pdj:test-command . "pytest")
     (pdj:test-suite-prefix . "")
     (pdj:venv-name . "pyrocumulus")
     (pdj:project-directory . "/home/juca/mysrc/pyrocumulus/")
     (pdj:py-test-command . "python pyromanager.py test --settings=settings.test")
     (pdj:coverage-command . "./check_coverage.sh")
     (pdj:venv-name . "jaobi")
     (pdj:project-directory . "/home/juca/mysrc/jaobi/")
     (pdj:coverage-command . "./build-scripts/check_coverage.sh jaobi 100")))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
