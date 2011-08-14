;; Aqui é a configuração do python
;; Você precisa ter instalado rope, ropemacs, ropemode
;; e pymacs instalado

(require 'python)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; Carregando Pymacs
;; Pymacs permite execução de código python no emacs e código
;; elisp no python
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; Adicionando ropemacs
(pymacs-load "ropemacs" "rope-")


