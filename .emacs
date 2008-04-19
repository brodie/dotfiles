(add-to-list 'load-path "~/.emacs.d/plugins")

; Plugins
(require 'yasnippet-bundle)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(autoload 'markdown-mode "markdown-mode.el")

; Settings
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq show-trailing-whitespace t)
(setq longlines-show-hard-newlines t)
(setq transient-mark-mode t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(show-paren-mode 1)

; Bindings
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(global-set-key (kbd "M-[ h") 'beginning-of-line)
(global-set-key (kbd "M-[ f") 'end-of-line)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
