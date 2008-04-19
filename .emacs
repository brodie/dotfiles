(add-to-list 'load-path "~/.emacs.d/plugins")

; Settings
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq show-trailing-whitespace t)
(setq longlines-show-hard-newlines t)
(setq transient-mark-mode t)
(setq backup-inhibited t)
(setq auto-save-default nil)

; Bindings
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(global-set-key (kbd "M-[ h") 'beginning-of-line)
(global-set-key (kbd "M-[ f") 'end-of-line)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
