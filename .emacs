(add-to-list 'load-path "~/.emacs.d/plugins")

; Plugins
(require 'sudo)
(autoload 'yas/minor-mode "yasnippet-bundle" nil t)
(autoload 'js2-mode "js2" nil t)
(autoload 'rst-mode "rst" nil t)
(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'pod-mode "pod-mode" nil t)
(autoload 'po-mode "po-mode" nil t)
(autoload 'css-mode "css-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.\\." . po-mode))
(add-to-list 'auto-mode-alist '("\\.css$\\|\\.css\\.dtml$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.pt$" . html-mode))

; Indentation settings
(setq-default indent-tabs-mode nil)
(setq standard-indent 4)
(setq c-default-style "bsd")
(setq c-basic-offset 4)
(setq-default c-indent-level 4)
(setq c-offsets-alist
      '((arglist-intro c-lineup-arglist-intro-after-pern)))
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76
                        80 84 88 92 96 100 104 108 112 116 120))

; Other settings
(setq-default show-trailing-whitespace t)
(setq-default rst-level-face-base-color nil)
(setq longlines-show-hard-newlines t)
(setq transient-mark-mode t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq vc-handled-backends nil)
(setq inhibit-splash-screen t)
(show-paren-mode 1)
(column-number-mode 1)
(add-hook 'after-change-major-mode-hook '(lambda () (c-subword-mode 1)))
(setq require-final-newline 'visit-save)
(if (eq window-system nil)
    (menu-bar-mode -1))

; Mouse settings
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)
(global-set-key [mouse-4] '(lambda ()
                             (interactive)
                             (scroll-down 1)))

(global-set-key [mouse-5] '(lambda ()
                             (interactive)
                             (scroll-up 1)))

; Bindings
(defun delete-backward-indent (&optional arg)
  "Erase a level of indentation, or 1 character"
  (interactive "*P")
  (if (= (current-column) 0)
      (delete-backward-char 1)
    (let ((n (mod (current-column) standard-indent)))
      (if (looking-back (concat "^\s*" (make-string n 32)))
          (if (= n 0)
              (delete-backward-char standard-indent)
            (delete-backward-char n))
        (delete-backward-char 1)))))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "RET") 'newline-and-indent)
             (define-key python-mode-map (kbd "DEL") 'delete-backward-indent)))
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
             (define-key c-mode-base-map (kbd "DEL") 'delete-backward-indent)))
(add-hook 'css-mode-hook
          '(lambda ()
             (define-key css-mode-map (kbd "RET") 'newline-and-indent)
             (define-key css-mode-map (kbd "DEL") 'delete-backward-indent)))


(global-set-key (kbd "M-[ h") 'beginning-of-line)
(global-set-key (kbd "M-[ f") 'end-of-line)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
(global-set-key (kbd "DEL") 'delete-backward-indent)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x M-s") 'sudo-unset-ro-or-save)
(global-set-key (kbd "C-x M-f") 'sudo-find-file)
(global-set-key (kbd "ESC ESC") 'keyboard-quit)

; Spelling
(add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'rst-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'c-mode-common-hook '(lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook '(lambda () (flyspell-prog-mode)))
(add-hook 'sh-mode-hook '(lambda () (flyspell-prog-mode)))
