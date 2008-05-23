(add-to-list 'load-path "~/.emacs.d/plugins")

; Plugins
;(require 'yasnippet-bundle)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(autoload 'markdown-mode "markdown-mode.el")

; Indentation settings
(setq-default indent-tabs-mode nil)
(setq standard-indent 4)
(setq c-default-style "bsd")
(setq c-basic-offset 4)
(setq-default c-indent-level 4)
(setq c-offsets-alist
      '((arglist-intro c-lineup-arglist-intro-after-pern)))

; Other settings
(setq-default show-trailing-whitespace t)
(setq longlines-show-hard-newlines t)
(setq transient-mark-mode t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq vc-handled-backends nil)
(setq inhibit-splash-screen t)
(show-paren-mode 1)
(column-number-mode 1)
(menu-bar-mode -1)

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
             (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)))
(global-set-key (kbd "M-[ h") 'beginning-of-line)
(global-set-key (kbd "M-[ f") 'end-of-line)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
(global-set-key (kbd "DEL") 'delete-backward-indent)