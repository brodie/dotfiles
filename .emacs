; Set up GUI-related stuff as soon as possible
(when window-system
  (if (eq system-type 'darwin)
      (add-to-list 'default-frame-alist '(alpha 92 92))
    (menu-bar-mode -1))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (server-start))

; Set PATH/exec-path based on the shell's configuration
(defun set-path-from-shell ()
  (if (get-buffer "*set-path-from-shell*")
      (kill-buffer "*set-path-from-shell*"))
  (call-process-shell-command "echo $PATH" nil "*set-path-from-shell*")
  (with-current-buffer "*set-path-from-shell*"
    (let ((output (buffer-substring (point-min) (- (point-max) 1)))
          (emacs-path (nth 0 (last exec-path))))
      (setenv "PATH" (concat output emacs-path))
      (setq exec-path `(,@(split-string output ":") ,emacs-path))
      (kill-buffer))))
(set-path-from-shell)

; Plugins
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'color-theme) ; load color themes
(require 'color-theme-brodie)
(color-theme-brodie) ; set color theme
(require 'sudo) ; open/save files with sudo
(require 'flymake-point) ; shows errors in the minibuffer when highlighted
(autoload 'yas/minor-mode "yasnippet-bundle" nil t) ; like TextMate snippets
(autoload 'js2-mode "js2" nil t)
(autoload 'rst-mode "rst" nil t) ; restructured text mode
(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'pod-mode "pod-mode" nil t)
(autoload 'po-mode "po-mode" nil t)
(autoload 'css-mode "css-mode" nil t)

; File/mode associations
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.\\." . po-mode))
(add-to-list 'auto-mode-alist '("\\.css$\\|\\.css\\.dtml$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.pt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))

; Indentation settings
(setq-default indent-tabs-mode nil) ; disable tab character insertion
(setq standard-indent 4)
(setq c-default-style "bsd") ; brackets go on a separate line
(setq c-basic-offset 4)
(setq-default c-indent-level 4)
; line up args on separate lines with opening parens
(setq c-offsets-alist
      '((arglist-intro c-lineup-arglist-intro-after-pern)))
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76
                        80 84 88 92 96 100 104 108 112 116 120))

; Other settings
(set-fringe-style 'none) ; disable fringes
(setq-default show-trailing-whitespace t)
(setq-default rst-level-face-base-color nil)
(setq longlines-show-hard-newlines t)
(setq transient-mark-mode t) ; highlight marked text (i.e. selected text)
;(setq vc-handled-backends nil)
(setq inhibit-splash-screen t)
(show-paren-mode t) ; highlight matching parens
(column-number-mode t) ; show the column number in the status bar
(delete-selection-mode 1) ; backspace deletes selected text
(savehist-mode 1) ; save command history
; camel case word navigation
(add-hook 'after-change-major-mode-hook '(lambda () (c-subword-mode 1)))
; add missing trailing newline one both visit and save
(setq require-final-newline 'visit-save)
(ido-mode t) ; fancy file navigation
(setq ido-enable-flex-matching t)
(setq mac-option-modifier 'meta) ; option is alt
; make sentence navigation more useful
(setq sentence-end-double-space nil)
(setq sentence-end "[.?!][]\"')]\\($\\|\t\\| \\)[ \t\n]")
; handle duplicate buffer names better
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator ":")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

; Disable the fringe for all frames
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))

; Smart backup/autosave into secure directory in /tmp (instead of littering)
(defvar user-temporary-file-directory
  (concat temporary-file-directory "emacs-" user-login-name "/"))
(make-directory user-temporary-file-directory t)
(set-file-modes user-temporary-file-directory #o700)

(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq backup-enable-predicate '(lambda (name)
  (and (normal-backup-enable-predicate name)
       (eq (nth 2 (file-attributes name)) (user-uid)))))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory "auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

; Mouse wheel scrolling in xterm
(unless window-system
  (xterm-mouse-mode 1)
  (mouse-wheel-mode 1)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1))))

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

(defun newline-maybe-indent ()
  "Like newline-and-indent, but doesn't indent if the previous line is blank"
  (interactive "*")
  (if (= (line-beginning-position) (line-end-position))
      (newline)
    (newline-and-indent)))

(add-hook 'python-mode-hook
          '(lambda ()
             (hs-minor-mode 1)
             (define-key python-mode-map (kbd "RET") 'newline-maybe-indent)
             (define-key python-mode-map (kbd "DEL") 'delete-backward-indent)
             (define-key python-mode-map (kbd "M-RET") 'hs-toggle-hiding)))
(add-hook 'c-mode-common-hook
          '(lambda ()
             (hs-minor-mode 1)
             (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
             (define-key c-mode-base-map (kbd "DEL") 'delete-backward-indent)
             (define-key c-mode-base-map (kbd "M-RET") 'hs-toggle-hiding)))
(add-hook 'css-mode-hook
          '(lambda ()
             (hs-minor-mode 1)
             (define-key css-mode-map (kbd "RET") 'newline-and-indent)
             (define-key css-mode-map (kbd "DEL") 'delete-backward-indent)
             (define-key css-mode-map (kbd "M-RET") 'hs-toggle-hiding)))
(add-hook 'ido-setup-hook
          '(lambda ()
             (define-key ido-completion-map (kbd "SPC") 'self-insert-command)))

; Extra bindings for various terminals
(global-set-key (kbd "M-[ h") 'beginning-of-line)
(global-set-key (kbd "M-[ f") 'end-of-line)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
(global-set-key (kbd "DEL") 'delete-backward-indent)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x M-s") 'sudo-unset-ro-or-save)
(global-set-key (kbd "C-x M-f") 'sudo-find-file)
(global-set-key (kbd "C--") 'undo)
; FIXME: This needs something like vim's ttimeout setting
;(global-set-key (kbd "ESC ESC") 'keyboard-quit)

; On-the-fly spell checking
(add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'rst-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'c-mode-common-hook '(lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook '(lambda () (flyspell-prog-mode)))
(add-hook 'sh-mode-hook '(lambda () (flyspell-prog-mode)))

; On-the-fly pyflakes checking
(setq python-check-command "pyflakes")
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook '(lambda () (flymake-mode 1)))

; When dealing with two side-by-side windows, automatically resize the frame
(defadvice split-window-horizontally (before resize-window)
  (set-frame-width (selected-frame) 160))
(ad-activate 'split-window-horizontally)

(defadvice delete-window (after resize-window)
  (if (= (count-windows) 1)
      (set-frame-width (selected-frame) 80)))
(ad-activate 'delete-window)

(defadvice delete-other-windows (after resize-window)
  (if (= (count-windows) 1)
      (set-frame-width (selected-frame) 80)))
(ad-activate 'delete-other-windows)
