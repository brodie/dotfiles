; Set up GUI-related stuff as soon as possible
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(background-color . "black"))
  (add-to-list 'default-frame-alist '(width . 162))
  (add-to-list 'default-frame-alist '(height . 60))

  ; Set PATH/exec-path based on the shell's configuration
  (defun set-path-from-shell ()
    (if (get-buffer "*set-path-from-shell*")
        (kill-buffer "*set-path-from-shell*"))
    (call-process-shell-command "zsh -c 'echo $PATH'" nil
                                "*set-path-from-shell*")
    (with-current-buffer "*set-path-from-shell*"
      (let ((output (buffer-substring (point-min) (- (point-max) 1)))
            (emacs-path (nth 0 (last exec-path))))
        (setenv "PATH" (concat output emacs-path))
        (setq exec-path `(,@(split-string output ":") ,emacs-path))
        (kill-buffer "*set-path-from-shell*"))))
  (set-path-from-shell))

; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'brodie)
(add-to-list 'load-path "~/.emacs.d/plugins")

; Mouse wheel scrolling in xterm
(add-hook 'server-switch-hook
	  (lambda ()
	    (unless window-system
	      (require 'mouse)
	      (xterm-mouse-mode 1)
	      (global-set-key [mouse-4] '(lambda ()
					   (interactive)
					   (scroll-down 1)))
	      (global-set-key [mouse-5] '(lambda ()
					   (interactive)
					   (scroll-up 1))))))

; Plugins
(require 'sudo) ; open/save files with sudo
(require 'flymake-point) ; shows errors in the minibuffer when highlighted
;(autoload 'js2-mode "js2" nil t)
;(autoload 'rst-mode "rst" nil t) ; restructured text mode
(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'css-mode "css-mode" nil t)

; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/Documents/work.org"
                             "~/Documents/school.org"
                             "~/Documents/home.org"))

; File/mode associations
(add-to-list 'auto-mode-alist '("\\.md$\\|\\.text$" . markdown-mode))

; Indentation settings
(setq-default indent-tabs-mode nil) ; disable tab character insertion
(setq standard-indent 4)
(setq c-default-style "bsd") ; brackets go on a separate line
(setq c-basic-offset 4)
(setq-default c-indent-level 4)
(setq-default js-indent-level 2)
; line up args on separate lines with opening parens
(setq c-offsets-alist
      '((arglist-intro c-lineup-arglist-intro-after-paren)))
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76
                        80 84 88 92 96 100 104 108 112 116 120))

; Other settings
(setq server-kill-new-buffers nil) ; preserve new buffers when closing clients
(setq server-temp-file-regexp (substitute-in-file-name "^$TMPDIR/.*"))
(when (fboundp 'set-fringe-style) (set-fringe-style 'none)) ; disable fringes
(setq-default show-trailing-whitespace t)
(setq-default rst-level-face-base-color nil)
(setq longlines-show-hard-newlines t)
(setq transient-mark-mode t) ; highlight marked text (i.e. selected text)
(setq vc-handled-backends nil)
(setq inhibit-splash-screen t)
(show-paren-mode t) ; highlight matching parens
(column-number-mode t) ; show the column number in the status bar
(delete-selection-mode 1) ; backspace deletes selected text
(savehist-mode 1) ; save command history
; camel case word navigation
(add-hook 'after-change-major-mode-hook '(lambda () (subword-mode 1)))
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
(setq js-auto-indent-flag nil)
(setq js2-highlight-level 3)
(setq js2-mirror-mode nil)
(setq js2-language-version 150)
(setq js2-rebind-eol-bol-keys nil)
(setq dabbrev-case-distinction nil)

; ropemacs stuff
(setq ropemacs-guess-project t)
(setenv "PYTHONPATH" (concat (expand-file-name "~/.emacs.d/python") ":"
                             (getenv "PYTHONPATH")))

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
             (require 'pymacs)
             (pymacs-load "ropemacs" "rope-")
             (ropemacs-mode 1)
             (hs-minor-mode 1)
             (define-key python-mode-map (kbd "RET") 'newline-maybe-indent)
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
(add-hook 'js-mode-hook
          '(lambda ()
             (hs-minor-mode 1)
             (define-key js-mode-map (kbd "RET") 'newline-and-indent)
             (define-key js-mode-map (kbd "DEL") 'delete-backward-indent)
             (define-key js-mode-map (kbd "M-RET") 'hs-toggle-hiding)))
(add-hook 'ido-setup-hook
          '(lambda ()
             (define-key ido-completion-map (kbd "SPC") 'self-insert-command)))

; Fix js2-mode's completely braindead indentation
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node)
      (save-excursion
        (back-to-indentation)
        ;; align consecutive declarations in a var statement
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(add-hook 'js2-mode-hook
          '(lambda ()
             (require 'js)
             (set (make-local-variable 'indent-line-function)
                  'my-js2-indent-function)
             (define-key js2-mode-map (kbd "RET") 'newline-and-indent)))

; Extra bindings for various terminals
(global-set-key (kbd "M-[ h") 'beginning-of-line)
(global-set-key (kbd "M-[ f") 'end-of-line)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
(global-set-key (kbd "DEL") 'delete-backward-indent)
(global-set-key (kbd "C-x M-s") 'sudo-unset-ro-or-save)
(global-set-key (kbd "C-x M-f") 'sudo-find-file)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(when (fboundp 'ns-toggle-fullscreen)
  (global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen))
(when window-system
  (global-set-key (kbd "s-1")
                  (lambda ()
                    (interactive)
                    (set-frame-width (selected-frame) 80)
                    (balance-windows)))
  (global-set-key (kbd "s-2")
                  (lambda ()
                    (interactive)
                    (set-frame-width (selected-frame) 160)
                    (balance-windows)))
  (global-set-key (kbd "s-3")
                  (lambda ()
                    (interactive)
                    (set-frame-width (selected-frame) 240)
                    (balance-windows))))

; On-the-fly spell checking
(setq flyspell-issue-message-flag nil)
(add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'rst-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'c-mode-common-hook '(lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook '(lambda () (flyspell-prog-mode)))
(add-hook 'sh-mode-hook '(lambda () (flyspell-prog-mode)))

; On-the-fly pyflakes/pep8 checking
(setq python-check-command "flypy")
(setq python-saved-check-command "flypy")
(when (load "flymake" t)
  (defun flymake-flypy-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "flypy" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-flypy-init)))
(add-hook 'python-mode-hook '(lambda () (flymake-mode 1)))

; Hide uninteresting files
(eval-after-load "dired" '(require 'dired-x))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
