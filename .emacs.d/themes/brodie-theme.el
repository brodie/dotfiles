;;; brodie-theme.el

(deftheme brodie
  "Grey on black.")

(custom-theme-set-faces
  'brodie
  '(isearch ((t (:background "pink"))))
  '(lazy-highlight ((t (:background "dark blue"))))
  '(font-lock-comment-face ((t (:foreground "red"))))
  '(font-lock-string-face ((t (:foreground "green"))))
  '(font-lock-keyword-face ((t (:foreground "cyan" :bold t))))
  '(font-lock-builtin-face ((t (:foreground "blue" :bold t))))
  '(font-lock-function-name-face ((t (:foreground "blue" :bold t))))
  '(font-lock-variable-name-face ((t (:foreground "light salmon"))))
  '(font-lock-type-face ((t (:foreground "yellow"))))
  '(font-lock-constant-face ((t (:foreground "purple"))))
  '(font-lock-warning-face ((t (:foreground "gold" :bold t)))))

(provide-theme 'brodie)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; brodie-theme.el ends here
