(cond ((fboundp 'point-at-bol)
       (defalias 'hoc-point-at-bol 'point-at-bol)
       (defalias 'hoc-point-at-eol 'point-at-eol))
      ;; Emacs 20.4
      ((fboundp 'line-beginning-position)
       (defalias 'hoc-point-at-bol 'line-beginning-position)
       (defalias 'hoc-point-at-eol 'line-end-position))
      (t
       (defmacro hoc-point-at-bol ()
	 (save-excursion (beginning-of-line) (point)))
       (defmacro hoc-point-at-eol ()
	 (save-excursion (end-of-line) (point)))))

;(defcustom matlab-indent-level 2
;  "*The basic indentation amount in `matlab-mode'."
;  :group 'matlab
;  :type 'integer)

(setq hoc-indent-level 4)


(defun hoc-mode ()
  "Documentation"
  (interactive)
  (kill-all-local-variables)
;  (use-local-map hoc-mode-map)
  (setq major-mode 'hoc-mode)
  (setq mode-name "Hoc")
  (make-local-variable 'indent-line-function)
;  (setq local-abbrev-table matlab-mode-abbrev-table)
;  (set-syntax-table matlab-mode-syntax-table)
;  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'hoc-indent-line)
;  (make-local-variable 'paragraph-start)
;  (setq paragraph-start (concat "^$\\|" page-delimiter))
;  (make-local-variable 'paragraph-separate)
;  (setq paragraph-separate paragraph-start)
;  (make-local-variable 'paragraph-ignore-fill-prefix)
;  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//\\s-+")
  (make-local-variable 'comment-start)
  (setq comment-start "//")
;  (make-local-variable 'comment-column)
;  (setq comment-column matlab-comment-column)
;  (make-local-variable 'comment-indent-function)
;  (setq comment-indent-function 'matlab-comment-indent)
;  (make-local-variable 'fill-column)
;  (setq fill-column default-fill-column)
;  (make-local-variable 'auto-fill-function)
;  (if matlab-auto-fill (setq auto-fill-function 'matlab-auto-fill))
;  ;; Emacs 20 supports this variable.  This lets users turn auto-fill
;  ;; on and off and still get the right fill function.
;  (make-local-variable 'normal-auto-fill-function)
;  (setq normal-auto-fill-function 'matlab-auto-fill)
;  (make-local-variable 'fill-prefix)
;  (make-local-variable 'imenu-generic-expression)
;  (setq imenu-generic-expression matlab-imenu-generic-expression)
;  ;; Save hook for verifying src.  This lets us change the name of
;  ;; the function in `write-file' and have the change be saved.
;  ;; It also lets us fix mistakes before a `save-and-go'.
;  (make-local-variable 'write-contents-hooks)
;  (add-hook 'write-contents-hooks 'matlab-mode-verify-fix-file-fn)
;  ;; Tempo tags
;  (make-local-variable 'tempo-local-tags)
;  (setq tempo-local-tags (append matlab-tempo-tags tempo-local-tags))
;  ;; give each file it's own parameter history
;  (make-local-variable 'matlab-shell-save-and-go-history)
;  (make-local-variable 'font-lock-defaults)
;  (setq font-lock-defaults '((matlab-font-lock-keywords
;			      matlab-gaudy-font-lock-keywords
;			      matlab-really-gaudy-font-lock-keywords
;			      )
;			     t ; do not do string/comment highlighting
;			     nil ; keywords are case sensitive.
;			     ;; This puts _ as a word constituent,
;			     ;; simplifying our keywords significantly
;			     ((?_ . "w"))))
;  (matlab-enable-block-highlighting 1)
;  (if window-system (matlab-frame-init))
;  (run-hooks 'matlab-mode-hook)
;  (if matlab-vers-on-startup (matlab-show-version)))
)

(defun hoc-calc-indent ()
  "Return the appropriate indentation for this line as an integer."
  (interactive)
  (let
      ((ci                              ; current indentation
       (save-excursion 
         (forward-line -1)
         (current-indentation)
         ))
       ; is there an open bracket on the previous line that isn't 
       ; cancelled by a closed bracket?
       (open-brak                       
        (save-excursion 
          (forward-line -1)
          (cond ((and
                  (string-match "{" (buffer-substring (hoc-point-at-bol) (hoc-point-at-eol)))
                  (not (string-match "}" (buffer-substring (hoc-point-at-bol) (hoc-point-at-eol)) )))
                 1) (t 0))))
       ; is there an closing bracket on this line that isn't 
       ; cancelled by a opening bracket?
       (close-brak              
        (save-excursion 
          (cond ((and 
                  (string-match "}" (buffer-substring (hoc-point-at-bol) (hoc-point-at-eol))) 
                  (not (string-match "{" (buffer-substring (hoc-point-at-bol) (hoc-point-at-eol)))))
                 1) 
                (t 0)))))
;    (prin1 open-brak)
;    (prin1 close-brak)
;    (prin1 ci)
    (+ ci (* open-brak hoc-indent-level) (* close-brak (- hoc-indent-level)))))


(defun hoc-indent-line ()
  (interactive)
    ; Is the previous line blank, i.e. does not contain any word characters
  (forward-line -1)
  (cond ((not 
         (string-match "\\w" (buffer-substring (hoc-point-at-bol) (hoc-point-at-eol))))
;         (prin1 "blank")
         (hoc-indent-line)))
  (forward-line 1)
  (indent-line-to (hoc-calc-indent)))

;(defvar hoc-mode-map
;  (let ((km (make-sparse-keymap)))
;    (define-key km [return] 'matlab-return)

