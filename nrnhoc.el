;;; hoc.el --- major mode for HOC dot-hoc files
;;
;; Author: David C. Sterratt <david.c.sterratt@ed.ac.uk>
;; Maintainer: David C. Sterratt <david.c.sterratt@ed.ac.uk>
;; Created: 03 Mar 03
;; Version: 0.2
;; Keywords: HOC, NEURON
;;
;; Copyright (C) 2003 David C. Sterratt
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; This major mode for GNU Emacs provides support for editing HOC
;; dot-hoc files.  It automatically indents for block structures and
;; comments.  It highlights code using font-lock.
;;
;;; Finding Updates:
;;
;; The latest stable version of hoc.el can be found here:
;;
;; http://www.anc.ed.ac.uk/~dcs/progs/hoc-mode/hoc.el
;;
;;; Installation:
;;
;;   Put the this file as "hoc.el" somewhere on your load path, then
;;   add this to your .emacs or init.el file:
;;
;;   (autoload 'hoc-mode "hoc" "Enter HOC mode." t)
;;   (setq auto-mode-alist (cons '("\\.hoc\\'" . hoc-mode) auto-mode-alist))
;;
;; Please read the mode help for hoc-mode for configuration options.
;;
;; Syntax highlighting:
;;   To get font-lock try adding this for older emacsen:
;;     (font-lock-mode 1)
;;   Or for newer versions of Emacs:
;;     (global-font-lock-mode t)
;;
;; This package will optionally use custom.
;;
;; It is partly based on Eric M. Ludlam's and Matthew R. Wette's matlab-mode
;;

;;; Code:

(defconst hoc-mode-version "0.2"
  "Current version of HOC mode.")

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro custom-add-option (&rest args)
      nil)
    (defmacro defface (&rest args) nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

;; compatibility

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


;;; User-changeable variables =================================================

;; Variables which the user can change
(defgroup hoc nil
  "HOC mode."
  :prefix "hoc-"
  :group 'languages)

(defcustom hoc-mode-hook nil
  "*List of functions to call on entry to HOC mode."
  :group 'hoc
  :type 'hook)

(defcustom hoc-indent-level 4
  "*The basic indentation amount in `hoc-mode'."
  :group 'hoc
  :type 'integer)

(defcustom hoc-comment-column 40
  "*The goal comment column in `hoc-mode' buffers."
  :group 'hoc
  :type 'integer)


;;; HOC mode variables =====================================================


;;; Keybindings ===============================================================

;; mode map
(defvar hoc-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [return] 'hoc-return)
    km)
  "The keymap used in `hoc-mode'.")


;;; Font locking keywords =====================================================

(defvar hoc-font-lock-keywords 
      '(
;        ("//.*" . font-lock-comment-face)
;        ("/\\*[^\\*]*\\*/" . font-lock-comment-face)
        ; Keywords (proc and func are syntax)
        ("\\<\\(break\\|else\\|insert\\|stop\\|\\|continue\\|em\
\\|local\\|strdef\\|\\|debug\\|eqn\\|print\\|uninsert\\|\\|delete\
\\|for\\|read\\|while\\|\\|depvar\\|help\\|return\\|\\|double\
\\|if\\|setpointer\\|proc\\|func\\)\\>" . 1)
        ; Object-oriented-programming
        ("\\<\\(begintemplate\\|init\\|objref\\|endtemplate\\|new\
\\|public\\|external\\|objectvar\\|unref\\)\\>" . 1)
        ; Section stuff
        ("\\<\\(access\\|forsec\\|pop_section\
\\|forall\\|ifsec\\|push_section\
\\|L\\|diam3d\\|pt3dchange\\|setSpineArea\
\\|Ra\\|diam_changed\\|pt3dclear\\|spine3d\
\\|arc3d\\|distance\\|pt3dconst\\|x3d\
\\|area\\|getSpineArea\\|pt3dinsert\\|y3d\
\\|define_shape\\|n3d\\|pt3dremove\\|z3d\
\\|diam\\|pt3dadd\\|ri\
\\|connect\\|delete_section\\|nseg\
\\|create\\|disconnect\\|topology\\)\\>" . 1)
        ; neuron/nrnoc.html#functions
        ("\\<\\(attr_praxis\\|fit_praxis\\|nrnmechmenu\\|secname\
\\|batch_run\\|fmatrix\\|nrnpointmenu\\|section_orientation\
\\|batch_save\\|fstim\\|nrnsecmenu\\|sectionname\
\\|fadvance\\|fstimi\\|parent_connection\\|stop_praxis\
\\|fclamp\\|hocmech\\|parent_node\\|this_node\
\\|fclampi\\|initnrn\\|parent_section\\|this_section\
\\|fclampv\\|ismembrane\\|prstim\
\\|fcurrent\\|issection\\|psection\
\\|finitialize\\|nrnglobalmechmenu\\|pval_praxis\\)\\>" . 1)
        ; Functions
        ("\\<\\(fprint\\|hoc_stdio\\|sred\\|xred\
\\|fscan\\|printf\\|wopen\
\\|getstr\\|ropen\\|xopen\
\\|doEvents\\|doNotify\
\\|hoc_pointer_\\|object_pop\\|sprint\
\\|ivoc_style\\|object_push\\|sscanf\
\\|allobjects\\|load_file\\|obsolete\\|startsw\
\\|allobjectvars\\|load_func\\|print_session\\|stopsw\
\\|chdir\\|load_proc\\|prmat\\|stopwatch\
\\|checkpoint\\|load_template\\|pwman_place\\|strcmp\
\\|coredump_on_error\\|machine_name\\|quit\\|symbols\
\\|dialogs\\|math\\|retrieveaudit\\|system\
\\|eqinit\\|name_declared\\|save_session\\|units\
\\|execute\\|neuronhome\\|saveaudit\\|variable_domain\
\\|execute1\\|numarg\\|show_errmess_always\
\\|getcwd\\|object_id\\|solve\\)\\>" . font-lock-function-name-face)
;        ("\\<\\(break\\|ca\\(se\\|tch\\)\\|e\\(lse\\(\\|if\\)\\|ndfunction\\)\
;\\|for\\|global\\|if\\|otherwise\\|return\\|switch\\|try\\|while\\)\\>" . 1)
        )
        "Expressions to highlight in HOC mode.")


;;; HOC mode entry point ==================================================

(defun hoc-mode ()
  "HOC-mode is a major mode for editing HOC dot-hoc files.
\\<hoc-mode-map>

Variables:
  `hoc-indent-level'		  Level to indent blocks.
  `hoc-comment-column'		The goal comment column
  `fill-column'			      Column used in auto-fill.
  `hoc-return-function'  	Customize RET handling with this function

All Key Bindings:
\\{hoc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map hoc-mode-map)
  (setq major-mode 'hoc-mode)
  (setq mode-name "Hoc")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'hoc-indent-line)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//\\s-+")
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'comment-column)
  (setq comment-column hoc-comment-column)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((hoc-font-lock-keywords)
                             nil ; do not do string/comment highlighting
                             nil ; keywords are case sensitive.
                             ;; This puts _ as a word constituent,
                             ;; simplifying our keywords significantly
                             ((?_ . "w")
                              (?\n . "> b")
                              (?/ . ". 1456")
                              (?* . ". 23")   
                              (?\^m . "> b")
                              )))
  (run-hooks 'hoc-mode-hook)
  )


;;; Indent functions ==========================================================

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


;;; The return key ============================================================

(defcustom hoc-return-function 'hoc-indent-before-ret
  "Function to handle return key.
Must be one of:
    'hoc-plain-ret
    'hoc-indent-after-ret
    'hoc-indent-before-ret"
  :group 'hoc
  :type '(choice (function-item hoc-plain-ret)
		 (function-item hoc-indent-after-ret)
		 (function-item hoc-indent-before-ret)))

(defun hoc-return ()
  "Handle carriage return in `hoc-mode'."
  (interactive)
  (funcall hoc-return-function))

(defun hoc-plain-ret ()
  "Vanilla new line."
  (interactive)
  (newline))
  
(defun hoc-indent-after-ret ()
  "Indent after new line."
  (interactive)
  (newline)
  (hoc-indent-line))

(defun hoc-indent-before-ret ()
  "Indent line, start new line, and indent again."
  (interactive)
  (hoc-indent-line)
  (newline)
  (hoc-indent-line))



;;; Change log
;;; $Log: nrnhoc.el,v $
;;; Revision 1.7  2003/03/06 12:23:49  dcs
;;; * Return now optionally auto-indents lines
;;; * Added variable hoc-mode-map
;;; * Added variable hoc-return-function
;;; * Added function hoc-return
;;; * Added function hoc-plain-ret
;;; * Added function hoc-indent-after-ret
;;; * Added function hoc-indent-before-ret
;;;
;;; Revision 1.6  2003/03/03 16:24:25  dcs
;;; * Release 0.1
;;; * Quotes in comment font locking fixed
;;; * Documentation added
;;; * User variables optionally controlled by custom
;;; * New variable hoc-comment-column
;;;

;;; hoc.el ends here