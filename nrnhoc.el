;;; nrnhoc.el --- major mode for Neuron HOC dot-hoc files
;;
;; Author: David C. Sterratt <david.c.sterratt@ed.ac.uk>
;; Maintainer: David C. Sterratt <david.c.sterratt@ed.ac.uk>
;; Created: 03 Mar 03
;; Version: 0.3.3
;; Keywords: HOC, NEURON
;;
;; Copyright (C) 2003 David C. Sterratt and Andrew Gillies
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
;; This major mode for GNU Emacs provides support for editing Neuron HOC
;; dot-hoc files.  It automatically indents for block structures and
;; comments.  It highlights code using font-lock.
;;
;;; Finding Updates:
;;
;; The latest stable version of nrnhoc.el can be found here:
;;
;; http://www.anc.ed.ac.uk/~dcs/progs/neuron/nrnhoc.el
;;
;;; Installation:
;;
;;   Put the this file as "nrnhoc.el" somewhere on your load path, then
;;   add this to your .emacs or init.el file:
;;
;;   (autoload 'nrnhoc-mode "nrnhoc" "Enter NRNHOC mode." t)
;;   (setq auto-mode-alist (cons '("\\.hoc\\'" . nrnhoc-mode) auto-mode-alist))
;;
;; Please read the mode help for nrnhoc-mode for configuration options.
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

(defconst nrnhoc-mode-version "0.3.3"
  "Current version of NRNHOC mode.")

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
       (defalias 'nrnhoc-point-at-bol 'point-at-bol)
       (defalias 'nrnhoc-point-at-eol 'point-at-eol))
      ;; Emacs 20.4
      ((fboundp 'line-beginning-position)
       (defalias 'nrnhoc-point-at-bol 'line-beginning-position)
       (defalias 'nrnhoc-point-at-eol 'line-end-position))
      (t
       (defmacro nrnhoc-point-at-bol ()
	 (save-excursion (beginning-of-line) (point)))
       (defmacro nrnhoc-point-at-eol ()
	 (save-excursion (end-of-line) (point)))))


;;; User-changeable variables =================================================

;; Variables which the user can change
(defgroup nrnhoc nil
  "NRNHOC mode."
  :prefix "nrnhoc-"
  :group 'languages)

(defcustom nrnhoc-mode-hook nil
  "*List of functions to call on entry to NRNHOC mode."
  :group 'nrnhoc
  :type 'hook)

(defcustom nrnhoc-indent-level 4
  "*The basic indentation amount in `nrnhoc-mode'."
  :group 'nrnhoc
  :type 'integer)

(defcustom nrnhoc-comment-column 40
  "*The goal comment column in `nrnhoc-mode' buffers."
  :group 'nrnhoc
  :type 'integer)


;;; NRNHOC mode variables =====================================================


;;; Keybindings ===============================================================

;; mode map
(defvar nrnhoc-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [return] 'nrnhoc-return)
    (define-key km [}] 'nrnhoc-closing-brace)
    km)
  "The keymap used in `nrnhoc-mode'.")


;;; Font locking keywords =====================================================

(defvar nrnhoc-font-lock-keywords 
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
        ; Section stuff: neuron/geometry.html
        ("\\<\\(access\\|forsec\\|pop_section\
\\|forall\\|ifsec\\|push_section\
\\|diam3d\\|pt3dchange\\|setSpineArea\
\\|pt3dclear\\|spine3d\
\\|arc3d\\|distance\\|pt3dconst\\|x3d\
\\|area\\|getSpineArea\\|pt3dinsert\\|y3d\
\\|define_shape\\|n3d\\|pt3dremove\\|z3d\
\\|pt3dadd\\|ri\
\\|connect\\|delete_section\
\\|create\\|disconnect\\|topology\\)\\>" . 1)
        ; Built-in Variables: neuron/geometry.html
        ("\\<\\(L\\|Ra\\|diam\\|nseg\\|diam_changed\\)\\>" .
         font-lock-variable-name-face)
        ; Built-in global varibailes: neuron/1nrn.html#globals
        ("\\<\\(celsius\\|dt\\|t\\|clamp_resist\\|secondorder\\)\\>" . 
         font-lock-variable-name-face) 
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
        ; neuron/nrnoc.html#Objects
        ("\\<\\(Random\\|Vector\\|Pointer\\|Graph\\)\\>" . 1)
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
        "Expressions to highlight in NRNHOC mode.")


;;; NRNHOC mode entry point ==================================================

(defun nrnhoc-mode ()
  "NRNHOC-mode is a major mode for editing Neuron HOC dot-hoc files.
\\<nrnhoc-mode-map>

Variables:
  `nrnhoc-indent-level'		       Level to indent blocks.
  `nrnhoc-comment-column'		     The goal comment column
  `fill-column'			           Column used in auto-fill.
  `nrnhoc-return-function'  	     Customize RET handling with this function
  `nrnhoc-closing-brace-function' Customize } handling with this function

All Key Bindings:
\\{nrnhoc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map nrnhoc-mode-map)
  (setq major-mode 'nrnhoc-mode)
  (setq mode-name "Hoc")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'nrnhoc-indent-line)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//\\s-+")
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'comment-column)
  (setq comment-column nrnhoc-comment-column)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((nrnhoc-font-lock-keywords)
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
  (run-hooks 'nrnhoc-mode-hook)
  )


;;; Indent functions ==========================================================

(defun nrnhoc-point-at-eol-or-boc ()
  "Return the location of the point at the end of the line or the beginning of a //-style comment."
  (let 
      ((comment
       (string-match "//" (buffer-substring (nrnhoc-point-at-bol) (nrnhoc-point-at-eol)))))
    (if comment
        (+ (point-at-bol) comment )
      (point-at-eol))))
        
(defun nrnhoc-calc-indent ()
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
                  (string-match "{" (buffer-substring (nrnhoc-point-at-bol) (nrnhoc-point-at-eol-or-boc)))

                  (not (string-match "{.*}" (buffer-substring (nrnhoc-point-at-bol) (nrnhoc-point-at-eol-or-boc)) )))
                 1) (t 0))))
       ; is there an closing bracket on this line that isn't 
       ; cancelled by a opening bracket?
       (close-brak              
        (save-excursion 
          (cond ((and 
                  (string-match "}" (buffer-substring (nrnhoc-point-at-bol) (nrnhoc-point-at-eol-or-boc))) 
                  (not (string-match "{.*}" (buffer-substring (nrnhoc-point-at-bol) (nrnhoc-point-at-eol-or-boc)))))
                 1) 
                (t 0))))
       )
;    (prin1 open-brak)
;    (prin1 close-brak)
;    (prin1 ci)
    (+ ci 
       (* open-brak nrnhoc-indent-level) (* close-brak (- nrnhoc-indent-level)
       ))))


(defun nrnhoc-indent-line ()
  (interactive)
  (save-excursion 
    (cond
     ; Are we on the first line of the buffer?  If so, just indent to 0
     ((= (line-number) 1)   (indent-line-to 0))
     ; Otherwise do the main indent routine
     ; Is the previous line blank, i.e. does not contain any word characters? 
     ; If so, we should indent it
     (t 
      (forward-line -1)
      (cond ((not 
              (string-match "\\w" (buffer-substring (nrnhoc-point-at-bol) (nrnhoc-point-at-eol))))
             (nrnhoc-indent-line)))
      (forward-line 1)
      ; Finally we indent this line
      (indent-line-to (nrnhoc-calc-indent)))))
  ; Is this line blank?  If so put the point at the end
      (cond ((not
             (string-match "\\w" (buffer-substring (nrnhoc-point-at-bol) (nrnhoc-point-at-eol))))
             (end-of-line)))
    )


;;; The return key ============================================================

(defcustom nrnhoc-return-function 'nrnhoc-indent-before-ret
  "Function to handle return key.
Must be one of:
    'nrnhoc-plain-ret
    'nrnhoc-indent-after-ret
    'nrnhoc-indent-before-ret"
  :group 'hoc
  :type '(choice (function-item nrnhoc-plain-ret)
		 (function-item nrnhoc-indent-after-ret)
		 (function-item nrnhoc-indent-before-ret)))

(defun nrnhoc-return ()
  "Handle carriage return in `nrnhoc-mode'."
  (interactive)
  (funcall nrnhoc-return-function))

(defun nrnhoc-plain-ret ()
  "Vanilla new line."
  (interactive)
  (newline))
  
(defun nrnhoc-indent-after-ret ()
  "Indent after new line."
  (interactive)
  (newline)
  (nrnhoc-indent-line))

(defun nrnhoc-indent-before-ret ()
  "Indent line, start new line, and indent again."
  (interactive)
  (newline)
  (forward-line -1)
  (nrnhoc-indent-line)
  (forward-line 1)
  (nrnhoc-indent-line))

(defcustom nrnhoc-closing-brace-function 'nrnhoc-plain-closing-brace
  "Function to handle \"}\" key.
Must be one of:
    'nrnhoc-plain-closing-brace
    'nrnhoc-electric-closing-brace
    "
  :group 'hoc
  :type '(choice (function-item nrnhoc-plain-closing-brace)
		 (function-item nrnhoc-electric-closing-brace)))

(defun nrnhoc-closing-brace ()
  "Handle closing brace in `nrnhoc-mode'."
  (interactive)
  (funcall nrnhoc-closing-brace-function))

(defun nrnhoc-plain-closing-brace ()
  "Insert closing brace."
  (interactive)
  (insert-char ?} ))

(defun nrnhoc-electric-closing-brace ()
  "Insert closing brace and indent."
  (interactive)
  (insert-char ?} )
  (nrnhoc-indent-line)
  (end-of-line))



;;; Change log
;;; $Log: nrnhoc.el,v $
;;; Revision 1.16  2003/05/08 16:40:59  sterratt
;;; * Fixed bug in documentation
;;;
;;; Revision 1.15  2003/03/11 12:43:31  dcs
;;; * Version 0.3.3
;;; * Fixed fatal bug due to brackets left from removing comment in
;;;   previous version
;;;
;;; Revision 1.14  2003/03/10 15:18:37  dcs
;;; * Version 0.3.2
;;; * Bug fix: closing and opening bracket on same line (e.g. " } else { ")
;;;   now are treated as closing bracket
;;; * Changed function hoc-calc-indent
;;;
;;; Revision 1.13  2003/03/07 17:58:31  dcs
;;; * Version 0.3.1
;;; * Bug fix: tab on empty line puts the point at the end of the line now
;;; * Changed function: hoc-indent-line
;;;
;;; Revision 1.12  2003/03/07 13:08:23  dcs
;;; * Version 0.3
;;; * New feature: electric closing braces
;;; * Bug fix: Indentation of comments with braces in them
;;; * Bug fix: Indentation of first line of buffer
;;; * Bug fix: Point location in line doesn't change when indenting now
;;; * New functions: hoc-closing-brace, hoc-plain-closing-brace,
;;;   hoc-electric-closing-brace
;;; * New variable: hoc-closing-brace-function
;;; * New feature: celsius, dt, t, clamp_resist, secondorder fontified
;;; * New function: hoc-point-at-eol-or-boc
;;; * Changed functions: hoc-calc-indent, hoc-indent-line
;;;
;;; Revision 1.11  2003/03/06 13:55:41  dcs
;;; * Version 0.2.2
;;; * Another modification to hoc-indent-before-ret to fix the bug better
;;;
;;; Revision 1.10  2003/03/06 13:48:00  dcs
;;; * Version 0.2.1
;;; * Fixed bug in hoc-indent-before-ret that caused a new line to be
;;;   inserted before the current line
;;;
;;; Revision 1.9  2003/03/06 12:53:33  dcs
;;; * Version 0.2
;;; * Changed font-lock-face of L, Ra, diam, nseg and diam_changed to
;;;   font-lock-variable-name-face
;;; * Changed URL
;;;
;;; Revision 1.8  2003/03/06 12:39:42  anaru
;;; added some hoc Objects highlighting
;;;
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