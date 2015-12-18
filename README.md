# HOC and NMODL Emacs modes for NEURON

Currently I've two useful resources for the Neuron simulator: a Neuron
HOC mode for emacs and an NMODL mode for emacs. Their main features
are syntax highlighting and syntax-based indentation. They've been
tested under XEmacs 21.4 and Emacs 20.7.1. Andrew Gillies has also
worked on this project.

## Installing nrnhoc.el

* Copy `nrnhoc.el` somewhere in your emacs load path, then add this
to your `.emacs` file:
```
(autoload 'nrnhoc-mode "nrnhoc" "Enter NRNHOC mode." t)
(setq auto-mode-alist (cons '("\\.hoc\\'" . nrnhoc-mode) auto-mode-alist))
```
* Syntax highlighting may work for you automatically, depending on
your emacs setup. If it doesn't work, try putting
```
(add-hook 'nrnhoc-mode-hook 'turn-on-font-lock)
```
somewhere in your `.emacs` file.

## Installing nmodl.el

* Install nrnhoc mode as above.
* Put `nmodl.el` somewhere in your emacs load path, then add this to
your `.emacs` file:
```
(autoload 'nmodl-mode "nmodl" "Enter NMODL mode." t)
(setq auto-mode-alist (cons '("\\.mod\\'" . nmodl-mode) auto-mode-alist))
```


