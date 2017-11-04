
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-configuration-layers
   '(
     python
     clojure
     (shell :variables shell-default-term-shell "fish")
     yaml
     helm
     auto-completion
     emacs-lisp
     git
     markdown
     org
     spell-checking
     syntax-checking
     )))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   ;; gnutls-trustfiles '("/etc/ssl/cert.pem")
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(wombat
                         spacemacs-dark
                         spacemacs-light)
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font
    (cond ((eq system-type 'darwin)    '("Monaco"
                                         :size 10
                                         :weight normal
                                         :width normal
                                         :powerline-scale 1.1))
          ((eq system-type 'gnu/linux) '("Monospace"
                                         :size 12
                                         :weight normal
                                         :width normal
                                         :powerline-scale 1.1)))
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq ispell-dictionary "english")
  (setq custom-file "~/.spacemacs.d/custom.el"))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq powerline-default-separator nil)
  (evil-set-initial-state 'term-mode 'emacs)
  (when (file-exists-p custom-file)
    (load-file custom-file)))
