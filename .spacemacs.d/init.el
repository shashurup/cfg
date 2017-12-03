
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
      shell-scripts
      javascript
      sql
     )
   dotspacemacs-additional-packages '(org-jira)))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   gnutls-trustfiles '("/etc/ssl/cert.pem")
   dotspacemacs-elpa-timeout 5
   dotspacemacs-verbose-loading t
   dotspacemacs-themes '(wombat
                         spacemacs-dark
                         spacemacs-light)
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
  ;; (setq custom-file "~/.spacemacs.d/custom.el")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq powerline-default-separator nil)
  (setq jiralib-url "https://jira.lpr.jet.msk.su")
  (setq org-jira-working-dir "~/jet/org")
  (evil-set-initial-state 'term-mode 'emacs)
  ;; (when (file-exists-p custom-file)
  ;;  (load-file custom-file))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ns-command-modifier (quote control))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#000000" :underline nil))))
 '(term-color-blue ((t (:background "dodger blue" :foreground "dodger blue"))))
 '(term-color-cyan ((t (:background "cyan" :foreground "cyan"))))
 '(term-color-green ((t (:background "spring green" :foreground "spring green"))))
 '(term-color-magenta ((t (:background "magenta" :foreground "magenta"))))
 '(term-color-red ((t (:background "orange red" :foreground "orange red"))))
 '(term-color-yellow ((t (:background "yellow" :foreground "yellow")))))
