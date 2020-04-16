
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-configuration-layers
   '(
      python
      clojure
      (shell :variables shell-default-shell 'eshell)
      yaml
      html
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
      restclient
      (mu4e :variables mu4e-enable-mode-line t)
      rust
      pdf-tools
      csharp
      ipython-notebook
      docker
      go
     )
   dotspacemacs-additional-packages '(russian-holidays
                                      vlf
                                      pyenv-mode-auto)))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-verbose-loading t
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark
                         wombat)
   dotspacemacs-default-font
    (cond ((eq system-type 'darwin)    '("Monaco"
                                         :size 10
                                         :weight normal
                                         :width normal
                                         :powerline-scale 1.1))
          ((eq system-type 'gnu/linux) '("Monospace"
                                         :size 19
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
  (setq ispell-dictionary "english"
        calendar-week-start-day 1
        calendar-date-style "european"
        calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
        calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                   "Июнь" "Июль" "Август" "Сентябрь"
                                   "Октябрь" "Ноябрь" "Декабрь"])
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
  (setq user-full-name "Георгий Кибардин")
  (setq auth-sources '(default))
  (evil-define-key 'normal cider-repl-mode-map
    (kbd "C-j") 'cider-repl-next-input
    (kbd "C-k") 'cider-repl-previous-input
    (kbd "RET") 'cider-repl-return)
  (evil-define-key 'insert cider-repl-mode-map
    (kbd "C-j") 'cider-repl-next-input
    (kbd "C-k") 'cider-repl-previous-input)

  (evil-define-key 'normal eshell-mode-map
    (kbd "C-k") 'eshell-previous-input
    (kbd "C-j") 'eshell-next-input
    (kbd "RET") 'eshell-send-input)

  (with-eval-after-load 'org
    (setq org-mobile-directory "~/Nextcloud/org-mobile/")
    (setq org-mobile-files (concat org-directory "/mobile/"))
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (setq org-agenda-files (list org-directory))
    (setq org-agenda-include-diary t))

  (load-file "~/.spacemacs.d/mu4e.el")
  ;; (when (file-exists-p custom-file)
  ;;  (load-file custom-file))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-resolution 216)
 '(ns-command-modifier (quote control))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
