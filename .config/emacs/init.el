;; Common vars
(setq user-full-name "Георгий Кибардин"
      ispell-dictionary "english"
      ns-command-modifier 'control
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      calendar-week-start-day 1
      calendar-date-style "european"
      calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
				 "Июнь" "Июль" "Август" "Сентябрь"
				 "Октябрь" "Ноябрь" "Декабрь"])


;; Appearence
(setq inhibit-startup-screen t)
(menu-bar-mode -99)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell 1)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Make wider margins for documentation modes
(defun my-set-comfortable-margins ()
  (setq left-margin-width 1
	right-margin-width 1)
  (when-let ((w (get-buffer-window (current-buffer))))
    (set-window-buffer w (current-buffer))))

(with-eval-after-load 'info
  (add-hook 'Info-mode-hook 'my-set-comfortable-margins))

(with-eval-after-load 'help
  (add-hook 'help-mode-hook 'my-set-comfortable-margins))

(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook 'my-set-comfortable-margins))


(defun my-convert-window-to-frame ()
  (interactive)
  (let ((to-delete (selected-window))
        (has-other-windows (cdr (window-list))))
    (make-frame)
    (when has-other-windows
      (delete-window to-delete))))

(global-set-key (kbd "C-x 5 6") 'my-convert-window-to-frame)


;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Install packages automatically
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(savehist-mode)

(recentf-mode)

(pixel-scroll-precision-mode)


;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


(setq space-map (make-sparse-keymap))
(set-keymap-parent space-map ctl-x-map)


(use-package ob-restclient)


;; org
(use-package org
  :defer t
  :after ob-restclient
  :config (progn
            (setq org-directory "~/org"
                  org-default-notes-file (concat org-directory "/notes.org")
                  org-agenda-files (list (concat org-directory "/my.org")
                                         (concat org-directory "/projects"))
                  org-refile-targets '((org-agenda-files . (:level . 1)))
                  org-capture-templates '(("t" "Todo" entry (file "")
                                           "* TODO %?\n  %i\n  %a")
                                          ("c" "Note" entry (file "")
                                           "* %?\n  %i\n  %a")
                                          ("e" "External note (from clipboard)" entry (file "")
                                           "* %?\n  %x"))
                  ;; turn off auto-identation in src code blocks
                  ;; while this sounds like a good feature it is actually broken
                  org-confirm-babel-evaluate nil
                  org-src-tab-acts-natively nil
                  org-babel-load-languages '((python . t)
                                             (sql . t)
                                             (clojure . t)
                                             (restclient . t)
                                             (shell . t)
                                             (emacs-lisp . t)))
            (defun my-org-capture-external-note ()
              (interactive)
              (let ((org-capture-p (lambda (w)
                                     (string-prefix-p "CAPTURE-"
                                                      (buffer-name (window-buffer w))))))
                (org-capture nil "e")
                (let ((capture-win (seq-find org-capture-p (window-list))))
                  (delete-other-windows capture-win)
                  (add-hook 'kill-buffer-hook 'delete-frame 0 t))))
            ;; Shows link target in the status line when hovered
            (help-at-pt-set-timer)))


(use-package evil-org
  :after (org evil)
  :hook (org-mode . evil-org-mode)
  :config (progn
            (evil-org-set-key-theme '(navigation
                                      insert
                                      textobjects
                                      additional
                                      calendar
                                      todo
                                      heading
                                      return))
            (require 'evil-org-agenda)
            (evil-org-agenda-set-keys)))


(use-package org-superstar
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1))))


;; Evil
(use-package evil
  :init (progn
          (setq-default evil-symbol-word-search t)
          (setq evil-flash-delay 8
                evil-want-integration t
                evil-want-keybinding nil
                evil-move-beyond-eol t
                evil-auto-balance-windows t
                evil-undo-system 'undo-redo))
  :bind (:map space-map
              ("SPC" . #'execute-extended-command)
              (":" . #'eval-expression)
              ("f" . #'find-file))
  :config (progn
            (evil-mode 1)
            (evil-global-set-key 'motion (kbd "SPC") space-map)

            (defun my-letter-p (ch)
              (and (>= ch 97) (<= ch 122)))

            ;; Builds a new keymap from major mode map
            (defun my-build-comma-map (source-map)
              (let ((result (make-sparse-keymap)))
                (map-keymap (lambda (k v)
                              (let ((mods (event-modifiers k))
                                    (ch (event-basic-type k)))
                                (if (equal mods '(control))
                                    (when (my-letter-p ch)
                                      (keymap-set result (single-key-description ch) v))
                                  (when (not (my-letter-p ch))
                                    (keymap-set result (single-key-description k) v)))))
                            source-map)
                result))

            ;; Automatically generate new normal mode bindings for major modes
            ;; so that "C-c C-e" is augmented with ", e"
            ;; and "C-c #" makes ", #"
            (defun my-setup-comma-map ()
              (let* ((mm-map (current-local-map))
                     (c-c-map (keymap-lookup mm-map "C-c")))
                (when (and c-c-map) ;; TODO check comma map is already installed
                  (let ((comma-map (my-build-comma-map c-c-map)))
                    (when (cdr comma-map)  ;; keymap is not empty
                      (evil-define-key 'motion mm-map "," comma-map)
                      (message "Made a new map for %s !!!!" major-mode)
                      )))))

            (add-hook 'after-change-major-mode-hook #'my-setup-comma-map)))


(use-package evil-collection
  :after evil
  :config (evil-collection-init))


(use-package which-key
  :config (which-key-mode))


(use-package map)

(use-package consult
  :after map
  :bind (("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-extra-find)
         :map space-map
         ("j" . my-jump))
  :config (progn
            (defun my-jump ()
              (interactive)
              (consult-buffer (list  consult--source-hidden-buffer
                                     consult--source-buffer
                                     (map-merge 'plist
                                                consult--source-recent-file
                                                '(:narrow ?r :hidden t))
                                     consult-project-extra--source-file
                                     (map-merge 'plist
                                                consult-project-extra--source-project
                                                '(:hidden t)))))
            ))


(use-package consult-ag
  :after consult
  :bind (:map space-map
              ("/" . consult-ag)))


(use-package consult-eglot
  :after consutl)


(use-package consult-project-extra
  :after consult)


(use-package embark
  :bind (("C-c a" . embark-act)
         :map space-map
         ("." . embark-act)
         :map embark-buffer-map
         ("f" . switch-to-buffer-other-frame)))


(use-package embark-consult
  :after (consult embark))


(use-package vertico
  :config (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . #'vertico-next)
              ("C-k" . #'vertico-previous)))


(use-package marginalia
  :config (marginalia-mode))


(use-package corfu
  :init (setq corfu-auto t)
  :config (global-corfu-mode))


(use-package orderless
  :config (setq completion-styles '(orderless)))


(use-package magit)


(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))


(use-package yaml-mode
  :ensure t)


(use-package json-mode
  :ensure t)


(use-package cider)


(use-package smartparens
  :hook (prog-mode text-mode)
  :config (progn
            (show-smartparens-global-mode t)
            (smartparens-global-mode)
            (require 'smartparens-config)))


(use-package evil-cleverparens
  :after evil
  :hook (emacs-lisp-mode clojure-mode))

;; dired
(setq image-dired-thumb-size 256)
(setq dired-listing-switches "-l")
(put 'dired-find-alternate-file 'disabled nil)


;; Open new frames by default so that window manager
;; can be used for managing windows
;; !!!!!!!!! incompatible with embark
;; (use-package frames-only-mode
;;   :config (frames-only-mode))


;; Themes
(use-package doom-themes
  :config (load-theme 'doom-material t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 65 :family "DejaVu Sans Mono"))))
 '(fixed-pitch-serif ((t (:height 1.1 :family "Linux Libertine Mono O"))))
 '(variable-pitch ((t (:height 1.1 :family "DejaVu Sans")))))
