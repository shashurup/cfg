(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function (quote split-window-horizontally))
 '(evil-flash-delay 8)
 '(evil-symbol-word-search t)
 '(inhibit-startup-screen t)
 '(ns-command-modifier (quote control))
 '(package-selected-packages
   (quote
    (doom-themes dakrone-theme humanoid-themes kaolin-themes oceanic-theme zeno-theme pyenv-mode-auto elpy ag org-bullets winum which-key ivy-rich counsel-projectile projectile evil-collection ivy evil magit evil-magit)))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Common vars
(setq ispell-dictionary "english"
      calendar-week-start-day 1
      calendar-date-style "european"
      calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
				 "Июнь" "Июль" "Август" "Сентябрь"
				 "Октябрь" "Ноябрь" "Декабрь"])

;; Appearence
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -99)
  (set-frame-font "Monospace-6" nil t))

(when (eq system-type 'darwin)
  (set-frame-font "Monaco-10" nil t)
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(scroll-bar-mode -1)

;; Package initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; Do not initialize packages again after init
(setq package-enable-at-startup nil)

;; theme
(load-theme 'dakrone t)


;; core
;; Input method toggling by caps mapped to F19
(global-set-key (kbd "<f19>") 'toggle-input-method)
;; Unmap "SPC" so that it can be used by evil motion map
(define-key special-mode-map (kbd "SPC") nil)
(define-key Info-mode-map (kbd "SPC") nil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(evil-global-set-key 'motion (kbd "SPC") nil)
(evil-set-leader 'motion (kbd "SPC"))
(evil-set-leader 'motion (kbd ",") t)
;(define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
;(define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)

(setq my-root-map (make-sparse-keymap))

; Basic keybinding are inspired by spacemacs
(define-key my-root-map " " 'execute-extended-command)
(define-key my-root-map "'" 'eshell)
(define-key my-root-map "!" 'shell-command)
(define-key my-root-map "?" 'describe-bindings)

(define-key my-root-map "bb" 'switch-to-buffer)
(define-key my-root-map "bd" 'kill-current-buffer)
(define-key my-root-map "bk" 'kill-current-buffer)
(define-key my-root-map "bn" 'next-buffer)
(define-key my-root-map "bp" 'previous-buffer)
(define-key my-root-map "br" 'revert-buffer)

(define-key my-root-map "ff" 'find-file)
(define-key my-root-map "fr" 'recentf-open-files)
(define-key my-root-map "fs" 'save-buffer)

(define-key my-root-map "hdf" 'describe-function)
(define-key my-root-map "hdk" 'describe-key)
(define-key my-root-map "hdm" 'describe-mode)
(define-key my-root-map "hdv" 'describe-variable)
(define-key my-root-map "hi" 'info)
(define-key my-root-map "hm" 'man)
(define-key my-root-map "hr" 'info-emacs-manual)

(put 'narrow-to-region 'disabled nil)
(define-key my-root-map "nd" 'narrow-to-defun)
(define-key my-root-map "nr" 'narrow-to-region)
(define-key my-root-map "nw" 'widen)

(define-key my-root-map "qq" 'save-buffers-kill-terminal)
(define-key my-root-map "qQ" 'kill-emacs)
(define-key my-root-map "qz" 'delete-frame)

(define-key my-root-map "sb" 'occur)
(define-key my-root-map "sd" 'find-dired)
(define-key my-root-map "sg" 'find-grep)
(define-key my-root-map "ss" 'find-grep)

(define-key my-root-map "w=" 'balance-windows)
(define-key my-root-map "wd" 'delete-window)
(define-key my-root-map "wD" 'kill-buffer-and-window)
(define-key my-root-map "wF" 'make-frame)
(define-key my-root-map "wh" 'evil-window-left)
(define-key my-root-map "wj" 'evil-window-down)
(define-key my-root-map "wk" 'evil-window-up)
(define-key my-root-map "wl" 'evil-window-right)
(define-key my-root-map "wH" 'evil-window-move-far-left)
(define-key my-root-map "wJ" 'evil-window-move-very-bottom)
(define-key my-root-map "wK" 'evil-window-move-very-top)
(define-key my-root-map "wL" 'evil-window-move-far-right)
(define-key my-root-map "wm" 'delete-other-windows)
(define-key my-root-map "ws" 'split-window-vertically)
(define-key my-root-map "wv" 'split-window-horizontally)
(define-key my-root-map "ww" 'other-window)

(evil-define-key 'motion 'global (kbd "<leader>") my-root-map)

;; calendar
(evil-collection-init 'calendar)

;; org
(add-hook 'org-mode-hook #'org-bullets-mode)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sql . t)
   (clojure . t)
   (shell . t)
   (emacs-lisp . nil)))
(define-key org-mode-map (kbd "<localleader> ,") 'org-ctrl-c-ctrl-c)
(define-key org-mode-map (kbd "<localleader> /") 'org-sparse-tree)

;; Package list
(define-key package-menu-mode-map " " my-root-map)
(define-key package-menu-mode-map "j" 'next-line)
(define-key package-menu-mode-map "k" 'previous-line)
(define-key package-menu-mode-map "/" 'evil-search-forward)
(define-key package-menu-mode-map "?" 'evil-search-backward)
(define-key package-menu-mode-map "n" 'evil-search-next)
(define-key package-menu-mode-map "N" 'evil-search-previous)
(define-key package-menu-mode-map (kbd "C-f") 'evil-scroll-page-down)
(define-key package-menu-mode-map (kbd "C-b") 'evil-scroll-page-up)

;; Dired
(evil-set-initial-state 'dired-mode 'emacs)
(with-eval-after-load 'dired
  (define-key dired-mode-map " " my-root-map)
  (define-key dired-mode-map "j" 'next-line)
  (define-key dired-mode-map "k" 'previous-line)
  (define-key dired-mode-map "J" 'dired-goto-file)
  (define-key dired-mode-map "K" 'dired-do-kill-lines)
  (define-key dired-mode-map "/" 'evil-search-forward)
  (define-key dired-mode-map "?" 'evil-search-backward)
  (define-key dired-mode-map "n" 'evil-search-next)
  (define-key dired-mode-map "N" 'evil-search-previous)
  (define-key dired-mode-map (kbd "C-f") 'evil-scroll-page-down)
  (define-key dired-mode-map (kbd "C-b") 'evil-scroll-page-up))

;; Custom
(with-eval-after-load 'cus-edit
  (define-key custom-mode-map " " my-root-map)
  (define-key custom-mode-map "j" 'widget-forward)
  (define-key custom-mode-map "k" 'widget-backward))

;; Emacs lisp
(define-key emacs-lisp-mode-map (kbd "<localleader> f") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "<localleader> r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "<localleader> b") 'eval-buffer)


;; ag
(define-key my-root-map "sf" 'ag-dired)
(define-key my-root-map "sg" 'ag)
(define-key my-root-map "ss" 'ag)


;; counsel ivy
;; Make ivy use C-j and C-k for up and down
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(setq ivy-use-virtual-buffers t)
(ivy-mode 1)
(ivy-rich-mode 1)
;; Unmap theese keys to allow global keys to be used for up/down
(define-key ivy-minibuffer-map (kbd "C-j") nil)
(define-key ivy-switch-buffer-map (kbd "C-k") nil)
(define-key ivy-switch-buffer-map (kbd "C-w") 'ivy-switch-buffer-kill)

(defun counsel-ag-with-thing-at-point ()
  (interactive)
  (counsel-ag (ivy-thing-at-point)))

;(defun counsel-ag-ask-dir (dir)
;  (interactive)
;  (counsel-ag (ivy-thing-at-point) dir))

(define-key my-root-map " " 'counsel-M-x)
(define-key my-root-map "?" 'counsel-descbinds)
(define-key my-root-map "bb" 'ivy-switch-buffer)
(define-key my-root-map "ff" 'counsel-find-file)
(define-key my-root-map "fr" 'counsel-recentf)
(define-key my-root-map "hdf" 'counsel-describe-function)
(define-key my-root-map "hdv" 'counsel-describe-variable)
(define-key my-root-map "sb" 'swiper-thing-at-point)
(define-key my-root-map "sd" 'counsel-ag-with-thing-at-point)


;; projectile
(define-key my-root-map "p'" 'projectile-run-eshell)
(define-key my-root-map "p!" 'projectile-run-shell-command-in-root)
(define-key my-root-map "pd" 'projectile-dired)
(with-eval-after-load 'projectile
  (projectile-mode +1))


;; counsel-projectile
(setq counsel-projectile-ag-initial-input '(ivy-thing-at-point))
(define-key my-root-map "/" 'counsel-projectile-ag)
(define-key my-root-map "pf" 'counsel-projectile-find-file)
(define-key my-root-map "pp" 'counsel-projectile-switch-project)
(define-key my-root-map "ps" 'counsel-projectile-ag)
(define-key my-root-map "sp" 'counsel-projectile-ag)


;; winum
(define-key my-root-map "1" 'winum-select-window-1)
(define-key my-root-map "2" 'winum-select-window-2)
(define-key my-root-map "3" 'winum-select-window-3)
(define-key my-root-map "4" 'winum-select-window-4)
(define-key my-root-map "5" 'winum-select-window-5)
(define-key my-root-map "6" 'winum-select-window-6)
(define-key my-root-map "7" 'winum-select-window-7)
(define-key my-root-map "8" 'winum-select-window-8)
(define-key my-root-map "9" 'winum-select-window-9)
(with-eval-after-load 'winum
  (winum-mode))


;; which key
(define-key my-root-map "hk" 'which-key-show-top-level)
(which-key-mode)


;; magit
(define-key my-root-map "gs" 'magit-status)
(define-key my-root-map "gb" 'magit-blame-addition)
(define-key my-root-map "gm" 'magit-dispatch)
(define-key my-root-map "gl" 'magit-log-buffer-file)
(with-eval-after-load 'magit
  (require 'evil-magit))


;; python
;; TODO pyenv shim path for child processes
;; TODO autoload on visiting python files
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt"
      exec-path (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims")))
(with-eval-after-load 'elpy
  (define-key elpy-mode-map (kbd "<localleader> c") 'elpy-config)
  (define-key elpy-mode-map (kbd "<localleader> d") 'elpy-doc)
  (define-key elpy-mode-map (kbd "<localleader> g g") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "<localleader> g G") 'elpy-goto-definition-other-window)
  (define-key elpy-mode-map (kbd "<localleader> g a") 'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "<localleader> s s") 'elpy-shell-switch-to-shell)
  (define-key elpy-mode-map (kbd "<localleader> s f") 'elpy-shell-send-defun)
  (define-key elpy-mode-map (kbd "<localleader> s F") 'elpy-shell-send-defun-and-go)
  (define-key elpy-mode-map (kbd "<localleader> s b") 'elpy-shell-send-buffer)
  (define-key elpy-mode-map (kbd "<localleader> s B") 'elpy-shell-send-buffer-and-go)
  (define-key elpy-mode-map (kbd "<localleader> s r") 'elpy-shell-send-region-or-buffer)
  (define-key elpy-mode-map (kbd "<localleader> s R") 'elpy-shell-send-region-or-buffer-and-go)
  (define-key elpy-mode-map (kbd "<localleader> S n") 'elpy-flymake-next-error)
  (define-key elpy-mode-map (kbd "<localleader> S p") 'elpy-flymake-previous-error)
  (elpy-enable))

;; mu4e
(load "~/.emacs.d/mu4e.el")

;; TODO layouts (persp mode)
;; TODO org mode keybindings
;; TODO clojure mode
;; TODO image mode
;; TODO pdf mode
;; TODO docview mode
;; TODO sql repl
;; TODO setup eshell
;; TODO check out hydra (ivy-hydra)
;; TODO refine man keys
;; TODO folding
