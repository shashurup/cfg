;; Common vars
(setq user-full-name "First Last"
      ispell-dictionary "english"
      ns-command-modifier 'control
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

(setq inhibit-startup-screen t)
(scroll-bar-mode -1)

;; Package initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; Do not initialize packages again after init
(setq package-enable-at-startup nil)

;; theme
(load-theme 'doom-henna t)


;; core
;; Input method toggling by caps mapped to F19
(global-set-key (kbd "<f19>") 'toggle-input-method)
;; Unmap "SPC" so that it can be used by evil motion map
(define-key special-mode-map (kbd "SPC") nil)
(define-key Info-mode-map (kbd "SPC") nil)
(setq-default evil-symbol-word-search t)
(setq evil-flash-delay 8)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(evil-global-set-key 'motion (kbd "SPC") nil)
(evil-set-leader 'motion (kbd "SPC"))
(evil-set-leader 'motion (kbd ",") t)
;(define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
;(define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)

(setq lesser-evil-leader-map (make-sparse-keymap))

; Basic keybinding are inspired by spacemacs
(define-key lesser-evil-leader-map " " 'execute-extended-command)
(define-key lesser-evil-leader-map (kbd "TAB") 'evil-switch-to-windows-last-buffer)
(define-key lesser-evil-leader-map ":" 'eval-expression)
(define-key lesser-evil-leader-map "'" 'eshell)
(define-key lesser-evil-leader-map "!" 'shell-command)
(define-key lesser-evil-leader-map "u" 'universal-argument)

(define-key lesser-evil-leader-map "aC" 'calendar)
(define-key lesser-evil-leader-map "ac" 'calc-dispatch)
(define-key lesser-evil-leader-map "ap" 'list-processes)
(define-key lesser-evil-leader-map "aP" 'proced)

(define-key lesser-evil-leader-map "bb" 'switch-to-buffer)
(define-key lesser-evil-leader-map "bd" 'kill-current-buffer)
(define-key lesser-evil-leader-map "bD" 'kill-buffer-and-window)
(define-key lesser-evil-leader-map "bk" 'kill-current-buffer)
(define-key lesser-evil-leader-map "bn" 'next-buffer)
(define-key lesser-evil-leader-map "bp" 'previous-buffer)
(define-key lesser-evil-leader-map "br" 'revert-buffer)
(define-key lesser-evil-leader-map "bs" (lambda ()
			       (interactive)
			       (switch-to-buffer "*scratch*")))

(define-key lesser-evil-leader-map "ff" 'find-file)
(define-key lesser-evil-leader-map "fr" 'recentf-open-files)
(define-key lesser-evil-leader-map "fs" 'save-buffer)

(define-key lesser-evil-leader-map "hdf" 'describe-function)
(define-key lesser-evil-leader-map "hdk" 'describe-key)
(define-key lesser-evil-leader-map "hdm" 'describe-mode)
(define-key lesser-evil-leader-map "hdv" 'describe-variable)
(define-key lesser-evil-leader-map "hdp" 'describe-package)
(define-key lesser-evil-leader-map "hi" 'info)
(define-key lesser-evil-leader-map "hm" 'man)
(define-key lesser-evil-leader-map "hr" 'info-emacs-manual)

(put 'narrow-to-region 'disabled nil)
(define-key lesser-evil-leader-map "nd" 'narrow-to-defun)
(define-key lesser-evil-leader-map "nr" 'narrow-to-region)
(define-key lesser-evil-leader-map "nw" 'widen)

(define-key lesser-evil-leader-map "qq" 'save-buffers-kill-terminal)
(define-key lesser-evil-leader-map "qQ" 'kill-emacs)
(define-key lesser-evil-leader-map "qz" 'delete-frame)

(define-key lesser-evil-leader-map "sb" 'occur)
(define-key lesser-evil-leader-map "sd" 'find-dired)
(define-key lesser-evil-leader-map "sg" 'find-grep)
(define-key lesser-evil-leader-map "ss" 'find-grep)

(define-key lesser-evil-leader-map "tf" 'auto-fill-mode)
(define-key lesser-evil-leader-map "tl" 'toggle-truncate-lines)
(define-key lesser-evil-leader-map "tn" 'linum-mode)

(define-key lesser-evil-leader-map "w=" 'balance-windows)
(define-key lesser-evil-leader-map "wd" 'delete-window)
(define-key lesser-evil-leader-map "wD" 'kill-buffer-and-window)
(define-key lesser-evil-leader-map "wF" 'make-frame)
(define-key lesser-evil-leader-map "wh" 'evil-window-left)
(define-key lesser-evil-leader-map "wj" 'evil-window-down)
(define-key lesser-evil-leader-map "wk" 'evil-window-up)
(define-key lesser-evil-leader-map "wl" 'evil-window-right)
(define-key lesser-evil-leader-map "wH" 'evil-window-move-far-left)
(define-key lesser-evil-leader-map "wJ" 'evil-window-move-very-bottom)
(define-key lesser-evil-leader-map "wK" 'evil-window-move-very-top)
(define-key lesser-evil-leader-map "wL" 'evil-window-move-far-right)
(define-key lesser-evil-leader-map "wm" 'maximize-window)
(define-key lesser-evil-leader-map "ws" 'split-window-vertically)
(define-key lesser-evil-leader-map "wv" 'split-window-horizontally)
(define-key lesser-evil-leader-map "ww" 'other-window)

(defhydra window-resize (lesser-evil-leader-map "wr")
  "Window resizing"
  ("j" shrink-window "shrink window")
  ("k" enlarge-window "enlarge window")
  ("h" shrink-window-horizontally "shrink window horizontally")
  ("}" enlarge-window-horizontally "enlarge window horizontally"))

(evil-define-key 'motion 'global (kbd "<leader>") lesser-evil-leader-map)

(setq lesser-evil-keys-desc nil)
(push '(nil
	"<leader>a" "applications"
	"<leader>b" "buffer"
	"<leader>bs" "scratch"
	"<leader>f" "file"
	"<leader>g" "git"
	"<leader>h" "help"
	"<leader>hd" "describe"
	"<leader>n" "narrow"
	"<leader>l" "layout"
	"<leader>p" "project"
	"<leader>q" "quit"
	"<leader>s" "search"
	"<leader>s" "search"
	"<leader>w" "window"
	"<leader>wr" "resize"
	) lesser-evil-keys-desc)

;; undo for windows
(winner-mode)
(define-key lesser-evil-leader-map "wu" 'winner-undo)

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
(evil-define-key 'normal org-mode-map "t" 'org-todo)
(evil-define-key 'normal org-mode-map "<" 'org-metaleft)
(evil-define-key 'normal org-mode-map ">" 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "<localleader> ,") 'org-ctrl-c-ctrl-c)
(evil-define-key 'normal org-mode-map (kbd "<localleader> /") 'org-sparse-tree)
(evil-define-key 'normal org-mode-map (kbd "<localleader> .") 'org-time-stamp)
(evil-define-key 'normal org-mode-map (kbd "<localleader> a") 'org-agenda)
(evil-define-key 'normal org-mode-map (kbd "<localleader> A") 'org-archive-subtree)
(evil-define-key 'normal org-mode-map (kbd "<localleader> d") 'org-deadline)
(evil-define-key 'normal org-mode-map (kbd "<localleader> s") 'org-schedule)
(evil-define-key 'normal org-mode-map (kbd "<localleader> n") 'org-narrow-to-subtree)
(evil-define-key 'normal org-mode-map (kbd "<localleader> N") 'widen)
(evil-define-key 'normal org-mode-map (kbd "<localleader> p") 'org-set-property)

(defun my-bind-basic-motion (map)
  (define-key map "j" 'next-line)
  (define-key map "k" 'previous-line)
  (define-key map "/" 'evil-search-forward)
  (define-key map "?" 'evil-search-backward)
  (define-key map "n" 'evil-search-next)
  (define-key map "N" 'evil-search-previous)
  (define-key map (kbd "C-f") 'evil-scroll-page-down)
  (define-key map (kbd "C-b") 'evil-scroll-page-up)
  (if (not (keymapp (lookup-key map "g")))
      (define-key map "g" nil))
  (define-key map "gg" 'beginning-of-buffer)
  (define-key map "G" 'end-of-buffer))

;; Emacs child processes
(evil-set-initial-state 'process-menu-mode 'motion)

;; Proced
(with-eval-after-load 'proced
  (define-key proced-mode-map " " lesser-evil-leader-map)
  (my-bind-basic-motion proced-mode-map)
  (define-key proced-mode-map "gr" 'revert-buffer))

;; Package list
(define-key package-menu-mode-map " " lesser-evil-leader-map)
(my-bind-basic-motion package-menu-mode-map)
(define-key package-menu-mode-map "gr" 'revert-buffer)

;; Dired
(evil-set-initial-state 'dired-mode 'emacs)
(with-eval-after-load 'dired
  (define-key dired-mode-map " " lesser-evil-leader-map)
  (my-bind-basic-motion dired-mode-map)
  (define-key dired-mode-map "J" 'dired-goto-file)
  (define-key dired-mode-map "K" 'dired-do-kill-lines)
  (define-key dired-mode-map "I" (lambda () "Image dired"
				   (interactive)
				   (image-dired ".")))
  (define-key dired-mode-map "gr" 'revert-buffer)
  (define-key dired-mode-map "gf" 'dired-goto-file)
  (define-key dired-mode-map "gG" 'dired-do-chgrp))
(setq image-dired-thumb-size 256)
(evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
(evil-set-initial-state 'image-dired-display-image-mode 'emacs)
(with-eval-after-load 'image-dired
    (define-key image-dired-display-image-mode-map " " lesser-evil-leader-map)
    (define-key image-dired-thumbnail-mode-map " " lesser-evil-leader-map)
    (define-key image-dired-thumbnail-mode-map "j" 'image-dired-next-line-and-display)
    (define-key image-dired-thumbnail-mode-map "k" 'image-dired-previous-line-and-display)
    (define-key image-dired-thumbnail-mode-map "h" 'image-dired-display-previous-thumbnail-original)
    (define-key image-dired-thumbnail-mode-map "l" 'image-dired-display-next-thumbnail-original)
    (define-key image-dired-thumbnail-mode-map (kbd "C-f") 'scroll-up)
    (define-key image-dired-thumbnail-mode-map (kbd "C-b") 'scroll-down)
    (define-key image-dired-thumbnail-mode-map "G" 'end-of-buffer)
    (define-key image-dired-thumbnail-mode-map "gd" 'image-dired-line-up-dynamic)
    (define-key image-dired-thumbnail-mode-map "gg" 'beginning-of-buffer)
  )
(push '(dired-mode
	"I" "image-dired")
      lesser-evil-keys-desc)

;; Custom
(with-eval-after-load 'cus-edit
  (define-key custom-mode-map " " lesser-evil-leader-map)
  (define-key custom-mode-map "j" 'widget-forward)
  (define-key custom-mode-map "k" 'widget-backward))

;; Emacs lisp
;(evil-define-key 'normal emacs-lisp-mode-map (kbd "<localleader> ,") 'eval-sexp)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "<localleader> f") 'eval-defun)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "<localleader> r") 'eval-region)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "<localleader> b") 'eval-buffer)

;; man
(evil-define-key 'motion Man-mode-map (kbd "TAB") 'forward-button)
(evil-define-key 'motion Man-mode-map "gs" 'Man-goto-section)
(evil-define-key 'motion Man-mode-map "]]" 'Man-next-section)
(evil-define-key 'motion Man-mode-map "][" 'Man-next-section)
(evil-define-key 'motion Man-mode-map "[[" 'Man-previous-section)
(evil-define-key 'motion Man-mode-map "[]" 'Man-previous-section)

;; Help
(evil-define-key 'motion help-mode-map (kbd "TAB") 'forward-button)


;; docview
(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map " " lesser-evil-leader-map)
  (define-key doc-view-mode-map "j" 'doc-view-next-page)
  (define-key doc-view-mode-map "k" 'doc-view-previous-page)
  (define-key doc-view-mode-map "g" 'nil)
  (define-key doc-view-mode-map "gr" 'doc-view-revert-buffer)
  (define-key doc-view-mode-map "gg" 'doc-view-first-page)
  (define-key doc-view-mode-map "G" 'doc-view-last-page)
  (define-key doc-view-mode-map "/" 'doc-view-search)
  (define-key doc-view-mode-map "n" 'doc-view-search-next-match)
  (define-key doc-view-mode-map "N" 'doc-view-search-previous-match))


;; image
(evil-set-initial-state 'image-mode 'emacs)
(with-eval-after-load 'image-mode
  (define-key image-mode-map " " lesser-evil-leader-map)
  (define-key image-mode-map "j" 'image-scroll-up)
  (define-key image-mode-map "k" 'image-scroll-down)
  (define-key image-mode-map "h" 'image-scroll-rigth)
  (define-key image-mode-map "l" 'image-scroll-left))


;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; It is not that simple :(
;(defun evilify-ediff-a-bit ()
;  (define-key ediff-mode-map "gj" 'ediff-jump-to-difference)
;  (define-key ediff-mode-map "j" 'ediff-next-difference)
;  (define-key ediff-mode-map "k" 'ediff-previous-difference)
;  )
;(add-hook 'ediff-keymap-setup-hook 'evilify-ediff-a-bit)
 

;; ag
(define-key lesser-evil-leader-map "sf" 'ag-dired)
(define-key lesser-evil-leader-map "sg" 'ag)
(define-key lesser-evil-leader-map "ss" 'ag)
(push '(nil
	"<leader>sf" "file"
	"<leader>sg" "a directory"
	"<leader>ss" "a directory"
	)
      lesser-evil-keys-desc)


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

(define-key lesser-evil-leader-map " " 'counsel-M-x)
(define-key lesser-evil-leader-map "bb" 'ivy-switch-buffer)
(define-key lesser-evil-leader-map "ff" 'counsel-find-file)
(define-key lesser-evil-leader-map "fr" 'counsel-recentf)
(define-key lesser-evil-leader-map "hdf" 'counsel-describe-function)
(define-key lesser-evil-leader-map "hdv" 'counsel-describe-variable)
(define-key lesser-evil-leader-map "hdk" 'counsel-descbinds)
(define-key lesser-evil-leader-map "sb" 'swiper-thing-at-point)
(define-key lesser-evil-leader-map "sd" 'counsel-ag-with-thing-at-point)
(push '(nil
	"<leader>sb" "current buffer"
	"<leader>sd" "current directory"
	)
      lesser-evil-keys-desc)

;; projectile
(define-key lesser-evil-leader-map "p'" 'projectile-run-eshell)
(define-key lesser-evil-leader-map "p!" 'projectile-run-shell-command-in-root)
(define-key lesser-evil-leader-map "pd" 'projectile-dired)
(with-eval-after-load 'projectile
  (projectile-mode +1))


;; counsel-projectile
(setq counsel-projectile-ag-initial-input '(ivy-thing-at-point))
(define-key lesser-evil-leader-map "/" 'counsel-projectile-ag)
(define-key lesser-evil-leader-map "pf" 'counsel-projectile-find-file)
(define-key lesser-evil-leader-map "pp" 'counsel-projectile-switch-project)
(define-key lesser-evil-leader-map "ps" 'counsel-projectile-ag)
(define-key lesser-evil-leader-map "sp" 'counsel-projectile-ag)


;; winum
(define-key lesser-evil-leader-map "1" 'winum-select-window-1)
(define-key lesser-evil-leader-map "2" 'winum-select-window-2)
(define-key lesser-evil-leader-map "3" 'winum-select-window-3)
(define-key lesser-evil-leader-map "4" 'winum-select-window-4)
(define-key lesser-evil-leader-map "5" 'winum-select-window-5)
(define-key lesser-evil-leader-map "6" 'winum-select-window-6)
(define-key lesser-evil-leader-map "7" 'winum-select-window-7)
(define-key lesser-evil-leader-map "8" 'winum-select-window-8)
(define-key lesser-evil-leader-map "9" 'winum-select-window-9)
(with-eval-after-load 'winum
  (winum-mode))


;; smooth scrolling
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)


;; which key
(define-key lesser-evil-leader-map (kbd "h SPC") 'which-key-show-top-level)
(which-key-mode)

;; magit
(define-key lesser-evil-leader-map "gs" 'magit-status)
(define-key lesser-evil-leader-map "gb" 'magit-blame-addition)
(define-key lesser-evil-leader-map "gm" 'magit-dispatch)
(define-key lesser-evil-leader-map "gl" 'magit-log-buffer-file)
(evil-define-key 'normal 'with-editor-mode-map (kbd "<localleader> ,") 'with-editor-finish)
(evil-define-key 'normal 'with-editor-mode-map (kbd "<localleader> k") 'with-editor-cancel)
(evil-define-key 'normal 'magit-diff-mode-map " " lesser-evil-leader-map)
(evil-initial-state 'magit-diff-mode 'motion)
(with-eval-after-load 'magit
  (require 'evil-magit))


;; company
(global-company-mode)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)


;; layouts with perspective
;; Switching layou function cannot be autoloaded
;; and we cannot load perspective lazily
;; so we use this hack to at least turn on
;; perspective mode with familiar keybinding
(define-key lesser-evil-leader-map "l" 'persp-mode)
(with-eval-after-load 'perspective

  (if (featurep 'ivy-rich)
      (let ((cmd 'persp-ivy-switch-buffer)
	    (props (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)))
	(ivy-set-display-transformer cmd (ivy-rich-build-transformer cmd props))))

  (if (featurep 'ivy)
      (define-key lesser-evil-leader-map "bb" 'persp-ivy-switch-buffer))

  (add-to-list 'mu4e-view-actions
	       '("View in browser" . mu4e-action-view-in-browser) t)

  (define-key lesser-evil-leader-map "l" nil)
  (define-key lesser-evil-leader-map "ll" 'persp-switch)
  (define-key lesser-evil-leader-map (kbd "l TAB") 'persp-switch-last)
  (define-key lesser-evil-leader-map "ld" 'persp-kill)
  (define-key lesser-evil-leader-map "la" 'persp-add-buffer)
  (define-key lesser-evil-leader-map "lr" 'persp-remove-buffer))


;; smartparens
(show-smartparens-global-mode t)
(smartparens-global-mode)
(defhydra "smartparens" (lesser-evil-leader-map "k")
  "Messing with parens"
  ("h" sp-backward-sexp "back")
  ("l" sp-next-sexp "forward")
  ("j" sp-down-sexp "down")
  ("k" sp-backward-up-sexp "up")

  ("d" sp-kill-sexp "copy")
  ("p" evil-paste-after "paste after")
  ("P" evil-paste-before "copy before")
  ("y" sp-copy-sexp "yank")
  ("u" undo-tree-undo "undo")

  ("a" sp-absorb-sexp "absorb")
  ("e" sp-extract-before-sexp "extract before")
  ("E" sp-extract-aftere-sexp "extract after")
  ("J" sp-join-sexp "join")
  ("B" sp-backward-barf-sexp "backward barf")
  ("s" sp-forward-slurp-sexp "forward slurp")
  ("S" sp-backward-slurp-sexp "backward slurp")
  ("r" sp-rewrap-sexp "rewrap")
  ("t" sp-transpose-sexp "transpose")
  ("w" sp-wrap-round "wrap")
  ("W" sp-unwrap-sexp "unwrap")
  )


;; python
;; TODO pyenv shim path for child processes
;; TODO autoload on visiting python files
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt"
      exec-path (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims")))
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> c") 'elpy-config)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> d") 'elpy-doc)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> g g") 'elpy-goto-definition)
(evil-define-key 'normal elpy-mode-map (kbd "RET") 'elpy-goto-definition)
(evil-define-key 'normal elpy-mode-map (kbd "DEL") 'pop-tag-mark)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> g G") 'elpy-goto-definition-other-window)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> g a") 'elpy-goto-definition)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> '") 'elpy-shell-switch-to-shell)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> s f") 'elpy-shell-send-defun)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> s F") 'elpy-shell-send-defun-and-go)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> s b") 'elpy-shell-send-buffer)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> s B") 'elpy-shell-send-buffer-and-go)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> s r") 'elpy-shell-send-region-or-buffer)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> s R") 'elpy-shell-send-region-or-buffer-and-go)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> S n") 'elpy-flymake-next-error)
(evil-define-key 'normal elpy-mode-map (kbd "<localleader> S p") 'elpy-flymake-previous-error)

(with-eval-after-load 'elpy
  (elpy-enable))

(defun enable-elpy-reload-buffer ()
    (interactive)
    ;; For some reason autoloading elpy upon opening first python buffer
    ;; doesn't apply <localleader> keymaps correctly
    ;; So by now just ugly hack with buffer reverting
    (unless (and (boundp 'elpy-enabled-p) (symbol-value 'elpy-enabled-p))
      (elpy-enable)
      (revert-buffer nil t)))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'enable-elpy-reload-buffer))


;; mu4e
(load "~/.emacs.d/mu4e.el")


;; apply keybindings descriptions to which-key
(dolist (kd lesser-evil-keys-desc)
  (if (car kd)
      (apply 'which-key-add-major-mode-key-based-replacements kd)
      (apply 'which-key-add-key-based-replacements (cdr kd))))


(server-start)

;; TODO org mode keybindings
;; TODO clojure mode (smartparens?)
;; TODO sql repl
;; TODO setup eshell
;; TODO check out hydra (ivy-hydra)
;; TODO folding
;; TODO pdf mode (check out pdf tools)
;; TODO ediff keybindings and floating window
;; Themes to consider:
;;   wombat (встроенная, чуть менее контрастно чем dakrone, но меньше цветов)
;;   doom-city-lights (средний контраст, разноцветная)
;;   doom-gruvbox (приятная, теплая менее контрастная чем monokai)
;;   doom-henna (средний контраст, зеленый, голубой) ++
;;   doom-laserwave (интересная тема пурпурного оттенка)
;;   doom-material (очень неплохо на среднем контрасте)
;;   doom-monokai-pro (чуть менее контрастная чем классический monokai)
;;   doom-one (очень неплохо на среднем контрасте)
;;   doom-opera (еще один интересный вариант на слабом контрасте)
;;   doom-palenight (средний контраст, неброско)
;;   doom-peacock (теплая, без броских цветов как в monokai) ++
;;   doom-solarized-dark and light (выглядит вроде получше классических) 
;;   doom-space-gray (еще одна слабоконтрастная с неплохой гаммой) ++
;;   doom-tomorrow-night (среднеконтрастная)
;;   doom-vibrant (средний контраст, неброские цвета, нелохая альтернатива dakrone)
;;   doom-wilmersdorf (еще одна слабоконтрастная с неплохой прохладной гаммой)
;;   humanoid-dark (средний контраст, синезеленоголубая с морскими оттенками) ++
;;   kaolin-blossom (коричнево-пурпурно)
;;   kaolin-bubblegum (контраст, фиолетово-бирюзовый)
;;   kaolin-dark (слабый контраст едва различимые оттенки зеленого)
;;   kaolin-eclipse (как blossom, только холоднее)
;;   kaolin-galaxy (контраст, бирюзовый и пурпурный)
;;   kaolin-mono-dark (слабый контраст, оттенки бирюзового)
;;   kaolin-ocean (контраст, разноцветная и прохладная)
;;   kaolin-temple (похожа на vim'овский desert)
;;   kaolin-valley-dark (похожа на ocean, только теплее)
;;   adwaita (светлая, если хочется серого фона, встроенная)
;;   deeper-blue (разноцветненько, встроенная)
;;   misterioso, tango-dark (тепло, разноцветно, но цвета коментов и строк непрактичные)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (hydra cider smartparens perspective mu4e-alert smooth-scrolling doom-themes humanoid-themes kaolin-themes company pyenv-mode-auto elpy ag org-bullets winum which-key ivy-rich counsel-projectile projectile evil-collection ivy evil magit evil-magit)))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
