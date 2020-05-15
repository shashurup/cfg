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

(setq my-root-map (make-sparse-keymap))

; Basic keybinding are inspired by spacemacs
(define-key my-root-map " " 'execute-extended-command)
(define-key my-root-map (kbd "TAB") 'evil-switch-to-windows-last-buffer)
(define-key my-root-map ":" 'eval-expression)
(define-key my-root-map "'" 'eshell)
(define-key my-root-map "!" 'shell-command)
(define-key my-root-map "u" 'universal-argument)

(define-key my-root-map "aC" 'calendar)
(define-key my-root-map "ac" 'calc-dispatch)
(define-key my-root-map "ap" 'list-processes)
(define-key my-root-map "aP" 'proced)

(define-key my-root-map "bb" 'switch-to-buffer)
(define-key my-root-map "bd" 'kill-current-buffer)
(define-key my-root-map "bD" 'kill-buffer-and-window)
(define-key my-root-map "bk" 'kill-current-buffer)
(define-key my-root-map "bn" 'next-buffer)
(define-key my-root-map "bp" 'previous-buffer)
(define-key my-root-map "br" 'revert-buffer)
(define-key my-root-map "bs" (lambda ()
			       (interactive)
			       (switch-to-buffer "*scratch*")))

(define-key my-root-map "ff" 'find-file)
(define-key my-root-map "fr" 'recentf-open-files)
(define-key my-root-map "fs" 'save-buffer)

(define-key my-root-map "hdf" 'describe-function)
(define-key my-root-map "hdk" 'describe-key)
(define-key my-root-map "hdm" 'describe-mode)
(define-key my-root-map "hdv" 'describe-variable)
(define-key my-root-map "hdp" 'describe-package)
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

(define-key my-root-map "tf" 'auto-fill-mode)
(define-key my-root-map "tl" 'toggle-truncate-lines)
(define-key my-root-map "tn" 'linum-mode)

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
(define-key my-root-map "wm" 'maximize-window)
(define-key my-root-map "ws" 'split-window-vertically)
(define-key my-root-map "wv" 'split-window-horizontally)
(define-key my-root-map "ww" 'other-window)

(evil-define-key 'motion 'global (kbd "<leader>") my-root-map)

;; undo for windows
(winner-mode)
(define-key my-root-map "wu" 'winner-undo)

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
  (define-key proced-mode-map " " my-root-map)
  (my-bind-basic-motion proced-mode-map)
  (define-key proced-mode-map "gr" 'revert-buffer))

;; Package list
(define-key package-menu-mode-map " " my-root-map)
(my-bind-basic-motion package-menu-mode-map)
(define-key package-menu-mode-map "gr" 'revert-buffer)

;; Dired
(evil-set-initial-state 'dired-mode 'emacs)
(with-eval-after-load 'dired
  (define-key dired-mode-map " " my-root-map)
  (my-bind-basic-motion dired-mode-map)
  (define-key dired-mode-map "J" 'dired-goto-file)
  (define-key dired-mode-map "K" 'dired-do-kill-lines)
  (define-key dired-mode-map "gr" 'revert-buffer)
  (define-key dired-mode-map "gf" 'dired-goto-file)
  (define-key dired-mode-map "gG" 'dired-do-chgrp))

;; Custom
(with-eval-after-load 'cus-edit
  (define-key custom-mode-map " " my-root-map)
  (define-key custom-mode-map "j" 'widget-forward)
  (define-key custom-mode-map "k" 'widget-backward))

;; Emacs lisp
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
  (define-key doc-view-mode-map " " my-root-map)
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
  (define-key image-mode-map " " my-root-map)
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
(define-key my-root-map "bb" 'ivy-switch-buffer)
(define-key my-root-map "ff" 'counsel-find-file)
(define-key my-root-map "fr" 'counsel-recentf)
(define-key my-root-map "hdf" 'counsel-describe-function)
(define-key my-root-map "hdv" 'counsel-describe-variable)
(define-key my-root-map "hdk" 'counsel-descbinds)
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


;; smooth scrolling
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)


;; which key
(define-key my-root-map (kbd "h SPC") 'which-key-show-top-level)
(which-key-mode)


;; magit
(define-key my-root-map "gs" 'magit-status)
(define-key my-root-map "gb" 'magit-blame-addition)
(define-key my-root-map "gm" 'magit-dispatch)
(define-key my-root-map "gl" 'magit-log-buffer-file)
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
(define-key my-root-map "l" 'persp-mode)
(with-eval-after-load 'perspective

  (if (featurep 'ivy-rich)
      (let ((cmd 'persp-ivy-switch-buffer)
	    (props (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)))
	(ivy-set-display-transformer cmd (ivy-rich-build-transformer cmd props))))

  (if (featurep 'ivy)
      (define-key my-root-map "bb" 'persp-ivy-switch-buffer))

  (add-to-list 'mu4e-view-actions
	       '("View in browser" . mu4e-action-view-in-browser) t)

  (define-key my-root-map "l" nil)
  (define-key my-root-map "ll" 'persp-switch)
  (define-key my-root-map (kbd "l TAB") 'persp-switch-last)
  (define-key my-root-map "ld" 'persp-kill)
  (define-key my-root-map "la" 'persp-add-buffer)
  (define-key my-root-map "lr" 'persp-remove-buffer))


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

(server-start)
;; TODO org mode keybindings
;; TODO clojure mode (smartparens?)
;; TODO tune thumbnails mode
;; TODO sql repl
;; TODO setup eshell
;; TODO check out hydra (ivy-hydra)
;; TODO folding
;; TODO keybindings in magit commit buffer
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
    (perspective mu4e-alert smooth-scrolling doom-themes humanoid-themes kaolin-themes company pyenv-mode-auto elpy ag org-bullets winum which-key ivy-rich counsel-projectile projectile evil-collection ivy evil magit evil-magit)))
 '(tool-bar-mode nil)
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
