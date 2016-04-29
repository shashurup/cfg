(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(evil-flash-delay 8)
 '(evil-symbol-word-search t)
 '(inhibit-startup-screen t)
 '(ns-command-modifier (quote control))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (eq system-type 'gnu/linux)
  (set-frame-font "Monospace-9" nil t))

(when (eq system-type 'darwin)
  (set-frame-font "Monaco-10" nil t)
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'evil)
(evil-mode 1)
(define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
(define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)
(evil-define-key 'normal python-mode-map (kbd "<backspace>") 'pop-tag-mark)

(setq python-check-command "flake8")

(elpy-enable)
(elpy-use-ipython)
(evil-define-key 'normal python-mode-map (kbd "RET") 'elpy-goto-definition)

(global-set-key (kbd "C-x g") 'magit-status)
(require 'evil-magit)

(projectile-global-mode)
(helm-projectile-on)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
