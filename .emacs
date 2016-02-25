(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
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
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'evil)
(evil-mode 1)
(define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
(define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)
(evil-define-key 'normal python-mode-map (kbd "<backspace>") 'pop-tag-mark)

(elpy-enable)
(elpy-use-ipython)
(evil-define-key 'normal python-mode-map (kbd "RET") 'elpy-goto-definition)
