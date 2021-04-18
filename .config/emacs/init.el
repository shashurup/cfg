;; Common vars
(setq user-full-name "First Last"
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
(setq visible-bell 1)
(column-number-mode)
(setq-default indent-tabs-mode nil)

;; Package initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; Do not initialize packages again after init
(setq package-enable-at-startup nil)

(defun set-comfortable-margins ()
  (setq left-margin-width 1
	right-margin-width 1)
  (when-let ((w (get-buffer-window (current-buffer))))
    (set-window-buffer w (current-buffer))))

(with-eval-after-load 'info
  (add-hook 'Info-mode-hook 'set-comfortable-margins))

(with-eval-after-load 'help
  (add-hook 'help-mode-hook 'set-comfortable-margins))

(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook 'set-comfortable-margins))

;; org
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list (concat org-directory "/my.org")
                             (concat org-directory "/projects")))
(setq org-refile-targets '((org-agenda-files . (:level . 1))))
(setq org-capture-templates
      '(("t" "Todo" entry (file "")
         "* TODO %?\n  %i\n  %a")
        ("c" "Note" entry (file "")
         "* %?\n  %i\n  %a")
        ("e" "External note (from clipboard)" entry (file "")
         "* %?\n  %x")
        ))
(defun lesser-evil-org-capture-external-note ()
  (interactive)
  (let ((org-capture-p (lambda (w)
                         (string-prefix-p "CAPTURE-"
                                          (buffer-name (window-buffer w))))))
    (org-capture nil "e")
    (let ((capture-win (seq-find org-capture-p (window-list))))
      (delete-other-windows capture-win)
      (add-hook 'kill-buffer-hook 'delete-frame 0 t))
    ))

;; theme
;(setq doom-henna-brighter-modeline t)
(load-theme 'doom-gruvbox t)

(load (concat user-emacs-directory "lesser-evil/core"))

(savehist-mode)

(server-start)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(docker-tramp plantuml-mode base16-theme htmlize lua-mode ivy-xref ctags-update ggtags markdown-mode ivy-hydra evil-org evil-text-object-python ob-restclient restclient hydra cider smartparens perspective mu4e-alert smooth-scrolling doom-themes humanoid-themes kaolin-themes company pyenv-mode-auto elpy ag org-bullets winum which-key ivy-rich counsel-projectile projectile evil-collection ivy evil magit evil-magit))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 57 :family "Monospace"))))
 '(fixed-pitch-serif ((t (:height 1.1 :family "Linux Libertine Mono O"))))
 '(variable-pitch ((t (:height 1.1 :family "Sans")))))


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

