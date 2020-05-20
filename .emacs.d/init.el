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

(load (concat user-emacs-directory "lesser-evil.el"))

(server-start)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (restclient hydra cider smartparens perspective mu4e-alert smooth-scrolling doom-themes humanoid-themes kaolin-themes company pyenv-mode-auto elpy ag org-bullets winum which-key ivy-rich counsel-projectile projectile evil-collection ivy evil magit evil-magit)))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

