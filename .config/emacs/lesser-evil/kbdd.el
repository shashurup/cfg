(require 'dbus)

(setq lesser-evil-kbdd-current-layout 0)

(defun lesser-evil-kbdd-set-layout (layout)
  (dbus-call-method-asynchronously :session
                                   "ru.gentoo.KbddService"
                                   "/ru/gentoo/KbddService"
                                   "ru.gentoo.kbdd"
                                   "set_layout"
                                   nil
                                   layout))

(defun lesser-evil-kbdd-get-layout (handler)
  (dbus-call-method-asynchronously :session
                                   "ru.gentoo.KbddService"
                                   "/ru/gentoo/KbddService"
                                   "ru.gentoo.kbdd"
                                   "getCurrentLayout"
                                   handler))

(defun lesser-evil-kbdd-push-mode (current-layout)
  (setq-local lesser-evil-kbdd-current-layout current-layout)
  (lesser-evil-kbdd-set-layout 0))

(defun lesser-evil-on-exit-insert-mode ()
  (lesser-evil-kbdd-get-layout 'lesser-evil-kbdd-push-mode))

(defun lesser-evil-on-enter-insert-mode ()
  (lesser-evil-kbdd-set-layout lesser-evil-kbdd-current-layout))

(add-hook 'evil-insert-state-exit-hook 'lesser-evil-on-exit-insert-mode)
(add-hook 'evil-insert-state-entry-hook 'lesser-evil-on-enter-insert-mode)

