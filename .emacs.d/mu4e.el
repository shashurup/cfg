(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(defun my-message-folder-matches (msg pattern )
    (when msg (string-match-p pattern (mu4e-message-field msg :maildir))))

(defun my-mu4e-context ()
  `( ,(make-mu4e-context
       :name "yandex"
       :match-func (lambda (msg) (my-message-folder-matches msg "^/yandex"))
       :vars '((user-mail-address . "name@example.com")
	       (mu4e-sent-folder   . "/yandex/Отправленные")
	       (mu4e-drafts-folder . "/yandex/Черновики")
	       (mu4e-trash-folder  . "/yandex/Удаленные")
	       (mu4e-refile-folder . "/yandex/Archive")
	       (mu4e-compose-signature .
				       (concat "Regard,\n"
					       "Name\n"))
	       (mu4e-sent-messages-behavior . sent)
	       (smtpmail-smtp-server  . "smtp.yandex.ru")
	       (smtpmail-smtp-user    . "name@example.com")
	       (smtpmail-smtp-service . 587)))
     ,(make-mu4e-context
       :name "sdrj"
       :match-func (lambda (msg) (my-message-folder-matches msg "^/sdrj"))
       :vars '((user-mail-address . "name@example.com")
	       (mu4e-drafts-folder . "/sdrj/[Gmail]/Черновики")
	       (mu4e-refile-folder . "/sdrj/[Gmail]/Вся почта")
	       (mu4e-trash-folder . "/trash")
	       (mu4e-compose-signature .
				       (concat "Regard,\n"
					       "Name\n"))
	       (mu4e-sent-messages-behavior . delete)
	       (smtpmail-smtp-server  . "smtp.gmail.com")
	       (smtpmail-smtp-user    . "name@example.com")
	       (smtpmail-smtp-service . 587))))

(setq send-mail-function 'smtpmail-send-it
      auth-sources '(default)
      ; mu4e-view-use-gnus t
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-context-policy 'ask-if-none
      mu4e-change-filenames-when-moving t
      mu4e-get-mail-command "sync-mail"
      mu4e-user-mail-address-list '("name@example.com"
				    "name@example.com"
				    "name@example.com")
      mu4e-headers-fields '((:human-date . 12)
			    (:flags . 6)
			    ; (:maildir . 12)
			    (:from-or-to . 24)
			    (:subject)))

(with-eval-after-load 'mu4e

  (require 'evil)
  (require 'ivy)

  (setq mu4e-completing-read-function 'ivy-read)
  
  ;; Binding fixed
  (define-key my-root-map "aM" 'mu4e)
  (define-key mu4e-main-mode-map " " my-root-map)
  (define-key mu4e-headers-mode-map " " my-root-map)
  (define-key mu4e-headers-mode-map "j" 'mu4e-headers-next)
  (define-key mu4e-headers-mode-map "k" 'mu4e-headers-prev)
  (define-key mu4e-headers-mode-map (kbd "C-f") 'evil-scroll-page-down)
  (define-key mu4e-headers-mode-map (kbd "C-b") 'evil-scroll-page-up)
  (evil-set-initial-state 'mu4e-view-mode 'motion)
  (evil-define-key 'motion mu4e-view-mode-map (kbd "C-j") 'mu4e-view-headers-next)
  (evil-define-key 'motion mu4e-view-mode-map (kbd "C-k") 'mu4e-view-headers-prev)
  (evil-define-key 'normal mu4e-compose-mode-map (kbd "<localleader> ,") 'message-send-and-exit)
  (evil-define-key 'normal mu4e-compose-mode-map (kbd "<localleader> s") 'message-send)
  (evil-define-key 'normal mu4e-compose-mode-map (kbd "<localleader> a") 'mml-attach-file)

  (setq mu4e-contexts (my-mu4e-context))

;; Fix opening attached eml files
(defun mu4e~view-mark-as-read-maybe (msg)
  "Clear the message MSG New/Unread status and set it to Seen.
If the message is not New/Unread, do nothing. Evaluates to t if it
triggers any changes, nil otherwise. If this function does any
changes, it triggers a refresh."
  (when (and mu4e-view-auto-mark-as-read msg)
    (let ((flags (mu4e-message-field msg :flags))
	   (msgid (mu4e-message-field msg :message-id))
	   (docid (mu4e-message-field msg :docid)))
      ;; attached (embedded) messages don't have docids; leave them alone if it
      ;; is a new message
      (mu4e-log 'misc "Embedded message docid %s" docid)
      (when (and (> docid 0) (or (member 'unread flags) (member 'new flags)))
	;; mark /all/ messages with this message-id as read, so all copies of
	;; this message will be marked as read. We don't want an update thougn,
	;; we want a full message, so images etc. work correctly.
	(mu4e~proc-move msgid nil "+S-u-N" 'noview)
        (mu4e~proc-view docid mu4e-view-show-images (mu4e~decrypt-p msg))
	t)))))

(require 'mu4e)
