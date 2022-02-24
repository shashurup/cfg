(require 'cl-generic)
(require 'f)

(defvar lesser-evil-proj-marker ".leproj")

(defun lesser-evil-try-proj (dir)
  (when-let (proj-dir (f-traverse-upwards
                       (lambda (path)
                         (file-exists-p
                          (concat (file-name-as-directory path)
                                  lesser-evil-proj-marker)))
                       dir))
    (cons 'proj (abbreviate-file-name (file-name-as-directory proj-dir)))))

(cl-defmethod project-roots ((project (head proj)))
  (list (cdr project)))

(add-hook 'project-find-functions 'lesser-evil-try-proj)

(setq lesser-evil-proj-recent (make-hash-table :test 'equal))
(setq lesser-evil-proj-known-dir (make-hash-table :test 'equal))

(setq lesser-evil-proj-recent-filename
      (concat (file-name-directory load-file-name) "proj-recent"))

(load lesser-evil-proj-recent-filename t)

(defun lesser-evil-proj-save ()
  (with-temp-file lesser-evil-proj-recent-filename
    (princ "(setq lesser-evil-proj-recent " (current-buffer))
    (prin1 lesser-evil-proj-recent (current-buffer))
    (princ ")" (current-buffer))
    nil))

(add-hook 'kill-emacs-hook 'lesser-evil-proj-save)

(defun lesser-evil-proj-find-file-hook-function ()
  (unless (file-remote-p default-directory)
    (let ((dir (file-name-as-directory default-directory)))

      (let ((proj (or (gethash dir lesser-evil-proj-known-dir)
                      (puthash dir (or (project-current) 'noproject)
                               lesser-evil-proj-known-dir))))
        (unless (eq proj 'noproject)
          (let ((known-proj (gethash proj lesser-evil-proj-recent)))
            (puthash proj (float-time) lesser-evil-proj-recent)
            (unless known-proj
              (lesser-evil-proj-save))))))))

(add-hook 'find-file-hook 'lesser-evil-proj-find-file-hook-function)

(defun lesser-evil-proj-recent ()
  (let ((result '()))
    (maphash (lambda (k v)
               (push (list k  v) result))
             lesser-evil-proj-recent)
    (mapcar 'car (sort result (lambda (x1 x2)
                                (> (cadr x1)
                                   (cadr x2)))))))

(defun lesser-evil-proj-select ()
  (project-current nil
                   (ivy-read "Choose a project: "
                             (mapcar 'cdr (lesser-evil-proj-recent)))))

(defun lesser-evil-proj-get-or-select ()
  (if-let ((proj (project-current)))
      proj
    (lesser-evil-proj-select)))

(defun lesser-evil-proj-find-file-in (proj)
  (ivy-read (concat "proj " (cdr proj) " ")
            (project-files proj)
            :action 'find-file
            :caller 'lesser-evil-proj-find-file-in))

(defun lesser-evil-proj-find-file ()
  (interactive)
  (lesser-evil-proj-find-file-in (lesser-evil-proj-get-or-select)))

(defun lesser-evil-proj-find-file-other-proj ()
  (interactive)
  (lesser-evil-proj-find-file-in (lesser-evil-proj-select)))

(ivy-add-actions 'lesser-evil-proj-find-file-in
		 '(("j" find-file-other-window "other window")
                   ("l" find-file-literally "open literally")
                   ("r" counsel-find-file-as-root "open as root")))

(defun lesser-evil-proj-or-dir-ag ()
  (interactive)
  (if-let ((proj (project-current)))
      (counsel-ag (ivy-thing-at-point)
                  (car (project-roots proj)))
    (counsel-ag (ivy-thing-at-point))))

(defun lesser-evil-proj-ag ()
  (interactive)
  (let ((proj (lesser-evil-proj-get-or-select)))
    (counsel-ag (ivy-thing-at-point)
                  (car (project-roots proj)))))

(defun lesser-evil-proj-eshell ()
  (interactive)
  (let ((proj (lesser-evil-proj-get-or-select)))
    (let* ((default-directory (car (project-roots proj)))
           (eshell-buffer-name (format "*eshell %s*" default-directory)))
      (eshell))))

(defun lesser-evil-proj-shell-command ()
  (interactive)
  (let ((proj (lesser-evil-proj-get-or-select)))
    (let ((default-directory (car (project-roots proj))))
      (call-interactively 'shell-command))))

(defun lesser-evil-proj-dired ()
  (interactive)
  (let ((proj (lesser-evil-proj-get-or-select)))
    (dired (car (project-roots proj)))))

(define-key lesser-evil-leader-map "pp" 'lesser-evil-proj-find-file-other-proj)
(define-key lesser-evil-leader-map "pf" 'lesser-evil-proj-find-file)
(define-key lesser-evil-leader-map "/" 'lesser-evil-proj-or-dir-ag)
(define-key lesser-evil-leader-map "ps" 'lesser-evil-proj-ag)
(define-key lesser-evil-leader-map "sp" 'lesser-evil-proj-ag)
(define-key lesser-evil-leader-map "p'" 'lesser-evil-proj-eshell)
(define-key lesser-evil-leader-map "p!" 'lesser-evil-proj-shell-command)
(define-key lesser-evil-leader-map "pd" 'lesser-evil-proj-dired)

(ivy-add-actions 'lesser-evil-jump-to
		 '(("p" (lambda (x) (lesser-evil-proj-find-file-other-proj)) "switch project")))
