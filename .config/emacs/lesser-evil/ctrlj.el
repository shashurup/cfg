;; -*- lexical-binding: t -*-

(defun lesser-evil-shorten-file-name (file-name dir)
  (let ((abbr (abbreviate-file-name file-name))
        (rel  (file-relative-name file-name dir)))
    (if (> (length abbr) (length rel)) rel abbr)))

(defun lesser-evil-buffer-list (dir)
  (let* ((persp-enabled (functionp #'persp-buffer-filter))
	 (inappropriate (lambda (b)
			  (or
			   (and persp-enabled (persp-buffer-filter b))
			   (eq (aref (buffer-name b) 0) 32))))
	 (make-name (lambda (b)
		      (propertize
		       (if-let ((file-name (buffer-file-name b)))
			   (lesser-evil-shorten-file-name file-name dir)
			 (buffer-name b))
		      'face 'counsel-outline-5))))
    (mapcar make-name
	    (cl-remove-if inappropriate
			  (buffer-list)))))

(defun lesser-evil-dir-file-list (dir)
  (let ((mark-dir (lambda (f) (if (file-directory-p f)
				  (propertize (file-name-as-directory f)
					      'face 'ivy-subdir)
				f)))
        (inappropriate (lambda (f)
                         (or (get-file-buffer f)
                             (string= f ".")
                             (string= f "..")))))
    (mapcar mark-dir
	    (cl-remove-if inappropriate
			  (directory-files dir)))))

(defun lesser-evil-proj-file-list (proj)
  (let ((dir (project-root proj)))
    (mapcar (lambda (f) (file-relative-name f dir))
	    (cl-remove-if #'get-file-buffer
			  (project-files proj)))))

(defun lesser-evil-jump-action (item dir find-file-func switch-buffer-func)
  (if-let (buff (get-buffer item))
      (funcall switch-buffer-func buff)
    (funcall find-file-func (expand-file-name item dir))))

(ivy-add-actions 'lesser-evil-jump-to
		 '(("d" counsel-find-file-mkdir-action "mkdir")
                   ("l" find-file-literally "open literally")
                   ("r" counsel-find-file-as-root "open as root")
                   ("f" (lambda (x) (counsel-find-file)) "switch dir")))

(defun lesser-evil-cur-dir ()
  (plist-get (ivy-state-extra-props ivy-last)
             :lesser-evil-dir))

(defun lesser-evil-counsel-at (dir)
  (ivy-set-action 'counsel-find-file)
  (ivy--done dir))

(defun lesser-evil-jump-backward-delete-char ()
  (if (eq (ivy-state-caller ivy-last) 'lesser-evil-jump-to)
      (let ((dir (lesser-evil-cur-dir)))
        (lesser-evil-counsel-at (file-name-directory (directory-file-name dir))))
    (abort-recursive-edit)))

(setq ivy-on-del-error-function 'lesser-evil-jump-backward-delete-char)

(defun lesser-evil-partial-or-done ()
  (interactive)
  (ivy-partial-or-done)
  (if (equal "/" (substring ivy-text -1))
      (lesser-evil-counsel-at
       (expand-file-name ivy-text
                         (lesser-evil-cur-dir)))))

(defun lesser-evil-alt-done (&optional arg)
  (interactive)
  (let ((dir (lesser-evil-cur-dir))
        (cand (ivy-state-current ivy-last)))
    (if (equal "/" (substring cand -1))
        (lesser-evil-counsel-at (expand-file-name cand dir))
      (ivy-alt-done))))

(defun lesser-evil-jump-to ()
  (interactive)
  (let* ((proj (project-current))
	 (dir (if proj
		  (car (project-roots proj))
		default-directory))
	 (coll (if proj
		   (append (lesser-evil-buffer-list dir)
			   (lesser-evil-proj-file-list proj))
		 (append (lesser-evil-buffer-list dir)
			 (lesser-evil-dir-file-list dir))))
	 (prompt (concat (if proj "proj " "dir ") dir " "))
	 (other (lambda (item)
		  (lesser-evil-jump-action item
					   dir
					   #'find-file-other-window
					   #'switch-to-buffer-other-window)))
         (km (make-sparse-keymap)))
    (define-key km (kbd "TAB") 'lesser-evil-partial-or-done)
    (define-key km (kbd "S-RET") 'lesser-evil-alt-done)
    (define-key km (kbd "S-<return>") 'lesser-evil-alt-done)
    (ivy-add-actions this-command `(("j" ,other "other window")))
    (ivy-read prompt
	      coll
	      :action (lambda (item)
			(lesser-evil-jump-action item
						 dir
						 #'find-file
						 #'switch-to-buffer))
	      :preselect (buffer-name (other-buffer (current-buffer)))
              :keymap km
	      :extra-props (list :lesser-evil-dir dir))))

(define-key lesser-evil-leader-map "j" 'lesser-evil-jump-to)
