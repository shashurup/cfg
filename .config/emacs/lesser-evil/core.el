; core

(defun load-from-this-dir (name)
  (load (concat (file-name-directory load-file-name) name)))

(defmacro make-interactive (fn &rest args)
  `(lambda ()
     (interactive)
     ,(cons fn args)))

;; Unmap "SPC" so that it can be used by evil motion map
(define-key special-mode-map (kbd "SPC") nil)
(define-key Info-mode-map (kbd "SPC") nil)
(setq-default evil-symbol-word-search t)
(setq evil-flash-delay 8)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-move-beyond-eol t)
(setq evil-auto-balance-windows t)
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
(define-key lesser-evil-leader-map "adb" 'ediff-buffers)
(define-key lesser-evil-leader-map "add" 'ediff-directories)
(define-key lesser-evil-leader-map "adf" 'ediff-files)
(define-key lesser-evil-leader-map "ap" 'list-processes)
(define-key lesser-evil-leader-map "aP" 'proced)

(define-key lesser-evil-leader-map "bb" 'switch-to-buffer)
(define-key lesser-evil-leader-map "bd" 'kill-current-buffer)
(define-key lesser-evil-leader-map "bD" 'kill-buffer-and-window)
(define-key lesser-evil-leader-map "bk" 'kill-current-buffer)
(define-key lesser-evil-leader-map "bn" 'next-buffer)
(define-key lesser-evil-leader-map "bp" 'previous-buffer)
(define-key lesser-evil-leader-map "br" 'revert-buffer)
(define-key lesser-evil-leader-map "bs" (make-interactive switch-to-buffer "*scratch*"))

(define-key lesser-evil-leader-map "ff" 'find-file)
(define-key lesser-evil-leader-map "fr" 'recentf-open-files)
(define-key lesser-evil-leader-map "fs" 'save-buffer)
(define-key lesser-evil-leader-map "fS" 'write-file)
(define-key lesser-evil-leader-map "fy" (make-interactive kill-new (buffer-file-name)))

(define-key lesser-evil-leader-map "hdf" 'describe-function)
(define-key lesser-evil-leader-map "hdk" 'describe-key)
(define-key lesser-evil-leader-map "hdf" 'describe-face)
(define-key lesser-evil-leader-map "hdm" 'describe-mode)
(define-key lesser-evil-leader-map "hdv" 'describe-variable)
(define-key lesser-evil-leader-map "hdp" 'describe-package)
(define-key lesser-evil-leader-map "ha" 'apropos)
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
(define-key lesser-evil-leader-map "ss" 'occur)
(define-key lesser-evil-leader-map "so" 'xref-find-definitions)
(define-key lesser-evil-leader-map "sO" 'imenu)
(define-key lesser-evil-leader-map "sr" 'xref-find-references)

(define-key lesser-evil-leader-map "tf" 'auto-fill-mode)
(define-key lesser-evil-leader-map "tl" 'toggle-truncate-lines)
(define-key lesser-evil-leader-map "tn" 'linum-mode)

(define-key lesser-evil-leader-map "w=" 'balance-windows)
(define-key lesser-evil-leader-map "wd" 'evil-window-delete)
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
(define-key lesser-evil-leader-map "wo" 'delete-other-windows)
(define-key lesser-evil-leader-map "wO" 'delete-other-windows-vertically)
(define-key lesser-evil-leader-map "wm" 'maximize-window)
(define-key lesser-evil-leader-map "ws" 'evil-window-split)
(define-key lesser-evil-leader-map "wv" 'evil-window-vsplit)
(define-key lesser-evil-leader-map "ww" 'other-window)

(defhydra window-resize (lesser-evil-leader-map "wr")
  "Window resizing"
  ("j" shrink-window "shrink window")
  ("k" enlarge-window "enlarge window")
  ("h" shrink-window-horizontally "shrink window horizontally")
  ("l" enlarge-window-horizontally "enlarge window horizontally"))

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
	"<leader>t" "toggle"
	"<leader>w" "window"
	"<leader>wr" "resize"
	) lesser-evil-keys-desc)

;; undo for windows
(winner-mode)
(define-key lesser-evil-leader-map "wu" 'winner-undo)

;; Info
(evil-define-key 'motion Info-mode-map "n" 'evil-search-next)
(evil-define-key 'motion Info-mode-map "gg" 'beginning-of-buffer)
(evil-define-key 'motion Info-mode-map "gn" 'Info-goto-node)

;; calendar
(evil-collection-init 'calendar)

;; org
(setq org-confirm-babel-evaluate nil)
(define-key lesser-evil-leader-map "A" 'org-agenda)
(define-key lesser-evil-leader-map "c" 'org-capture)
(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-mode-hook 'evil-org-mode)
(with-eval-after-load 'evil-org
  (evil-org-set-key-theme '(textobjects)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sql . t)
   (clojure . t)
   (restclient . t)
   (shell . t)
   (emacs-lisp . t)))
(setq org-local-leader-map (make-sparse-keymap))
(evil-define-key 'normal org-mode-map
  "t" 'org-todo
  "<" 'org-metaleft
  ">" 'org-metaright
  (kbd "<localleader>") org-local-leader-map)
(evil-define-key 'emacs org-agenda-keymap
  "j" 'org-agenda-next-line
  "k" 'org-agenda-previous-line)
(with-eval-after-load 'org
  (define-key org-local-leader-map "," 'org-ctrl-c-ctrl-c)
  (define-key org-local-leader-map "/" 'org-sparse-tree)
  (define-key org-local-leader-map "." 'org-time-stamp)
  (define-key org-local-leader-map "a" 'org-archive-subtree)
  (define-key org-local-leader-map "A" 'org-agenda)
  (define-key org-local-leader-map "d" 'org-deadline)
  (define-key org-local-leader-map "r" 'org-refile)
  (define-key org-local-leader-map "s" 'org-schedule)
  (define-key org-local-leader-map "n" 'org-narrow-to-subtree)
  (define-key org-local-leader-map "N" 'widen)
  (define-key org-local-leader-map "p" 'org-set-property)
  (define-key org-local-leader-map "tc" 'org-table-insert-column)
  (define-key org-local-leader-map "td" 'org-table-delete-column)
  (define-key org-local-leader-map "th" 'org-table-move-column-left)
  (define-key org-local-leader-map "tl" 'org-table-move-column-right)
  (define-key org-local-leader-map "ts" 'org-table-insert-hline)
  (define-key org-local-leader-map "t?" 'org-table-field-info)
  (define-key org-local-leader-map "t," 'org-table-recalculate)
  (defun lesser-evil-org-delete-src-block ()
    (interactive)
    (let ((pos (org-babel-where-is-src-block-head)))
      (if pos
	  (progn
	    (goto-char pos)
	    (org-babel-remove-result)
	    (apply 'evil-delete (evil-org-an-object)))
	(progn
	  (org-babel-previous-src-block)
	  (org-babel-remove-result)))))
  (defun lesser-evil-org-yank-src-block ()
    (interactive)
    (if (not (org-babel-where-is-src-block-head))
	(org-babel-previous-src-block))
    (apply 'evil-yank (evil-org-an-object)))
  (defun lesser-evil-org-paste-src-block-before ()
    (interactive)
    (goto-char (car (evil-org-an-object)))
    (evil-paste-before 1))
  (defun lesser-evil-org-paste-src-block-after ()
    (interactive)
    (if (not (org-babel-where-is-src-block-head))
	(org-babel-previous-src-block))
    (when-let ((pos (org-babel-where-is-src-block-result)))
      (goto-char pos)
      (goto-char (nth 1 (evil-org-an-object)))
      (evil-paste-before 1)))
  (defun lesser-evil-org-goto-scratch-src-block ()
    (interactive)
    (org-babel-goto-named-src-block "scratch"))
  (defhydra "org source code blocks" (org-local-leader-map "c")
    ("," org-ctrl-c-ctrl-c "ctrl-c-ctrl-c")
    ("<tab>" org-cycle "cycle")
    ("j" org-babel-next-src-block "next block")
    ("k" org-babel-previous-src-block "prev block")
    ("J" org-forward-paragraph "next paragraph")
    ("K" org-backward-paragraph "prev paragraph")
    ("d" lesser-evil-org-delete-src-block "delete")
    ("y" lesser-evil-org-yank-src-block "yank")
    ("p" lesser-evil-org-paste-src-block-after "paste after")
    ("P" lesser-evil-org-paste-src-block-before "paste before")
    ("s" lesser-evil-org-goto-scratch-src-block  "scratch"))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (evil-define-key 'normal org-capture-mode-map
    (kbd "<localleader>,") 'org-capture-finalize
    (kbd "<localleader>k") 'org-capture-kill
    (kbd "<localleader>w") 'org-capture-refile))

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
  (define-key dired-mode-map "I" (make-interactive image-dired "."))
  (define-key dired-mode-map "gr" 'revert-buffer)
  (define-key dired-mode-map "gf" 'dired-goto-file)
  (define-key dired-mode-map "gG" 'dired-do-chgrp)
  (define-key dired-mode-map "{" 'dired-prev-subdir)
  (define-key dired-mode-map "}" 'dired-next-subdir))
(setq image-dired-thumb-size 256)
(evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
(evil-set-initial-state 'image-dired-display-image-mode 'emacs)
(with-eval-after-load 'image-dired
    (define-key image-dired-display-image-mode-map " " lesser-evil-leader-map)
    (define-key image-dired-thumbnail-mode-map " " lesser-evil-leader-map)
    (define-key image-dired-thumbnail-mode-map "j" 'image-dired-next-line)
    (define-key image-dired-thumbnail-mode-map "k" 'image-dired-previous-line)
    (define-key image-dired-thumbnail-mode-map "h" 'image-dired-backward-image)
    (define-key image-dired-thumbnail-mode-map "l" 'image-dired-forward-image)
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
(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "<localleader>sf") 'eval-defun
  (kbd "<localleader>sr") 'eval-region
  (kbd "<localleader>sb") 'eval-buffer)
(push '(emacs-lisp-mode
	"<localleader>s" "repl")
      lesser-evil-keys-desc)

;; man
(evil-define-key 'motion Man-mode-map
  (kbd "TAB") 'forward-button
  "gs" 'Man-goto-section
  "]]" 'Man-next-section
  "][" 'Man-next-section
  "[[" 'Man-previous-section
  "[]" 'Man-previous-section)

;; Help
(evil-define-key 'motion help-mode-map (kbd "TAB") 'forward-button)


;; apropos
(evil-define-key 'motion apropos-mode-map (kbd "TAB") 'forward-button)


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
(evil-collection-init 'ediff)
(defun lesser-evil-bind-ediff-meta ()
  (define-key ediff-meta-buffer-map " " lesser-evil-leader-map)
  (define-key ediff-meta-buffer-map "j" 'ediff-next-meta-item)
  (define-key ediff-meta-buffer-map "k" 'ediff-previous-meta-item))
(add-hook 'ediff-meta-buffer-keymap-setup-hook 'lesser-evil-bind-ediff-meta)
(push '(nil
	"<leader>ad" "diff"
	) lesser-evil-keys-desc)


;; comint
(evil-define-key '(normal insert) comint-mode-map
  (kbd "C-j") 'comint-next-matching-input-from-input
  (kbd "C-k") 'comint-previous-matching-input-from-input)
(evil-define-key 'normal comint-mode-map
  (kbd "RET") 'comint-send-input
  "[[" 'comint-previous-prompt
  "]]" 'comint-next-prompt)


;; eshell
(evil-set-initial-state 'eshell-mode 'insert)
;; eshell-mode-map is buffer local for some reason
;; so we need to set it in the mode hook
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (evil-define-key '(normal insert) eshell-mode-map
	      (kbd "C-j") 'eshell-next-matching-input-from-input
	      (kbd "C-k") 'eshell-previous-matching-input-from-input)
	    (evil-define-key 'normal eshell-mode-map
	      (kbd "RET") 'eshell-send-input
	      "[[" 'eshell-previous-prompt
	      "]]" 'eshell-next-prompt)))


;; sql
(evil-set-initial-state 'sql-interactive-mode 'insert)
(evil-define-key 'normal sql-interactive-mode-map
  (kbd "RET") 'comint-send-input
  "[[" 'comint-previous-prompt
  "]]" 'comint-next-prompt)
(evil-define-key '(normal insert) sql-interactive-mode-map
  (kbd "C-j") 'comint-next-matching-input-from-input
  (kbd "C-k") 'comint-previous-matching-input-from-input)

(evil-define-key 'normal sql-mode-map
  (kbd "<localleader> s b") 'sql-send-buffer
  (kbd "<localleader> s s") 'sql-send-paragraph
  (kbd "<localleader> s r") 'sql-send-region)

(push '(sql-mode
	"<localleader>s" "repl"
	)
      lesser-evil-keys-desc)


;; HideShow
(evil-define-key 'normal 'hs-minor-mode
  (kbd "<localleader>zz") 'hs-toggle-hiding
  (kbd "<localleader>zA") 'hs-hide-all
  (kbd "<localleader>za") 'hs-show-all
  (kbd "<localleader>zl") 'hs-hide-level)
;; support for xml folding
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
; doesnt work for minor mode
;(push '(nil
;	"<localleader>t" "folding"
;	)
;      lesser-evil-keys-desc)


;; eww
(evil-set-initial-state 'eww-mode 'motion)
(evil-define-key 'motion eww-mode-map
  (kbd "TAB") 'shr-next-link
  "H" 'eww-back-url
  "L" 'eww-forward-url
  "gr" 'eww-reload)


;; xref 
(evil-set-initial-state 'xref--xref-buffer-mode 'motion)
(evil-define-key 'motion xref--xref-buffer-mode-map
  (kbd "TAB") 'xref-goto-xref
  (kbd "RET") 'xref-quit-and-goto-xref)


;;======================================================================

;; 3rd party stuff

;; ag
(define-key lesser-evil-leader-map "sf" 'ag-dired)
(define-key lesser-evil-leader-map "sg" 'ag)
(push '(nil
	"<leader>sf" "for file"
	"<leader>sg" "a directory"
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
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "C-RET") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "S-<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "S-RET") 'ivy-alt-done)
(define-key ivy-switch-buffer-map (kbd "C-k") nil)
(define-key ivy-switch-buffer-map (kbd "C-w") 'ivy-switch-buffer-kill)
(evil-set-initial-state 'ivy-occur-mode 'emacs)
(define-key ivy-occur-mode-map " " lesser-evil-leader-map)
(define-key ivy-occur-mode-map (kbd "C-f") 'scroll-up)
(define-key ivy-occur-mode-map (kbd "C-b") 'scroll-down)
(define-key ivy-occur-mode-map "G" 'end-of-buffer)
(define-key ivy-occur-mode-map "g" nil)
(define-key ivy-occur-mode-map "gg" 'beginning-of-buffer)

(defun counsel-ag-ask-dir ()
  (interactive)
  (counsel-ag
   (ivy-thing-at-point)
   (counsel-read-directory-name "ag in directory:")))

(with-eval-after-load 'counsel
  ;; this is just a modified copy of counsel function
  ;; which uses other window
  (defun counsel-git-grep-action-other-window (x)
    "Go to occurrence X in current Git repository."
    (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
      (let ((file-name (match-string-no-properties 1 x))
	    (line-number (match-string-no-properties 2 x)))
	(find-file-other-window (expand-file-name
				 file-name
				 (ivy-state-directory ivy-last)))
	(goto-char (point-min))
	(forward-line (1- (string-to-number line-number)))
	(when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
	  (when swiper-goto-start-of-match
	    (goto-char (match-beginning 0))))
	(swiper--ensure-visible)
	(run-hooks 'counsel-grep-post-action-hook)
	(unless (eq ivy-exit 'done)
	  (swiper--cleanup)
	  (swiper--add-overlays (ivy--regex ivy-text))))))

    (ivy-add-actions 'counsel-ag
		     '(("j" counsel-git-grep-action-other-window "other window")))
  )

(define-key lesser-evil-leader-map " " 'counsel-M-x)
(define-key lesser-evil-leader-map "bb" 'counsel-switch-buffer)
(define-key lesser-evil-leader-map "ff" 'counsel-find-file)
(define-key lesser-evil-leader-map "fr" 'counsel-recentf)
(define-key lesser-evil-leader-map "ha" 'counsel-apropos)
(define-key lesser-evil-leader-map "hdf" 'counsel-describe-function)
(define-key lesser-evil-leader-map "hdv" 'counsel-describe-variable)
(define-key lesser-evil-leader-map "hdk" 'counsel-descbinds)
(define-key lesser-evil-leader-map "sb" 'swiper-thing-at-point)
(define-key lesser-evil-leader-map "sd" (make-interactive counsel-ag
							  (ivy-thing-at-point)))
(define-key lesser-evil-leader-map "sg" 'counsel-ag-ask-dir)
(define-key lesser-evil-leader-map "ss" 'swiper-thing-at-point)
(define-key lesser-evil-leader-map "sO" 'counsel-imenu)
(push '(nil
	"<leader>sb" "current buffer"
	"<leader>sd" "current directory"
	"<leader>ss" "current buffer"
	)
      lesser-evil-keys-desc)

;; Single entry point for files and buffers
;(load (concat user-emacs-directory "ctrlj.el"))
(load-from-this-dir "ctrlj")


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


; winum
(defun lesser-evil-find-window (w1 w2 num)
  (when w2
    (if (eq w1 w2)
	num
      (lesser-evil-find-window w1
			       (window-in-direction 'right w2)
			       (1+ num)))))

;; Assign window numbers left to right first
(defun lesser-evil-window-num ()
  (lesser-evil-find-window (selected-window) (frame-first-window) 1))

(defun lesser-evil-display-buffer-in (num)
  (set-window-buffer (winum-get-window-by-number num)
                     (current-buffer))
  (switch-to-prev-buffer))

(define-key lesser-evil-leader-map "0" 'winum-select-window-0)
(define-key lesser-evil-leader-map "1" 'winum-select-window-1)
(define-key lesser-evil-leader-map "2" 'winum-select-window-2)
(define-key lesser-evil-leader-map "3" 'winum-select-window-3)
(define-key lesser-evil-leader-map "4" 'winum-select-window-4)
(define-key lesser-evil-leader-map "5" 'winum-select-window-5)
(define-key lesser-evil-leader-map "6" 'winum-select-window-6)
(define-key lesser-evil-leader-map "7" 'winum-select-window-7)
(define-key lesser-evil-leader-map "8" 'winum-select-window-8)
(define-key lesser-evil-leader-map "9" 'winum-select-window-9)
(define-key lesser-evil-leader-map "b1" (make-interactive lesser-evil-display-buffer-in 1))
(define-key lesser-evil-leader-map "b2" (make-interactive lesser-evil-display-buffer-in 2))
(define-key lesser-evil-leader-map "b3" (make-interactive lesser-evil-display-buffer-in 3))
(define-key lesser-evil-leader-map "b4" (make-interactive lesser-evil-display-buffer-in 4))
(define-key lesser-evil-leader-map "b5" (make-interactive lesser-evil-display-buffer-in 5))
(define-key lesser-evil-leader-map "b6" (make-interactive lesser-evil-display-buffer-in 6))
(define-key lesser-evil-leader-map "b7" (make-interactive lesser-evil-display-buffer-in 7))
(define-key lesser-evil-leader-map "b8" (make-interactive lesser-evil-display-buffer-in 8))
(define-key lesser-evil-leader-map "b9" (make-interactive lesser-evil-display-buffer-in 9))
(with-eval-after-load 'winum
  (winum-mode)
  ; TODO enable this again after fixing problems with multiple frames
  ; (add-to-list 'winum-assign-functions #'lesser-evil-window-num)
  )


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
(evil-define-key 'normal magit-mode-map
  " " lesser-evil-leader-map)
(evil-define-key 'normal with-editor-mode-map
  (kbd "<localleader> ,") 'with-editor-finish
  (kbd "<localleader> k") 'with-editor-cancel)
(evil-define-key 'normal magit-diff-mode-map
  " " lesser-evil-leader-map)
(evil-initial-state 'magit-diff-mode 'motion)
(with-eval-after-load 'magit
  (require 'evil-magit))


;; company
(global-company-mode)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)


;; layouts with perspective
;; Switching layout function cannot be autoloaded
;; and we cannot load perspective lazily
;; so we use this hack to at least turn on
;; perspective mode with familiar keybinding
(define-key lesser-evil-leader-map "l" 'persp-mode)
(with-eval-after-load 'perspective

  (if (featurep 'ivy-rich)
      (let ((cmd 'persp-ivy-switch-buffer)
	    (props (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)))
	(ivy-set-display-transformer cmd (ivy-rich-build-transformer cmd props))))

  (if (featurep 'counsel)
      (define-key lesser-evil-leader-map "bb" 'persp-counsel-switch-buffer))

  (define-key lesser-evil-leader-map "l" nil)
  (define-key lesser-evil-leader-map "ll" 'persp-switch)
  (define-key lesser-evil-leader-map (kbd "l TAB") 'persp-switch-last)
  (define-key lesser-evil-leader-map
              "ln"
              (lambda (persp-name)
		(interactive "sNew perspective name: ")
		(let ((buf (current-buffer)))
		  (persp-switch persp-name)
		  (persp-set-buffer (buffer-name buf))
		  (display-buffer buf))))
  (define-key lesser-evil-leader-map "ld" 'persp-kill)
  (define-key lesser-evil-leader-map "la" 'persp-add-buffer)
  (define-key lesser-evil-leader-map "lr" (make-interactive persp-remove-buffer
							    (current-buffer))))


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
  ("E" sp-extract-after-sexp "extract after")
  ("J" sp-join-sexp "join")
  ("o" sp-splice-sexp-killing-around "leave only")
  ("b" sp-forward-barf-sexp "backward barf")
  ("B" sp-backward-barf-sexp "backward barf")
  ("s" sp-forward-slurp-sexp "forward slurp")
  ("S" sp-backward-slurp-sexp "backward slurp")
  ("r" sp-rewrap-sexp "rewrap")
  ("t" sp-transpose-sexp "transpose")
  ("w" sp-wrap-round "wrap")
  ("W" sp-splice-sexp "unwrap"))

(push '(nil
	"<leader>k" "sexp"
	)
      lesser-evil-keys-desc)


;; python
(load-from-this-dir "elpy")


;; clojure
(load-from-this-dir "cider")


;; mu4e
(load-from-this-dir "mu4e")

;; ctags update
(setq tags-add-tables nil)
(defun lesser-evil-enable-ctags ()
  (turn-on-ctags-auto-update-mode)
  (when-let ((tf (ctags-update-find-tags-file)))
    (visit-tags-table tf t)))

(add-hook 'sql-mode-hook 'lesser-evil-enable-ctags)
(add-hook 'java-mode-hook 'lesser-evil-enable-ctags)
(add-hook 'ruby-mode-hook 'lesser-evil-enable-ctags)

;; plantuml
(setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(evil-define-key 'normal plantuml-mode-map
  (kbd "<localleader> ,") 'plantuml-preview)

;; apply keybindings descriptions to which-key
(dolist (kd lesser-evil-keys-desc)
  (if (car kd)
      (apply 'which-key-add-major-mode-key-based-replacements kd)
      (apply 'which-key-add-key-based-replacements (cdr kd))))

;; kbdd
(load-from-this-dir "kbdd")


;; xref window bindings
;; TODO sql repl establish connection by postgre connection string
;; TODO pdf mode (check out pdf tools)
;; TODO ediff keybindings and floating window
;; TODO sudo support
