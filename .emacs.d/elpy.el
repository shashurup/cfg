
;; TODO pyenv shim path for child processes
(setenv "PATH" (concat (expand-file-name "~/.pyenv/shims") ":" (getenv "PATH")))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt"
      exec-path (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims")))

(evil-define-key 'normal 'elpy-mode
  (kbd "<localleader> c") 'elpy-config
  (kbd "<localleader> h") 'elpy-doc
  (kbd "<localleader> g g") 'elpy-goto-definition
  (kbd "RET") 'elpy-goto-definition
  (kbd "DEL") 'pop-tag-mark
  (kbd "<localleader> g G") 'elpy-goto-definition-other-window
  (kbd "<localleader> g a") 'elpy-goto-definition
  (kbd "<localleader> '") 'elpy-shell-switch-to-shell
  (kbd "<localleader> s f") 'elpy-shell-send-defun
  (kbd "<localleader> s F") 'elpy-shell-send-defun-and-go
  (kbd "<localleader> s b") 'elpy-shell-send-buffer
  (kbd "<localleader> s B") 'elpy-shell-send-buffer-and-go
  (kbd "<localleader> s r") 'elpy-shell-send-region-or-buffer
  (kbd "<localleader> s R") 'elpy-shell-send-region-or-buffer-and-go
  (kbd "<localleader> S n") 'elpy-flymake-next-error
  (kbd "<localleader> S p") 'elpy-flymake-previous-error)

(push '(python-mode
	"<localleader>s" "repl"
	"<localleader>S" "flymake"
	)
      lesser-evil-keys-desc)

(with-eval-after-load 'elpy
  (elpy-enable))

(with-eval-after-load 'python
  (pyenv-mode)
  (require 'pyenv-mode-auto)
  (add-hook 'python-mode-hook 'elpy-enable))
