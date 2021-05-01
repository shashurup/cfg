
;; pyenv shim path for child processes
(setenv "PATH" (concat (expand-file-name "~/.pyenv/shims") ":" (getenv "PATH")))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; elpy needs pip for startup which is not available in the default system python
(add-to-list 'exec-path (expand-file-name "~/.pyenv/shims"))

(evil-define-key 'normal 'elpy-mode
  (kbd "<localleader> c") 'elpy-config
  (kbd "<localleader> h") 'elpy-doc
  (kbd "<localleader> g d") 'elpy-goto-definition
  (kbd "<localleader> g D") 'elpy-goto-definition-other-window
  "gd" 'elpy-goto-definition
  "gD" 'elpy-goto-definition-other-window
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

(defun lesser-evil-current-pyenv-version ()
  (car (process-lines "pyenv" "version-name")))

(defun lesser-evil-elpy-activate-venv ()
  (when-let ((venv (lesser-evil-current-pyenv-version)))
    (pyvenv-activate (pyenv-mode-full-path venv))
    ;; buffer-local var for venv tracking
    (setq-local pyvenv-activate pyvenv-virtual-env)))

(with-eval-after-load 'python
  (require 'pyenv-mode) ;; we just need a function from it
  (pyvenv-tracking-mode)
  (add-hook 'python-mode-hook 'elpy-enable)
  (add-hook 'python-mode-hook 'lesser-evil-elpy-activate-venv))
