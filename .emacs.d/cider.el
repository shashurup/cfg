(add-hook 'clojure-mode-hook #'cider-mode)
(evil-define-key 'normal 'cider-mode
  (kbd "<localleader> e r") 'cider-eval-region
  (kbd "<localleader> e f") 'cider-eval-defun-at-point
  (kbd "<localleader> e m") 'cider-macroexpand-1
  (kbd "<localleader> e e") 'cider-eval-sexp-at-point
  
  (kbd "<localleader> h a") 'cider-apropos
  (kbd "<localleader> h h") 'cider-doc
  ; (kbd "<localleader> h g") 'cider-grimoir
  (kbd "<localleader> h j") 'cider-javadoc
  (kbd "<localleader> h n") 'cider-browse-ns
  
  (kbd "<localleader> '") 'cider-switch-to-repl-buffer
  (kbd "<localleader> s B") 'cider-load-buffer
  (kbd "<localleader> s b") 'cider-load-buffer-and-switch-to-repl-buffer
  (kbd "<localleader> s f") 'cider-insert-defun-in-repl
  (kbd "<localleader> s n") 'cider-insert-ns-form-in-repl
  (kbd "<localleader> s r") 'cider-insert-region-in-repl
  
  (kbd "<localleader> t e") 'cider-enlighten-mode
  (kbd "<localleader> t n") 'cider-toggle-trace-ns
  (kbd "<localleader> t v") 'cider-toggle-trace-var)

(evil-set-initial-state 'cider-repl-mode 'insert)
(evil-define-key 'normal cider-repl-mode-map
  (kbd "RET") 'cider-repl-return)
(evil-define-key '(normal insert) cider-repl-mode-map
  (kbd "C-j") 'cider-repl-next-input
  (kbd "C-k") 'cider-repl-previous-input)

(evil-set-initial-state 'cider-stacktrace-mode 'motion)
(evil-define-key 'motion cider-stacktrace-mode-map
  (kbd "TAB") 'cider-stacktrace-cycle-current-cause
  "J" 'cider-stacktrace-toggle-java)

(evil-set-initial-state 'cider-repl-history-mode 'motion)
(evil-define-key 'motion cider-repl-history-mode-map
  (kbd "RET") 'cider-repl-history-insert-and-quit
  "j" 'cider-repl-history-forward
  "k" 'cider-repl-history-previous)


;; TODO cider-macroexpansion-mode-map
;; TODO cider-connections-buffer-mode-map
;; TODO cider-inspector-mode-map
;; TODO cider--debug-mode-map
;; TODO cider-test-report-mode-map

(push '(clojure-mode
	"<localleader>e" "eval"
	"<localleader>h" "help"
	"<localleader>s" "repl"
	"<localleader>t" "toggle"
	)
      lesser-evil-keys-desc)
