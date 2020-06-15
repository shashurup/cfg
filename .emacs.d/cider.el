(setq org-babel-clojure-backend 'cider)
(add-hook 'clojure-mode-hook #'cider-mode)
(setq cider-prompt-for-symbol nil)
(evil-define-key 'normal 'cider-mode
  (kbd "<localleader> e r") 'cider-eval-region
  (kbd "<localleader> e f") 'cider-eval-defun-at-point
  (kbd "<localleader> e m") 'cider-macroexpand-1
  (kbd "<localleader> e M") 'cider-macroexpand-all
  (kbd "<localleader> e e") 'cider-eval-sexp-at-point
  (kbd "<localleader> e i") 'cider-inspector

  "gd" 'cider-find-dwim
  "gD" 'cider-find-dwim-other-window
  (kbd "<localleader> g d") 'cider-find-dwim
  (kbd "<localleader> g D") 'cider-find-dwim-other-window
  (kbd "<localleader> g n") 'cider-find-ns
  
  (kbd "<localleader> h a") 'cider-apropos
  (kbd "<localleader> h h") 'cider-doc
  ; (kbd "<localleader> h g") 'cider-grimoir
  (kbd "<localleader> h j") 'cider-javadoc
  (kbd "<localleader> h n") 'cider-browse-ns
  
  (kbd "<localleader> '") 'cider-switch-to-repl-buffer
  (kbd "<localleader> s b") 'cider-load-buffer
  (kbd "<localleader> s B") 'cider-load-buffer-and-switch-to-repl-buffer
  (kbd "<localleader> s f") 'cider-insert-defun-in-repl
  (kbd "<localleader> s n") 'cider-insert-ns-form-in-repl
  (kbd "<localleader> s r") 'cider-insert-region-in-repl
  
  (kbd "<localleader> t e") 'cider-enlighten-mode
  (kbd "<localleader> t n") 'cider-toggle-trace-ns
  (kbd "<localleader> t v") 'cider-toggle-trace-var
  (kbd "<localleader> t r") 'cider-test-run-test
  (kbd "<localleader> t p") 'cider-test-run-project-tests
  (kbd "<localleader> t N") 'cider-test-run-ns-tests)

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

(evil-set-initial-state 'cider-browse-ns-mode 'motion)
(evil-define-key 'motion cider-browse-ns-mode-map
  (kbd "RET") 'cider-browse-ns-operate-at-point
  "q" 'cider-popup-buffer-quit-function)

(evil-set-initial-state 'cider-inspector-mode 'motion)
(evil-define-key 'motion cider-inspector-mode-map
  (kbd "RET") 'cider-inspector-operate-on-point
  (kbd "DEL") 'cider-inspector-pop
  "gr" 'cider-inspector-refresh)

(evil-set-initial-state 'cider-test-report-mode 'motion)
(evil-define-key 'motion 'cider-test-report-mode-map
  (kbd "TAB") 'cider-test-next-result
  (kbd "<backtab>") 'cider-test-previous-result
  (kbd "}") 'cider-test-next-result
  (kbd "{") 'cider-test-previous-result
  "gd" 'cider-test-jump
  "gs" 'cider-test-stacktrace)

;; TODO cider--debug-mode-map
;; TODO cider-macroexpansion-mode-map

(push '(clojure-mode
	"<localleader>e" "eval"
	"<localleader>h" "help"
	"<localleader>s" "repl"
	"<localleader>t" "toggle"
	)
      lesser-evil-keys-desc)
