;;; settings/setup-clojure.el -*- lexical-binding: t; -*-

(use-package! flycheck-clj-kondo
  :after clojurescript-mode)

(defun setup-clojure ()
  (interactive)
  (setq cider-inspector-fill-frame t))

(add-hook 'clojure-mode-hook 'setup-clojure)
(add-hook 'clojurescript-mode 'setup-clojure)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

;; Use clojure-lsp's built-in cljfmt for format-on-save
(setq-hook! 'clojure-mode-hook +format-with 'lsp)
(setq-hook! 'clojurescript-mode-hook +format-with 'lsp)
(setq-hook! 'clojurec-mode-hook +format-with 'lsp)

;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(map!
 :map clojure-mode-map
 :localleader
 (:prefix ("f" . "paredit")
  :desc "slurp" :n "s" #'paredit-forward-slurp-sexp
  :desc "barf" :n "b" #'paredit-forward-barf-sexp
  :desc "forward" :n "<right>" #'paredit-forward
  :desc "backward" :n "<left>" #'paredit-backward
  :desc "up" :n "<up>" #'paredit-forward-up
  :desc "down" :n "<down>" #'paredit-backward-down
  :desc "forward to matching sexp" :n "l" #'forward-sexp
  :desc "backward to matching sexp" :n "h" #'backward-sexp))
