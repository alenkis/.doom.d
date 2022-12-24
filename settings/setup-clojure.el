;;; settings/setup-clojure.el -*- lexical-binding: t; -*-

(use-package! flycheck-clj-kondo
  :after clojurescript-mode)

(defun setup-clojure ()
  (interactive)
  (progn
    (add-hook 'before-save-hook 'cider-format-buffer t t)
    (setq cider-inspector-fill-frame t)
    (setq lsp-mode t)))

(add-hook 'clojure-mode-hook 'setup-clojure)
(add-hook 'clojurescript-mode 'setup-clojure)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

(map! :after cider
      :map clojure-mode-map
      :localleader
      (:desc "eval" :prefix "e"
       :desc "send last sexp to REBL" :n "E" #'rebl-eval-last-sexp
       :desc "defn at point to REBL" :n "P" #'rebl-eval-defun-at-point))

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
  :desc "backward to matching sexp" :n "h" #'backward-sexp
  ;; nested prefix
  ;; (:prefix ("f" . "forward")
  ;;  :desc "slurp" :n "s" #'paredit-forward-slurp-sexp)
  ))
