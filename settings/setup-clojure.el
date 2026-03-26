;;; settings/setup-clojure.el -*- lexical-binding: t; -*-

(setq cider-inspector-fill-frame t)

;; Fix REPL routing: search all sessions when the current session
;; doesn't have a REPL of the right type (clj/cljs).
;; Works around sesman-current-sessions returning nil.
(after! cider
  (defun +cider/repls-across-sessions (orig-fn &optional type ensure required-ops)
    "Fall back to searching all CIDER sessions for matching REPLs."
    (or (condition-case nil
            (funcall orig-fn type ensure required-ops)
          (user-error nil))
        (let* ((type (cond
                      ((listp type) (mapcar #'cider-maybe-intern type))
                      (t (cider-maybe-intern type))))
               (all-repls (apply #'append
                                 (mapcar #'cdr (sesman-sessions 'CIDER)))))
          (seq-filter (lambda (b)
                        (and (buffer-live-p b)
                             (not (cider-cljs-pending-p b))
                             (cider--match-repl-type type b)))
                      all-repls))))
  (advice-add 'cider-repls :around #'+cider/repls-across-sessions))

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
