;;; settings/setup-haskell.el -*- lexical-binding: t; -*-

(map!
 :map haskell-mode-map
 :localleader
 (:prefix ("r" . "repl")
  :desc "open" :n "o" #'+haskell/open-repl
  :desc "load" :n "l" #'haskell-process-load-file)
 (:prefix ("f" . "format")
  :desc "format buffer" :n "b" #'haskell-mode-stylish-buffer))

(defun setup-haskell-mode ()
  (interactive)
  (setq haskell-stylish-on-save t))

(add-hook 'haskell-mode-hook #'setup-haskell-mode)

(add-to-list '+lookup-provider-url-alist '("Hoogle" "https://hoogle.haskell.org/?hoogle=%s"))
(add-to-list '+lookup-provider-url-alist '("Clojuredocs" "https://clojuredocs.org/clojure.core/%s"))
