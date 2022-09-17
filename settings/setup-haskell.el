;;; settings/setup-haskell.el -*- lexical-binding: t; -*-

(map!
 :map haskell-mode-map
 :localleader
 (:prefix ("r" . "repl")
  :desc "open" :n "o" #'+haskell/open-repl
  :desc "load" :n "l" #'haskell-process-load-file)
 (:prefix ("f" . "format")
  :desc "format buffer" :n "b" #'ormolu-format-buffer))

(defun setup-haskell-mode ()
  (interactive)
  (ormolu-format-on-save-mode +1))

(add-hook 'haskell-mode-hook #'setup-haskell-mode)

(use-package! ormolu
   :hook (haskell-mode . ormolu-format-on-save-mode))

(add-to-list '+lookup-provider-url-alist '("Hoogle" "https://hoogle.haskell.org/?hoogle=%s"))
(add-to-list '+lookup-provider-url-alist '("Clojuredocs" "https://clojuredocs.org/clojure.core/%s"))
