;;; settings/setup-elm.el -*- lexical-binding: t; -*-

;; (defun setup-elm-mode ()
;;   (interactive)
;;   (elm-format-on-save-mode +1))

;; (add-hook 'elm-mode-hook #'setup-elm-mode)

(add-hook 'elm-mode-hook 'elm-format-on-save-mode)

(map!
 :after elm-mode
 :map elm-mode-map
 :localleader
 (:prefix ("r" . "repl")
  :desc "Load REPL" :n "l" #'elm-repl-load
  :desc "Push to REPL" :n "p" #'elm-repl-push))
