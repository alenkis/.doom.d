;;; settings/setup-purescript.el -*- lexical-binding: t; -*-

(add-hook 'purescript-mode-hook 'inferior-psci-mode)
(require 'repl-toggle)
(require 'psci)
(add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))

(defun purescript-format-on-save ()
  (interactive)
  (let ((file (buffer-file-name)))
    (shell-command-to-string (concat "purty --write " file))
    (revert-buffer :ignore-auto :noconfirm)))


(defun setup-purs-format ()
  (when (eq major-mode 'purescript-mode)
    (purescript-format-on-save)))

(add-hook 'after-save-hook 'setup-purs-format)

(add-hook 'purescript-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("--|.*" . font-lock-comment-face)))))



(defun query-pursuit (query &optional info)
  (interactive
   (let ((def (purescript-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Pursuit query (default %s): " def)
                          "Pursuit query: ")
                        nil nil def)
           current-prefix-arg)))
  (let ((browse-url-browser-function 'eww-browse-url))
    (purescript-pursuit query info))

  (map!
   :map purescript-mode-map
   :localleader
   (:prefix ("f" . "format")
    :desc "format buffer" :n "b" #'purescript-format-on-save)
   (:prefix ("o" . "open")
    :desc "pursuit" :n "p" #'query-pursuit)))
