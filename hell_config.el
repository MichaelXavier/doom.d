;;; hell_config.el -*- lexical-binding: t; -*-

(require 'treesit)
(require 'flycheck)

;; (defgroup flycheck-hell nil
;;   "Support for the Hell syntax checker in Flycheck."
;;   :prefix "flycheck-hell-"
;;   :group 'flycheck)

;; (defcustom flycheck-hell-executable "hell"
;;   "Path to the Hell executable."
;;   :type 'string
;;   :group 'flycheck-hell)

(define-derived-mode hell-mode haskell-mode "Hell"
  "Major mode for editing Hell files.")

;; File association
(add-to-list 'auto-mode-alist '("\\.hell\\'" . hell-mode))

(defun hell-error-parser
    (error-string checker buffer)
  (let ((type-error (rx "hell: "
                        (group (one-or-more anything))
                        "arising from "
                        (group (one-or-more anything))
                        ":"
                        ;; (one-or-more anything)
                        (group (one-or-more digit))
                        ":"
                        (group (one-or-more digit))
                        line-end
                        ))
        (parse-error (rx "hell: "
                         (group (one-or-more not-newline))
                         ))
        (errors))

    (cond ((string-match type-error error-string)
           (push (flycheck-error-new :message (match-string 1 error-string)
                                     :buffer buffer
                                     :checker checker
                                     :filename (match-string 2 error-string)
                                     :line (string-to-number (match-string 3 error-string))
                                     :column (string-to-number (match-string 4 error-string))
                                     :level 'error
                                     :group "type-error"
                                     )
                 errors)
           )
          ((string-match parse-error error-string)
           (push (flycheck-error-new :message (match-string 1 error-string)
                                     :buffer buffer
                                     :checker checker
                                     :level 'error
                                     :group "parse-error"
                                     ;; NOTE: the parse error does not give us file location info but it's still required
                                     :filename (buffer-file-name buffer)
                                     :line 1
                                     :column 1
                                     )
                 errors)
           )
          )
    (nreverse errors)
    )
  )

;; Flycheck integration. Set flycheck-hell-executable to override the path of hell
(flycheck-define-checker hell
  "A Hell checker using hell --check."
  :command ("hell" "--check" source)
  :standard-input nil
  :error-parser hell-error-parser
  :modes hell-mode)

(add-to-list 'flycheck-checkers 'hell)
(add-hook 'hell-mode-hook 'flycheck-mode)

(add-to-list 'tree-sitter-major-mode-language-alist '(hell-mode . haskell))

(provide 'hell-mode)

;;; hell-mode.el ends here
