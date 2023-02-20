;; i still think this is what shm/copy-region is supposed to do. copy
;; the node but don't remove it
;; (defun shm/copy-node ()
;;   "Copy the current node."
;;   (interactive)
;;   (shm-kill-node nil nil nil t))

(after! haskell
  (map! :map haskell-mode-map
        "C-c C-f" #'haskell-mode-stylish-buffer
        "C-c C-o" #'ormolu-format-buffer))

;; (after! shm
;;   (map! :map shm-map
;;         "C-w" #'shm/backward-kill-word
;;         "C-M-w" #'shm/copy-node)
;;   ;; Make it easier to toggle shm when it shits the bed
;;   (defalias 'shm 'structured-haskell-mode))

;; haskell files use camelcase and tend to benefit from subword movement
(add-hook! 'haskell-mode-hook #'subword-mode)
(add-hook! 'haskell-cabal-mode-hook #'subword-mode)

;; use shm by default in haskell files
;; (add-hook! 'haskell-mode-hook #'structured-haskell-mode)

;; So smartparens doesn't get confused at language pragmas
(sp-local-pair 'haskell-mode "{-#" "#-}")

;;TODO: does this stll work?
(setq haskell-auto-insert-module-format-string
      "module %s\n    ( \n    ) where")

(setq ormolu-extra-args
      '("--ghc-opt" "-XTypeApplications" "--ghc-opt" "-XBangPatterns" "--ghc-opt" "-XTemplateHaskell"))

;; Try snippets first, failing that, terms found in buffers, then fallback
(set-company-backend! 'haskell-mode-hook '(company-yasnippet company-dabbrev-code company-capf))


(add-hook! 'haskell-mode-hook
  ;; these checkers are dog slow. not sure if this is the right way
  (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)
  (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint)
  ;; there is no +tree-sitter flag for the haskell doom-emacs lang module yet
  (tree-sitter!)
  )

;; turn off lsp to avoid annoying error. might break LSP but i don't use it currently
(remove-hook! 'haskell-mode-local-vars-hook #'lsp!)
