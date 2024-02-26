;;TODO: something about commenting out this file causes a hang?
(after! haskell-mode
  (map! :map haskell-mode-map
        "C-c C-f" #'haskell-mode-stylish-buffer
        "C-c C-o" #'ormolu-format-buffer
        ;; unmap haskell-mode-toggle-scc-at-point because i don't use it and accidentally hit it all the time
        "C-c C-s" nil
        )
  (add-hook! 'haskell-mode-hook
             ;; these checkers are dog slow. not sure if this is the right way
             (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)
             (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
             (add-to-list 'flycheck-disabled-checkers 'haskell-hlint)
             ;; there is no +tree-sitter flag for the haskell doom-emacs lang module yet
             (tree-sitter!)

             (setq haskell-auto-insert-module-format-string
                   "module %s\n    ( \n    ) where")
             (setq ormolu-extra-args
                   '("--ghc-opt" "-XTypeApplications" "--ghc-opt" "-XBangPatterns" "--ghc-opt" "-XTemplateHaskell" "--ghc-opt" "-XImportQualifiedPost" "--ghc-opt" "-XOverloadedLabels"))

             ;;TODO: is this anything? i don't know why you'd do this or why cape-dabbrev doesn't fuckin work in haskell-mode
             ;; it kinda works but all the suggestions are downcased?
             ;; (setq-local completion-at-point-functions
             ;;             (mapcar #'cape-company-to-capf
             ;;                     (list #'company-dabbrev)))
             )
  )

;; ;; So smartparens doesn't get confused at language pragmas
(after! smartparens
  (sp-with-modes 'haskell-mode
    (sp-local-pair "{-#" "#-}")
    )
  )

;;TODO: can this be turned off?
;; (after! (:and haskell-mode company)
;;   ;; Try snippets first, failing that, terms found in buffers, then fallback. Can turn this off if we end up using corfu
;;   (set-company-backend! 'haskell-mode-hook '(company-yasnippet company-dabbrev-code company-capf))
;;   )

;; TODO: is this helpful??
;; turn off lsp to avoid annoying error. might break LSP but i don't use it currently
;; (remove-hook! 'haskell-mode-local-vars-hook #'lsp!)
