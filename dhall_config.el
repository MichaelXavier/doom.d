;; dhall files use camelcase and tend to benefit from subword movement
(add-hook! 'dhall-mode-hook #'subword-mode)


;; disable buffer type, which just slows things down
(setq dhall-use-header-line nil)
