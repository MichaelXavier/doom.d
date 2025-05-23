;;; r_config.el -*- lexical-binding: t; -*-

(use-package! ess
  :commands (R r-mode R-mode ess-mode)
  :config
  (defun my-inferior-ess-init ()
    "Workaround for https://github.com/emacs-ess/ESS/issues/1193"
    (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter -90 t)
    (setq-local ansi-color-for-comint-mode nil))
  (add-hook 'inferior-ess-mode-hook #'my-inferior-ess-init)
  )

(use-package! apheleia
  :config
  (push '(r-styler . ("Rscript" "-e" "input <- readLines(file(\"stdin\")); local({tempfile <- withr::local_tempfile(lines=input, fileext = \".R\"); withr::with_options(list(styler.quiet=TRUE),  {styler::style_file(tempfile)}); cat(readLines(tempfile), sep = \"\\n\")})"))
        apheleia-formatters)
  (push '(ess-r-mode . r-styler)
        apheleia-mode-alist))
