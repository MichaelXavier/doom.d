;; custom keymapping
(map!
 "C-w" #'backward-kill-word
 "C-s" #'swiper
 "C-x C-m" #'execute-extended-command
 "C-x C-k" #'kill-region
 "C-S-p" #'previous-multiframe-window
 "C-S-n" #'next-multiframe-window
 "C-c s" #'counsel-tramp
 "C-S-k" #'kill-whole-line
 "C-;" #'iedit-mode)

;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'bu 'browse-url)
(defalias 'ft 'find-tag)

;; smartparens bindings
;; https://github.com/Fuco1/smartparens/wiki/Example-configuration
(map! :map smartparens-mode-map
      "C-M-f" #'sp-forward-sexp
      "C-M-b" #'sp-backward-sexp
      "C-M-d" #'sp-down-sexp
      "C-M-a" #'sp-backward-down-sexp
      "C-S-a" #'sp-beginning-of-sexp
      "C-S-d" #'sp-end-of-sexp
      "C-M-e" #'sp-up-sexp
      "C-M-u" #'sp-backward-up-sexp
      "C-M-t" #'sp-transpose-sexp
      "C-M-n" #'sp-next-sexp
      "C-M-p" #'sp-previous-sexp
      "C-M-k" #'sp-kill-sexp
      "C-M-w" #'sp-copy-sexp
      "M-<delete>" #'sp-unwrap-sexp
      "M-<backspace>" #'sp-backward-unwrap-sexp
      "C-<right>" #'sp-forward-slurp-sexp
      "C-<left>" #'sp-forward-barf-sexp
      "C-M-<left>" #'sp-backward-slurp-sexp
      "C-M-<right>" #'sp-backward-barf-sexp
      "M-D" #'sp-splice-sexp
      "C-M-<delete>" #'sp-splice-sexp-killing-forward
      "C-M-<backspace>" #'sp-splice-sexp-killing-backward
      "C-S-<backspace>" #'sp-splice-sexp-killing-around
      "C-]" #'sp-select-next-thing-exchange
      "C-<left_bracket>" #'sp-select-previous-thing
      "C-M-]" #'sp-select-next-thing
      "M-F" #'sp-forward-symbol
      "M-B" #'sp-backward-symbol
      "H-t" #'sp-prefix-tag-object
      "H-p" #'sp-prefix-pair-object
      "H-s c" #'sp-convolute-sexp
      "H-s a" #'sp-absorb-sexp
      "H-s e" #'sp-emit-sexp
      "H-s p" #'sp-add-to-previous-sexp
      "H-s n" #'sp-add-to-next-sexp
      "H-s j" #'sp-join-sexp
      "H-s s" #'sp-split-sexp
      "C-c SPC" #'ace-jump-mode
      "C-+" #'default-text-scale-increase
      "C--" #'default-text-scale-decrease
      "C-0" #'default-text-scale-reset)

(map! :map emacs-lisp-mode-map
      ")" #'sp-up-sexp)

;; managing windows
(map! :map winner-mode-map
      "C-M-S-<left>" #'winner-undo
      "C-M-S-<right>" #'winner-redo)

;; Previously hitting escape 3 times focuses the current window and
;; closes the others. I almost always hit it by accident since my
;; escape and ctrl are the same key
(global-unset-key "\e\e\e")

;; Remove the whole line when you hit Ctrl-K instead of just clearing it
(setq kill-whole-line t)

;; assume all new files are modified so you can save them
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

(add-hook! 'find-file-hooks #'assume-new-is-modified)

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; Silly function that comes in handy once in a blue moon
(defun reverse-region (start end)
  "region to reverse"
  (interactive "r")
  (let ((reversed (concat (reverse (string-to-list (buffer-substring-no-properties start end))))))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert reversed) )) )

(defun save-buffer-always ()
  "Save the buffer even if it is not modified. Useful for triggering file-watching tools."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(map! "C-x C-s" #'save-buffer-always)

(map! "C-z" #'ignore)

;; Set C-c C-p as the projectile command prefix
(map! :map projectile-mode-map
      "C-c C-p" #'projectile-command-map)

;;TODO: is this working?
;; dumb-jump
;; if you run C-u C-M-g (universal argument), it will run the prompt version of dumb-jump-go
(defun custom-dumb-jump-go (&optional _)
  "Delegates to dumb-jump-go. If given the universal argument, runs it with a prompt"
  (interactive "P")
  ;; If universal argument given, prompt
  (if current-prefix-arg
      (dumb-jump-go-prompt)
    (dumb-jump-go)))

(map!
 ;; Bind our customized dumb jump function
 "C-M-g" #'custom-dumb-jump-go)

(map! :map dumb-jump-mode-map
      ;; unbind dumb-jump-back which i always accidentally hit
      "C-M-p" nil)

;; configure auto formatters
(setq +format-on-save-enabled-modes
      '(not haskell-mode))

(after! smart-tab
  (global-smart-tab-mode t))

;; toggle god mode with esc. ergos aren't great on laptop keyboard
(global-set-key (kbd "<escape>") 'god-mode-all)

;; default to opening a project in dired
(setq counsel-projectile-switch-project-action
      '(4
        ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
        ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
        ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
        ("D" counsel-projectile-switch-project-action-dired "open project in dired")
        ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
        ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
        ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
        ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
        ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
        ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
        ("C" counsel-projectile-switch-project-action-configure "run project configure command")
        ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
        ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
        ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
        ("si" counsel-projectile-switch-project-action-git-grep "search project with git grep")
        ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
        ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
        ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
        ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
        ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
        ("xv" counsel-projectile-switch-project-action-run-vterm "invoke vterm from project root")
        ("Oc" counsel-projectile-switch-project-action-org-capture "capture into project")
        ("Oa" counsel-projectile-switch-project-action-org-agenda "open project agenda")))
