;; custom keymapping
(map!
 "C-w" #'backward-kill-word
 "C-s" #'consult-line
 ;; NOTE: meow can run this with SPC x
 "C-x C-m" #'execute-extended-command
 "C-x C-k" #'kill-region
 "C-S-p" #'edwina-select-previous-window
 "C-S-n" #'edwina-select-next-window
 "C-S-k" #'kill-whole-line
 "C-;" #'iedit-mode
 "C-/" #'undo-fu-only-undo
 "C-?" #'undo-fu-only-redo
 ;; unbind mail compose
 "C-x m" nil)

;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'bu 'browse-url)
(defalias 'ft 'find-tag)

(map! :map goto-map
      ;; This translates to M-g c
      :desc "Avy char" "c" #'avy-goto-char)

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

                                        ; this breaks file-templates expansion
                                        ; (add-hook! 'find-file-hooks #'assume-new-is-modified)

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

;; default to opening a project in dired
;; NOTE: not currently used with vertico
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

;; company mode is really annoying in these modes
(setq company-global-modes
      '(not org-mode erc-mode message-mode help-mode gud-mode vterm-mode sh-mode))

;; spellchecking only used selectively
(remove-hook 'text-mode-hook #'spell-fu-mode)

(add-hook! 'markdown-mode-hook #'spell-fu-mode)

;; suppress annoying TAGS prompt
(setq tags-add-tables nil)

;; configure edwina, a tiling window manager
;; this sets display-buffer to open a new window by default. whatever that means
(setq display-buffer-base-action '(display-buffer-below-selected))
(edwina-mode t)

;; https://github.com/ajgrf/edwina/issues/7#issuecomment-663598457
(defun my/edwina-zoom-and-switch ()
  "Zoom/cycle the selected window to/from master area and then switch to the master window."
  (interactive)
  (if (eq (selected-window) (frame-first-window))
      (edwina-swap-next-window)
    (let ((pane (edwina-pane (selected-window))))
      (edwina-delete-window)
      (edwina-arrange (cons pane (edwina-pane-list)))
      ;; switch to master window
      (select-window (car (edwina--window-list))))))

;; copy edwina's bindings into the doom w workspaces/windows leader prefix.
;; might make sense to have additional super-based bindings for exwm mode? maybe
;; that'll be too confusing.
(map! :map edwina-mode-map
      :leader
      :desc "Rearrange panes" "w r" #'edwina-arrange
      :desc "Move to next window cyclically" "w n" #'edwina-select-next-window
      :desc "Move to previous window cyclically" "w p" #'edwina-select-previous-window
      :desc "Move current window into master area" "w RET" #'my/edwina-zoom-and-switch
      :desc "Clone selected window" "w c" #'edwina-clone-window
      :desc "Delete selected window" "w k" #'edwina-delete-window
      :desc "Grow master window size" "w l" #'edwina-inc-mfact
      :desc "Grow master window size" "w h" #'edwina-dec-mfact
      )



;; Disambiguate auth sources
(setq auth-sources '("~/.authinfo.gpg"))

;; Save bookmarks file on every 1 modification. This is useful because with
;; EXWM, emacs is rarely killed
(setq bookmark-save-flag 1)
;; Set an explicit bookmark pack that can be symlinked in from Dropbox on systems that support it
(setq bookmark-default-file "~/.emacs-bookmarks")


;; Open dired when switching projects
(setq projectile-switch-project-action 'projectile-dired)

;; Allow you to descend into a directory with slash
(map! :map vertico-map
      "/" #'vertico-directory-enter)

;; Consult starts previewing as you type which causes weird stuff to happen on windows like terminal that are in char mode and capture input.
;; Solution is from https://github.com/minad/consult/issues/233#issuecomment-1109006627
(after! consult
  (consult-customize consult-buffer :preview-key nil))


;; Taken and slightly modified from  https://codeberg.org/dalz/dotfiles/src/branch/master/doom/+exwm.el
(defun my/app-launcher ()
  "An app launcher that completes using the executables in PATH (labeled with x) and desktop apps (labeled with d)"
  (interactive)
  (let* ((desktop-apps (mapcar (lambda (c)
                                 (let* ((name (caar c))
                                        (cats (cdar c))
                                        (str (concat name cats)))
                                   (put-text-property 0 (length str) 'display (concat "d: " name) str)
                                   (cons str (cdr c))))
                               (my/desktop-apps)))
         (executables (mapcar (lambda (s)
                                (put-text-property 0 (length s) 'display (concat "x: " s) s) s)
                              (my/executables-in-path)))
         (candidates (append desktop-apps executables))
         (selection (completing-read "Run: " candidates))
         )
    (if (consp selection)
        (counsel-linux-app-action-default selection)
      (start-process-shell-command selection nil selection))
    )
  )
(defun my/executables-in-path ()
  (delete-dups
   (mapcar #'f-filename
           (-filter #'f-executable-p
                    (-flatten
                     (mapcar (lambda (dir)
                               (directory-files dir t directory-files-no-dot-files-regexp))
                             (-filter #'f-directory-p (split-string (getenv "PATH") ":"))))))))

(defun my/desktop-apps ()
  (mapcar (lambda (c)
            (cons (with-temp-buffer
                    (insert-file-contents (cdr c))
                    (cons
                     (progn (re-search-forward "^Name *= *\\(.+\\)$")
                            (match-string 1))
                     (progn (goto-char (point-min))
                            (re-search-forward "^Categories *= *\\(.+\\)$" nil t)
                            (match-string 1))))
                  (car c)))
          (counsel-linux-apps-list-desktop-files)))

;;NOTE: I'm under the impression that using require is discouraged becase it slows load time? I can't seem to force counsel to load though so require it is
(use-package! consult
  :config
  ;; By default consult logs to an invisible buffer. This makes debugging a bit easier
  (setq consult--async-log "*consult-async*")
  ;; Slow down how quickly consult searches re-search.
  (setq consult-async-input-debounce 0.7)
  )

(use-package! counsel)
(use-package! counsel-tramp
  :after (counsel)
  :config
  (defun tramp-find-file ()
    "Prompt for a TRAMP path from your SSH config and then browse that host"
    (interactive)
    (let* ((host (completing-read "Host: " (counsel-tramp--candidates)))
           (default-directory host))
      (find-file)
      )
    )
  )

;; C-c f t to browse files via tramp, replacing counsel-tramp
(map! :map doom-leader-file-map
      :desc "Find file via TRAMP" "t" #'tramp-find-file)

;; Add some online search providers which can be used with C-c s O
(dolist
    (provider
     '(("Hackage" "https://hackage.haskell.org/packages/search?terms=%s")
       ("Hoogle" "https://hoogle.haskell.org/?scope=set:stackage&hoogle=%s")
       ("JustWatch" "https://www.justwatch.com/us/search?q=%s")
       ;;TODO: wiki search, local hoogle
       ))
  (add-to-list '+lookup-provider-url-alist provider))

(defun mx/browse-url-container (container url)
  "Open a URL in the named firefox container"
  (browse-url-firefox (s-lex-format "ext+container:name=${container}&url=${url}"))
  )

(defun mx/well/browse-url (url &rest args)
  "Open a URL in the well container, defaulting to the URL under point"
  (interactive (browse-url-interactive-arg "URL: "))
  (mx/browse-url-container "Well" url)
  )

(defun mx/well/browse-jira-ticket (ticket-number)
  "Open the JIRA ticket with the given ticket number"
  (interactive "sTicket Number: ")
  (mx/well/browse-url (s-lex-format "https://wellco.atlassian.net/browse/${ticket-number}"))
  )

(defun mx/well/memsource-project (project-id)
  "Open the Memsource project with the given project id"
  (interactive (list
                (read-string (format "Project ID (%s): " (thing-at-point 'sexp))
                             nil nil (thing-at-point 'sexp))))
  (mx/well/browse-url (s-lex-format "https://cloud.memsource.com/web/project2/show/${project-id}"))
  )

(map! :leader
      :desc "browse" :prefix ("b" . "browse")
      :desc "Browse URL" "b" #'browse-url
      :desc "Browse Well URL" "w" #'mx/well/browse-url
      :desc "Browse Well JIRA ticket" "j" #'mx/well/browse-jira-ticket
      :desc "Browse Memsource project" "m" #'mx/well/browse-memsource-project
      )

;; Add a few modes that should auto-color hex color codes
(defun mx/enable-rainbow-mode ()
  (rainbow-mode 1))

(add-hook! '(json-mode-hook haskell-mode-hook) 'mx/enable-rainbow-mode)

;; Show a number by company completions, e.g. 1 can be selected with M-1. Unclear if this is going to actually be useful
(setq company-show-quick-access 'left)


;; Set common time zones that i use
(use-package! tzc
  :init
  ;; tzc hardcodes this on linux and it's incorrect on nixos. This has to happen
  ;; before the package is loaded or it will eat shit.
  (setq tzc-main-dir (file-name-as-directory (getenv "TZDIR")))
  :config
  (setq tzc-favourite-time-zones-alist
        '(("America/Los_Angeles" "US_Pacific")
          ("UTC+0000" "UTC")
          ("America/Chicago" "US_Central")
          ("America/New_York" "US_Eastern")
          ("Europe/Istanbul" "Turkey")
          ("Europe/Berlin" "Germany")
          ))
  (map! :leader
        :desc "time zones" :prefix ("z" . "time zones")
        :desc "World clock" "w" #'tzc-world-clock
        )
  )

;; Dired is a little too aggressive with hiding
(setq dired-omit-extensions
      '(".hi"
       ".o"
       "~"
       ".lbin"
       ".so"
       ".a"
       ".ln"
       ".blg"
       ".bbl"
       ".elc"
       ".lof"
       ".glo"
       ".idx"
       ".lot"
       ".svn/"
       ".hg/"
       ".git/"
       ".bzr/"
       "CVS/"
       "_darcs/"
       "_MTN/"
       ".fmt"
       ".tfm"
       ".class"
       ".fas"
       ".lib"
       ".mem"
       ".x86f"
       ".sparcf"
       ".dfsl"
       ".pfsl"
       ".d64fsl"
       ".p64fsl"
       ".lx64fsl"
       ".lx32fsl"
       ".dx64fsl"
       ".dx32fsl"
       ".fx64fsl"
       ".fx32fsl"
       ".sx64fsl"
       ".sx32fsl"
       ".wx64fsl"
       ".wx32fsl"
       ".fasl"
       ".ufsl"
       ".fsl"
       ".dxl"
       ".lo"
       ".la"
       ".gmo"
       ".mo"
       ".toc"
       ".aux"
       ".cp"
       ".fn"
       ".ky"
       ".pg"
       ".tp"
       ".vr"
       ".cps"
       ".fns"
       ".kys"
       ".pgs"
       ".tps"
       ".vrs"
       ".pyc"
       ".pyo"
       ".idx"
       ".lof"
       ".lot"
       ".glo"
       ".blg"
       ".bbl"
       ".cp"
       ".cps"
       ".fn"
       ".fns"
       ".ky"
       ".kys"
       ".pg"
       ".pgs"
       ".tp"
       ".tps"
       ".vr"
       ".vrs"))

;; Taken from https://github.com/meow-edit/meow/blob/master/KEYBINDING_QWERTY.org
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   ;; TODO this is not working right. we need to set it to use C-x C-k to get kill-region
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

;; modal editing with meow. Can practice with meow-tutor
(use-package! meow
  :init
  ;; meow wants C-k to be kill-region but we've overridden that to C-x C-k
  (setq meow--kbd-kill-region "C-x C-k")
  (setq meow--kbd-kill-whole-line "C-S-k")
  :config
  (meow-setup)
  (meow-global-mode 1))

;; Commands seem to be failing and when you bring up the transient buffer it
;; shifts focus to the next window and you're not able to issue commands. This
;; is less usable because it shows a terse one-line summary of the magit commands but seems to work
(setq transient-show-popup nil)
