;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

(package! counsel)
(package! counsel-tramp)
(package! ormolu)
(package! default-text-scale)
(package! iedit)
;; drop doom default snippets
(package! doom-snippets :ignore t)
(package! string-inflection)
;;TODO: would be good to figure out how to make these conditional
(package! exwm)
;; TODO: doesn't work (package! exwm-config)
(package! xelb)

;; tiling window manager
;; This is my fork of another fork (just in case the original fork evaporates) which may fix some issues
(package! edwina
  :recipe (:host github :repo "MichaelXavier/edwina"))

;; Extract text fields into an emacs buffer for editing
(package! exwm-edit)

(package! memory-usage)

;; HTTP request library with babel integration
(package! verb)

;; Graphviz mode for .dot files
(package! graphviz-dot-mode)

;; Gopher and gemini client
(package! elpher)

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

;; Hash-table tools
(package! ht)

;; Minor mode that sets the background color of color codes to the corresponding color
(package! rainbow-mode)

;; Provides tools for converting between time zones
(package! tzc)

;; Normally the recipe is on the release branch which seems quite old. This gets us haskell support
(unpin! tree-sitter-langs)
(unpin! tree-sitter)

;; Structural editing with tree-sitter. very much a WIP
(package! tree-edit)

;; Major mode for editing jq scripts
(package! jq-mode)

;; Rust pest grammars
(package! pest-mode)
(package! flycheck-pest)

;; Modal editing mode. Trying this in a non-committal way. If it works out
;; there's a whole unofficial doom module for it. TODO
(package! meow)

;; This already seems to be included transitively but I'd rather be specific.
(package! avy)

;; UML tools
(package! plantuml-mode)
(package! flycheck-plantuml)

;; TODO: rolling my own cape integration since the WIP one seems to give me trouble
(package! corfu :recipe (:files ("*.el" "extensions/*.el")))
(package! cape)
(package! dabbrev)
(package! kind-icon)
(package! dired-rsync-transient)
;; Currently pinned one seems to be stuck on 2021 and the most recent change is December 2022
(unpin! dired-rsync)
;; Gitlab support
(package! lab)
;; notifications
;; i've only forked this because the repo is currently on somebody's flaky home server
(package! pushover :recipe (:host github :repo "MichaelXavier/pushover.el" :branch "master" :files ("*.el")))
;; 1password auth source
;; https://dev.to/kamushadenes/1passwordel-integrating-1password-with-emacs-and-auth-source-4nkl
;; TODO: temporarily using my own fork with manual sign in
(package! 1password :recipe (:host github :repo "MichaelXavier/1password.el" :branch "manual-signin" :files ("*.el")))
(package! alert)
(package! jiralib2)
(package! uuidgen)
;; system daemons management
(package! daemons)
;; OpenAPI
(package! gptel)
;;TODO: fork?
(package! gptel-extensions
  :recipe (:host github :repo "kamushadenes/gptel-extensions.el" :branch "main" :files ("*.el")))
;; Refactoring tool. Requires the comby utility to be installed.
(package! comby)
;; Emacs Speaks Statistics. Support for R.
(package! ess)
(package! embark)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist"))
  ;; Temporarily revert until https://github.com/zerolfx/copilot.el/issues/181
  ;; is resolved. In more recent versions, copilot will randomly stop suggesting
  ;; in files until they're reopened.
  :pin "b8a32ff6b9757a84320da003eeb672036d49d91e")

;; Even though I haven't set +crypt on org, it seems to be enabled.
;; org-encrypt-entries runs on save and adds a lot of time to saving large org
;; files.
;; https://github.com/doomemacs/doomemacs/issues/6250
(package! org-crypt :disable t)
