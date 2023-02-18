;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

(package! ace-jump-mode)
(package! shm)
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
;; Get a newer version of forge: https://github.com/magit/forge/issues/491
;; This is no longer working
;; (unpin! forge)

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
;; NOTE i'm keeping an eye on combobulate and tree-edit for structural editing. I'd like to eliminate SHM

;; Major mode for editing jq scripts
(package! jq-mode)

;; Rust pest grammars
(package! pest-mode)
(package! flycheck-pest)

;; Modal editing mode. Trying this in a non-committal way. If it works out
;; there's a whole unofficial doom module for it. TODO
(package! meow)
