;; I don't really like intellij type stuff and since most of the time i edit js
;; there won't be a working typescript server or whatever, let's just turn off this annoying stuff
(setq tide-disable-suggestions t)

;; When LSP isn't set up you get these really annoying identifier warnings in your minibuffer every half second
(setq tide-hl-identifier-idle-time nil)

;; Project cleanup also seems to do annoying stuff if there's no server running
;; can we get it to not try to run tsserver on entry?
