;;; exwm.el -*- lexical-binding: t; -*-
(require 'exwm-config)
;;(require 'exwm-systemtray) TODO: we'll let mate handle that
;; multi-monitor support
(require 'exwm-randr)

;;TODO: how do we make this conditional?

;; lots taken from https://github.com/johanwiden/exwm-setup

;; Number workspaces to monitors. this is very specifi to my home setup so if i
;; need to plug in somewhere else it's probably gonna suck. I think I only need
;; a workspace per monitor i'm using though
;; (setq exwm-randr-workspace-monitor-plist
;;       (let
;;           ((dell-monitor "DP-3")
;;            (laptop-monitor "eDP-1"))
;;         `(1 ,dell-monitor 2 ,laptop-monitor)))

;;TODO: shold we do server-start?


;; Support for multiple monitors, and plugging and unplugging of monitors.

;; If you have a static setup, i.e. you will not change the screen configuration
;; while emacs is running, then you do not need to
;; define jw/exwm-change-screen-hook.

;; If you are going to use more than one screen at the same time, you need to
;; define exwm-randr-workspace-monitor-plist, and call "(exwm-randr-enable)".
;; "(exwm-randr-enable)" must also be called if you use
;; exwm-randr-screen-change-hook.


;;TODO: extract? this is a lot of stuff
(defun jw/env-list (env-string)
  "Return list of strings in environment variable env-string.
  nil if empty or undefined."
  (let ((env-var (getenv env-string)))
    (if env-var
        (split-string env-var)
      nil)))
(defun jw/env-str (env-string)
  "Return string in environment variable env-string.
  nil if empty or undefined."
  (let ((env-var (getenv env-string)))
    (if (> (length env-var) 0)
        env-var
      nil)))

(defun jw/build-workspace-monitor-plist (list)
  (let (transformed-list first second (rev-list (reverse list)))
    (while rev-list
      (setq second (car rev-list))
      (setq first (string-to-number (car (cdr rev-list))))
      (setq transformed-list (cons first (cons second transformed-list)))
      (setq rev-list (cdr (cdr rev-list)))
      )
    transformed-list))

(defun jw/xrandr-output-list ()
  "Return list of connected X11 screens, according to xrandr."
  (interactive)
  (let* ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
         (find-outputs
          (lambda ()
            (let (output-list)
              (call-process "xrandr" nil t nil)
              (goto-char (point-min))
              (while (re-search-forward xrandr-output-regexp nil 'noerror)
                (setq output-list (cons (match-string 1) output-list))
                (forward-line))
              (reverse output-list))))
         (output-list (with-temp-buffer
                        (funcall find-outputs))))
    output-list))

(setq jw/x11-screen-list (jw/env-list "X11_SCREEN_LIST"))
(setq jw/x11-screen-order-list (jw/env-list "X11_SCREEN_ORDER_LIST"))
(setq jw/x11-screen-mode-list (jw/env-list "X11_SCREEN_MODE_LIST"))
(setq jw/x11-screen-rate-list (jw/env-list "X11_SCREEN_RATE_LIST"))
(setq jw/x11-screen-disabled-list (jw/env-list "X11_SCREEN_DISABLED_LIST"))
(setq jw/exwm-workspace-list (jw/env-list "EXWM_WORKSPACE_LIST"))
(setq jw/x11-screen-preferred (jw/env-str "X11_SCREEN_PREFERRED"))
(setq jw/x11-display-dpi (jw/env-str "X11_DISPLAY_DPI"))
(let ((env-var (getenv "X11_SCREEN_USE_ALL_AVAILABLE")))
  (setq jw/x11-screen-use-all-available
        (if (and (> (length env-var) 0) (string= "yes" env-var))
            t
          nil)))

(setq exwm-randr-workspace-monitor-plist (jw/build-workspace-monitor-plist jw/exwm-workspace-list))

(defun jw/exwm-change-screen-hook ()
  "Execute xrandr to select and position available screens according to X11_SCREEN_* environment variables."
  (let* ((output-list (jw/xrandr-output-list))
         (available-screens (seq-intersection jw/x11-screen-list output-list))
         (available-order-screens (seq-intersection jw/x11-screen-order-list output-list))
         ;; See "--auto" in xrandr(1) and https://github.com/ch11ng/exwm/issues/529.
         (unavailable-screens (seq-difference jw/x11-screen-list output-list))
         (available-disabled-screens (seq-intersection jw/x11-screen-disabled-list output-list))
         (available-screen-modes
          (let (mode-list
                mode screen
                (x-screen-list jw/x11-screen-list)
                (x-mode-list jw/x11-screen-mode-list))
            (while x-screen-list
              (setq screen (car x-screen-list))
              (setq x-screen-list (cdr x-screen-list))
              (setq mode (car x-mode-list))
              (setq x-mode-list (cdr x-mode-list))
              (if (seq-contains available-screens screen)
                  (setq mode-list (cons mode mode-list))))
            (reverse mode-list)))
         (available-screen-rates
          (let (rate-list
                rate screen
                (x-screen-list jw/x11-screen-list)
                (x-rate-list jw/x11-screen-rate-list))
            (while x-screen-list
              (setq screen (car x-screen-list))
              (setq x-screen-list (cdr x-screen-list))
              (setq rate (car x-rate-list))
              (setq x-rate-list (cdr x-rate-list))
              (if (seq-contains available-screens screen)
                  (setq rate-list (cons rate rate-list))))
            (reverse rate-list))))
    (if available-screens
        ;; Start building xrandr command line
        (let* ((x-primary-screen
                (if (and jw/x11-screen-preferred (seq-contains available-screens jw/x11-screen-preferred))
                    jw/x11-screen-preferred
                  (car available-screens)))
               (screen-pos (seq-position available-screens x-primary-screen))
               (x-primary-mode (elt available-screen-modes screen-pos))
               (x-primary-rate (elt available-screen-rates screen-pos))
               (xrandr-dpi-args
                (if jw/x11-display-dpi
                    (list jw/x11-display-dpi "--dpi")))
               (xrandr-primary-args (list x-primary-rate "--rate" x-primary-mode "--mode" "--primary" x-primary-screen "--output"))
               screen
               disabled-list
               (xrandr-disabled-args
                (progn
                  (while available-disabled-screens
                    (setq screen (car available-disabled-screens))
                    (setq available-disabled-screens (cdr available-disabled-screens))
                    (setq disabled-list (cons "--output" disabled-list))
                    (setq disabled-list (cons screen disabled-list))
                    (setq disabled-list (cons "--off" disabled-list)))
                  disabled-list))
               (unavailable-screen-list unavailable-screens)
               u-s-list
               (xrandr-unavailable-screen-args
                (progn
                  (while unavailable-screen-list
                    (setq screen (car unavailable-screen-list))
                    (setq unavailable-screen-list (cdr unavailable-screen-list))
                    (setq u-s-list (cons "--output" u-s-list))
                    (setq u-s-list (cons screen u-s-list))
                    ;; (setq u-s-list (cons "--auto" u-s-list))
                    (setq u-s-list (cons "--off" u-s-list)))
                  u-s-list))
               (screen-list available-screens)
               rest-list
               (xrandr-rest-available-screen-args
                (if jw/x11-screen-use-all-available
                    ;; Add remaining available screens, except the primary screen
                    (progn
                      (while screen-list
                        (setq screen (car screen-list))
                        (setq screen-list (cdr screen-list))
                        (if (not (string= screen x-primary-screen))
                            (progn
                              (setq rest-list (cons "--output" rest-list))
                              (setq rest-list (cons screen rest-list))
                              (setq rest-list (cons "--mode" rest-list))
                              (setq rest-list (cons (elt available-screen-modes (seq-position available-screens screen)) rest-list))
                              (setq rest-list (cons "--rate" rest-list))
                              (setq rest-list (cons (elt available-screen-rates (seq-position available-screens screen)) rest-list)))))
                      rest-list)
                  ;; Disable remaining available screens, except the primary screen
                  (progn
                    (while screen-list
                      (setq screen (car screen-list))
                      (setq screen-list (cdr screen-list))
                      (if (not (string= screen x-primary-screen))
                          (progn
                            (setq rest-list (cons "--output" rest-list))
                            (setq rest-list (cons screen rest-list))
                            (setq rest-list (cons "--off" rest-list)))))
                    rest-list)))
               (screen-order-list available-order-screens)
               order-list
               left-screen
               (xrandr-screen-order-args
                (if (and jw/x11-screen-use-all-available
                         (> (length screen-order-list) 1))
                    (progn
                      (setq left-screen (car screen-order-list))
                      (setq screen-order-list (cdr screen-order-list))
                      (while screen-order-list
                        (setq screen (car screen-order-list))
                        (setq screen-order-list (cdr screen-order-list))
                        (setq order-list (cons "--output" order-list))
                        (setq order-list (cons screen order-list))
                        (setq order-list (cons "--right-of" order-list))
                        (setq order-list (cons left-screen order-list))
                        (setq left-screen screen))
                      (reverse order-list))))
               (xrandr-args (reverse (append xrandr-rest-available-screen-args xrandr-unavailable-screen-args
                                             xrandr-disabled-args xrandr-primary-args xrandr-dpi-args))))
          (progn
            (setq jw/debug-output-list output-list)
            (setq jw/debug-xrandr-args xrandr-args)
            (setq jw/debug-xrandr-order-args xrandr-screen-order-args)
            (apply #'call-process
                   "xrandr" nil nil nil
                   xrandr-args)
            (if xrandr-screen-order-args
                (apply #'call-process
                       "xrandr" nil nil nil
                       xrandr-screen-order-args)))
          )
      )
    )
  )

;; this doesn't seem to work?
(add-hook 'exwm-randr-screen-change-hook 'jw/exwm-change-screen-hook)
;;TODO: is this making x windows float?

;; Emacs as a window manager
;; (exwm-config-default) ;;TODO: get a full config TODO will it work without?
;;(exwm-systemtray-enable)
(exwm-randr-enable)


;; keybindings
(unless (get 'exwm-input-global-keys 'saved-value)
  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode).
          (,(kbd "s-r") . exwm-reset)
          ;; 's-w': Switch workspace. TODO: keep?
          (,(kbd "s-w") . exwm-workspace-switch)
          ;; 's-p': Launch application.
          (,(kbd "s-p") . counsel-linux-app)
          ;; Toggle between "line-mode" and "char mode"
          (,(kbd "s-i") . exwm-input-toggle-keyboard)
          ;; Panic-button that resets EXWM to a sane mode for just the current
          ;; window
          (,(kbd "s-R") . exwm-reset)
          )))

;;TOOD: exwm-input-simulation-keys
;;TODO: edwina
;;TODO: telephone-line stuff

;; this always has to come last evidently
;;
(exwm-enable) ;; this is "harmless"
;; but logs an annoying warning on boot when exwm is not enabled

;;TODO: how do we get this to work when x windows are in focus?
;;TODO: capture bindings for window movement that overrides x, e.g. C-S-n and C-S p
;;TODO; bind xmonad like bindings for s-w and s-e which runs something like (exwm-workspace-switch-create 2)
;;TODO: probably a way to switch to named tmux sessions? it would be nice if the buffer inherited it somehow
;;TODO: investigate char mode. this seems like a good way to not have it intercept characters
