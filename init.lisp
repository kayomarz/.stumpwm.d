(set-prefix-key (kbd "s-SPC"))



;; --- useful bindings ---------------------------------------------------------

;; Apart from defining custom bindings, undefine some default bindings.

(undefine-key *root-map* (kbd "e"))  ; "emacs"
(undefine-key *root-map* (kbd "C-e")) ; "emacs"
(define-key *top-map* (kbd "s-e") "emacs")

(undefine-key *root-map* (kbd "c")) ; "exec xterm"
(undefine-key *root-map* (kbd "C-c")) ; "exec xterm"
(define-key *top-map* (kbd "s-s")  "run-or-raise-xterm")

(define-key *top-map* (kbd "s-t") "run-or-raise-thunar")
(define-key *top-map* (kbd "s-T") "exec thunar")
(define-key *top-map* (kbd "s-f") "run-or-raise-firefox")
(define-key *top-map* (kbd "s-c")  "run-or-raise-chrome")
(define-key *top-map* (kbd "Print") "exec xfce4-screenshooter")

(defcommand run-or-raise-xterm () ()
  (run-or-raise "xterm" '(:class "XTerm")))

(defcommand run-or-raise-thunar () ()
  (run-or-raise "thunar" '(:class "Thunar")))

(defcommand run-or-raise-firefox () ()
  (run-or-raise "firefox" '(:class "firefox-esr")))

(defcommand run-or-raise-chrome () ()
  (run-or-raise "google-chrome" '(:class "Google-chrome")))



;; --- module - winner mode ----------------------------------------------------

(load-module "winner-mode")
(defvar *winner-map* (make-sparse-keymap))
(define-key *winner-map* (kbd "Left") "winner-undo")
(define-key *winner-map* (kbd "Right") "winner-redo")
(define-key *top-map* (kbd "s-x") '*winner-map*)

(add-hook *post-command-hook*
          (lambda (command)
            (when (member command winner-mode:*default-commands*)
              (winner-mode:dump-group-to-file))))



;; --- volume ------------------------------------------------------------------

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "vol+")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "vol-")
(define-key *top-map* (kbd "XF86AudioMute") "toggle-mute")

(defun pamixer (&rest strings)
  (let* ((stdout
           (run-shell-command (format nil "pamixer ~{~A~^ ~}" strings) t))
         (ok (> (length stdout) 0)))
    (if ok
        stdout
        (error "Could not run pamixer, make sure it is installed"))))

(defcommand vol+ () ()
  (message "Volume ~A"
           (pamixer "--increase 10 --unmute --get-volume-human")))

(defcommand vol- () ()
  (message "Volume ~A"
           (pamixer "--decrease 10 --get-volume-human")))

(defcommand toggle-mute () ()
  (let ((mutedp (cl-ppcre:scan-to-strings
                 "true"
                 (pamixer "--toggle-mute --get-mute"))))
    (message "Volume ~A"
             (if mutedp
                 "Muted"
                 (pamixer "--get-volume")))))


;; --- module - xinput-toggle (touchpad on/off) --------------------------------

(load-module "xinput-toggle")
(define-key *top-map* (kbd "XF86TouchpadToggle")
  "xinput-toggle-devices touchpad")
(xinput-toggle:xinput-disable-devices "touchpad")

;; --- StumpWM with SLIME ------------------------------------------------------

;; https://www.kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
