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
  (run-shell-command (format nil "pamixer ~{~A~^ ~}" strings) t))

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



;; --- Touchpad enable/disable -------------------------------------------------

;; We use prefix `xit` (xinput toggle) to reduce name conflicts in the
;; stumpw-user namespace.

(defvar *case-insensitive-mode* t
  "Device names are specified via regex. This variable decides if the
  regex match should be case insensitive or not.")


(defvar *exclude-keyboards* t
  "When using a regex to select devices to turn on or off, this variable
guards against accidental disabling of keyboards (xinput devices with
\"keyboard\" in their name).  Passing no regex (because it is optional) match
all devices including the keyboards. Disabling keyboards may render StumpWM
unusable.

By default, this variable is set to T and if a regex matches device names
containing \"keyboard\" they will be excluded from the set of devices being
toggled (on/off).

When set to NIL, the guard is removed and now if a regex matches device names
containing \"keyboard\" they will be part of the set of devices being toggled
(on/off). Hence all keboards are at the risk of being disabled if your regex
includes them.  Incase all keyboards get disabled, the system might need to be
restarted in order to enable the keyboards again.

Use with caution.

ps: To test what device names matche a regex, invoke the function `get-devices`
passing in an optional regex.  It only returns device names but does not take
any action to enable or disable them, acting as a playground to see what devices
get selected.")

(defconstant *xinput-line-whites*
  (list #\Space #\Tab #\Newline #\Return #\Linefeed #\Page)
  "Characters considered as whitespace when parsing an xinput line.")

(defclass device ()
  ((id :reader device-id :initarg :id)
   (name :reader device-name :initarg :name)
   (info :reader device-info :initarg :info)))

(defmethod name-matches-p ((d device) regex)
  "Returns T if device name matches REGEX, else returns NIL."
  (let ((scanner (ppcre:create-scanner
                  regex :case-insensitive-mode *case-insensitive-mode*)))
    (when (cl-ppcre:scan-to-strings scanner (device-name d))
      t)))

(defmethod is-keyboard-p ((d device))
  "Returns T if device name contains string \"keyboard\", else returns NIL."
  (name-matches-p d "keyboard"))

(defun make-device-from-line (line)
  "Make a device from the line representing a device as displayed by the stdout
  of the `xinput --list` command .

An example of a line representing a device is:
\"FTCS1000:00 2808:0101 Touchpad\" \"13\" \"[floating slave]\""
  (let ((parts (multiple-value-bind (_ parts)
                   (ppcre:scan-to-strings
                    "^[^\\w]*(.*)id\\s*=\\s*(\\d*)(.*)"
                    line)
                 (declare (ignore _))
                 (map 'vector
                      (lambda (s) (string-trim *xinput-line-whites* s))
                      parts))))
    (make-instance 'device :name (elt parts 0)
                           :id (parse-integer (elt parts 1))
                           :info (elt parts 2))))

(defmethod enabledp ((d device))
  (xinput-enabled-p (device-id d)))

(defmethod disable ((d device))
  "Enables device and returns a string indicating the action."
  (xinput-cmd "disable" (device-id d))
  (format nil "disabled ~A" d))

(defmethod enable ((d device))
  "Enables device and returns a string indicating the action."
  (xinput-cmd "enable" (device-id d))
  (format nil "enabled ~A" d))

(defmethod toggle ((d device))
  "Toggles the device and returns a string indicating if it was enabled or
disabled along with the device id and name."
  (if (enabledp d) (disable d) (enable d)))

(defmethod print-object ((d device) stream)
  (format stream "~A (device ~A)"
          (device-name d)
          (device-id d)))

(defun xinput-cmd (&rest args)
  "Run `xinput` with args. For example:
  (xinput-cmd \"enable\" 10) ; enable device id 10

The equivalent shell command would be:
  xinput enable 10"
  (run-shell-command (format nil "xinput ~{~A~^ ~}" args) t))

(defun get-all-devices ()
  (mapcar
   (lambda (line)
     (make-device-from-line line))
   (ppcre:split #\Newline (xinput-cmd "list" "--short"))))

(defun sort-devices (device-list)
  (sort device-list
        (lambda (d1 d2)
          (< (device-id d1) (device-id d2)))))

(defun get-devices (&optional (regex nil))
  (sort-devices
   (let* ((all-devices (get-all-devices))
          (devices  (if (not regex)
                        all-devices
                        (remove-if-not (lambda (d) (name-matches-p d regex))
                                       all-devices))))
     (if *exclude-keyboards*
         (remove-if (lambda (d) (is-keyboard-p d))
                    devices)
         devices))))

(defun xinput-enabled-p (device-id)
  "Returns T if the device is enabled, else NIL."
  (let* ((scanner (ppcre:create-scanner "device\\s*enabled[^:]*:\\s*(\\d*)"
                                        :case-insensitive-mode t))
         (xinput-stdout (xinput-cmd "list-props" device-id))
         (groups (nth-value 1 (ppcre:scan-to-strings scanner xinput-stdout)))
         (status (elt groups 0)))
    (equal status "1")))

(defun for-devices (name-regex fn)
  (message (format nil "~{~A~^~%~}"
                   (mapcar (lambda (d) (funcall fn d))
                           (get-devices name-regex)))))

(defun toggle-devices (name-regex)
  "Toggle xinput devices whose name contains NAME-REGEX."
  (for-devices name-regex (lambda (d) (toggle d))))


(defun enable-devices (name-regex)
  "Toggle xinput devices whose name contains NAME-REGEX."
  (for-devices name-regex (lambda (d) (enable d))))

(defun disable-devices (name-regex)
  "Toggle xinput devices whose name contains NAME-REGEX."
  (for-devices name-regex (lambda (d) (disable d))))

(defcommand cmd-toggle-devices (name-regex) ((:string))
  "Toggle xinput devices whose name contains NAME-REGEX.

To view xinput device names on your system, use the shell command:
  xinput --list --names-only

For example to toggle devices whoose name contains \"touchpad\":

  (toggle-devices \"touchpad\")"
  (toggle-devices name-regex))

(define-key *top-map* (kbd "XF86TouchpadToggle") "cmd-toggle-devices touchpad")



;; --- StumpWM with SLIME ------------------------------------------------------

;; https://www.kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
