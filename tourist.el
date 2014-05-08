;;; tourist.el --- Navigate quickly to major landmarks
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/tourist.el
;; Version: 0.1.0
;; Last-Updated: 3 May 2014
;; EmacsWiki: Tourist
;; Keywords: navigation, mouse, convenience
;; Package-Requires: ((string-utils "0.3.2") (nav-flash "1.0.0") (back-button "0.6.0") (smartrep "0.0.3") (express "0.5.3"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'tourist)
;;
;;     (global-tourist-mode 1)
;;
;;     navigate using the mouse wheel in the left fringe
;;
;; Explanation
;;
;; Tourist-mode is a minor mode that provides a general mechanism for
;; navigating buffers by skipping around quickly, landing on major
;; landmarks.
;;
;; The default key bindings include mouse-wheel when hovering over the
;; left fringe.
;;
;; Plugins are provided to navigate to landmarks identified by
;;
;;     imenu                          ; function definitions, et al.
;;     outline                        ; also works with org-mode
;;     bookmark
;;     bm                             ; an alternative bookmarks package
;;     track-changes                  ; as defined by hilit-chg.el
;;     folding-mode
;;     flymake
;;     flyspell
;;     wiki-nav
;;     linkd
;;     font-lock-warning-face
;;     comment-section                ; sections marked by three leading comment delimiters
;;     saveplace
;;     mark-ring
;;     global-mark-ring
;;     Man-mode
;;     button.el
;;     page-boundary
;;     secondary-selection
;;     info
;;     interactive-prompt
;;     fixmee-mode
;;     last-edit
;;
;; The plugin interface is documented in
;;
;;     tourist-imenu.el
;;
;; To install, unpack plugins to ~/.emacs.d/plugins/tourist/,
;; place tourist.el somewhere Emacs can find it, and add the
;; following to your ~/.emacs
;;
;;     (require 'tourist)
;;     (global-tourist-mode 1)
;;
;; Most plugins will not be activated unless the relevant package is
;; installed and/or minor-mode is on.  To access all of the currently
;; available plugins, the following packages must be installed
;;
;;     bm.el
;;     folding.el
;;     wiki-nav.el
;;     linkd.el
;;     fixmee.el
;;     flymake.el
;;     nrepl.el
;;     slime.el
;;
;; If you are only interested in a certain type of landmark, you can
;; set `tourist-default-methods' in customize or directly
;;
;;     (setq tourist-default-methods '(flymake))
;;
;; Or you might prefer to make separate key bindings for your
;; landmark of interest
;;
;;     (define-key tourist-mode-map (kbd "M-<wheel-up>")   #'(lambda ()
;;                                                             (interactive)
;;                                                             (tourist-goto-previous-landmark nil 'flymake)))
;;     (define-key tourist-mode-map (kbd "M-<wheel-down>") #'(lambda ()
;;                                                             (interactive)
;;                                                             (tourist-goto-next-landmark nil 'flymake)))
;;
;; There is also a popup menu available on the modeline lighter.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Uses if present: string-utils.el, nav-flash.el, back-button.el,
;;                      smartrep.el, express.el, bm.el, folding.el,
;;                      wiki-nav.el, linkd.el, fixmee.el, flymake.el,
;;                      nrepl.el, slime.el
;;
;; Bugs
;;
;;     Many.
;;
;; TODO
;;
;;     flet replacement
;;
;;     wrap and cycle are very broken
;;
;;         Rip them out and move to a feature branch
;;
;;         Rules for cycling are not very thought out - tourist-mode can
;;         be on in practically any mode; it probably isn't useful to
;;         cycle from say code into a man page.
;;
;;         Isn't 'cycle broken b/c it doesn't bury buffer when switching,
;;         meaning that it will just cycle between two choices?
;;
;;         Wrap / cycle should be settings in customize and toggleable in
;;         menu.  or just have separate bindings?
;;
;;         'cycle and 'wrap in cannot find landmark at point-max/point-min such as EOB
;;
;;         (define-key global-map (kbd "H-]") #'(lambda () (interactive) (tourist-goto-next-landmark 1 nil 'cycle)))
;;         (define-key global-map (kbd "H-[") #'(lambda () (interactive) (tourist-goto-previous-landmark 1 nil 'cycle)))
;;         (define-key global-map (kbd "H-]") #'(lambda () (interactive) (tourist-goto-next-landmark 1 nil 'wrap)))
;;         (define-key global-map (kbd "H-[") #'(lambda () (interactive) (tourist-goto-previous-landmark 1 nil 'wrap)))
;;
;;     Escape or replace newlines in feedback strings + truncate
;;
;;     Better customize interface for choosing methods? In order to do
;;     that, all plugins must be loaded.
;;
;;     limit feedback of tourist-goto-next-landmark based on
;;     called-interactively-p, but the function can be called from
;;     various other functions - so add parameter for feedback or
;;     bind tourist-less-feedback variable
;;
;;     Which commands should be globally available (currently none)?
;;     What should happen when tourist command is issued in a
;;     non-tourist buffer and cycling is off?
;;
;;     General issue: navigation can land on line at edge of screen.
;;     Instead respect user settings and scroll inward if needed.
;;
;;     Tourist-describe-line function - narrow region to line and
;;     iterate over every method and position (skipping to landmark
;;     end for efficiency)
;;
;;     A way to use next-error without changing state.  Probably has
;;     to be done on a per-mode basis.
;;
;;     Menu-bar menu (in addition to lighter popup), placed under
;;     Navigation.
;;
;;     Methods should have a per-mode preference - eg in org-mode,
;;     outline should be preferred
;;
;;     Option to use tooltip or popup.el for line description
;;
;;     Option to flash only from start-to-end of landmark.
;;
;;     "similar" variants
;;
;;         Should "similar" nav variants require that the point still on
;;         the matching hit?
;;
;;         Should "similar" nav variants work off the context of the point
;;         rather than the last success?
;;
;;     plugins
;;
;;         Help function in each plugin?
;;
;;         May need to have two versions of pretty-name: one for menu
;;         and one for echo area.
;;
;;         more testing
;;
;;         Include customize stubs in all plugins for commentary link?
;;
;;         needed new plugins
;;
;;             flycheck
;;             TAGS
;;             cscope
;;             ediff changes
;;             git changes
;;             (highlight-changes has facility to compare buffer/file)
;;
;;; License
;;
;; Simplified BSD License
;;
;; Copyright (c) 2014, Roland Walker
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requirements

(eval-and-compile
  ;; for setf, callf, loop, flet/cl-flet*, remove-if-not, assert
  (require 'cl)
  (unless (fboundp 'cl-flet*)
    (defalias 'cl-flet* 'flet)
    (put 'cl-flet* 'lisp-indent-function 1)
    (put 'cl-flet* 'edebug-form-spec '((&rest (defun*)) cl-declarations body))))

(require 'nav-flash    nil t)
(require 'back-button  nil t)
(require 'smartrep     nil t)
(require 'string-utils nil t)

(autoload 'express-message-logonly "express" "An flet'able replacement for `message' which logs but does not echo.")

;;; declarations

(declare-function smartrep-define-key                     "smartrep.el")
(declare-function back-button-push-mark                   "back-button.el")
(declare-function back-button-push-mark-local-and-global  "back-button.el")

;;; customizable variables

;;;###autoload
(defgroup tourist nil
  "Navigate quickly to major landmarks."
  :version "0.1.0"
  :link '(emacs-commentary-link :tag "Commentary" "tourist")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/tourist")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/Tourist")
  :prefix "tourist-"
  :group 'navigation
  :group 'convenience)

(defcustom tourist-plugin-directories (list
                                       (expand-file-name "tourist"
                                                         (locate-user-emacs-file "plugins")))
  "Directories to find `tourist-mode' plugins.

Plugins may also be found next to the location of tourist.el or
in a subdirectory \"plugins\" next to tourist.el."
  :type '(repeat string)
  :group 'tourist)

(defcustom tourist-default-methods nil
  "List of methods for finding `tourist-mode' landmarks.

Set to nil to use all available methods.

To see a list of methods, use the command `tourist-list-methods'."
  :type '(repeat symbol)
  :group 'tourist)

(defcustom tourist-disabled-methods nil
  "List of `tourist-mode' methods to disable.

This setting will override `tourist-default-methods'.

To see a list of methods, use the command `tourist-list-methods'."
  :type '(repeat symbol)
  :group 'tourist)

(defcustom tourist-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'tourist)

(defcustom tourist-cycle-buffer-filter 'buffer-file-name
  "When cycling, only consider buffers for which the function evaluates to non-nil.

The function should take a single argument (the buffer).  The
default filter causes `tourist-mode' to consider only buffers
which are associated with a file.

To disable cycle filtering, set this value to nil or `identity'."
  :type 'function
  :group 'tourist)

(defcustom tourist-mode-lighter " tour"
  "This string appears in the mode-line when `tourist-mode' is active.

Set to nil or the empty string to disable the mode-line
lighter for `tourist-mode'."
  :type 'string
  :risky t
  :group 'tourist)

(defcustom tourist-never-widen nil
  "Never let `tourist-mode' call `widen' on a narrowed buffer."
  :type 'boolean
  :group 'tourist)

(defcustom tourist-push-mark t
  "Whether to set the global mark before a series of `tourist-mode' navigation commands.

When this option is set, `pop-global-mark' (typically bound to
C-x C-SPC) will return the cursor to the starting point after
a series of `tourist-mode' navigation commands."
  :type 'boolean
  :group 'tourist)

;;;###autoload
(defgroup tourist-keys nil
  "Keyboard and mouse bindings used by `tourist-mode'."
  :group 'tourist)

(defcustom tourist-smartrep-prefix "C-c"
  "Prefix key for smartrep.el bindings.

Smartrep bindings will be installed for all `tourist-mode' key
bindings which match this prefix.

The format for key sequences is as defined by `kbd'.

Set to nil or the empty string to disable smartrep for
`tourist-mode'."
  :type 'string
  :group 'tourist-keys)

(defcustom tourist-goto-previous-landmark-keystrokes '(
                                                       "C-c L"
                                                       "<left-fringe> <wheel-up>"
                                                       "<left-fringe> <mouse-4>"
                                                       )
  "Key sequences to search backward for the previous landmark.

These bindings are in effect whenever `tourist-mode' is active.
The default mouse binding is only active in the left fringe.

The format for key sequences is as defined by `kbd'."
  :type '(repeat string)
  :group 'tourist-keys)

(defcustom tourist-goto-next-landmark-keystrokes '(
                                                   "C-c l"
                                                   "<left-fringe> <wheel-down>"
                                                   "<left-fringe> <mouse-5>"
                                                   )
  "Key sequences to search forward for the next landmark.

These bindings are in effect whenever `tourist-mode' is active.
The default mouse binding is only active in the left fringe.

The format for key sequences is as defined by `kbd'."
  :type '(repeat string)
  :group 'tourist-keys)

(defcustom tourist-goto-previous-similar-landmark-keystrokes '(
                                                               "C-c C-L"
                                                               "<left-fringe> <C-wheel-up>"
                                                               "<left-fringe> <C-mouse-4>"
                                                               )
  "Key sequences to search backward for the previous landmark of same type.

These bindings are in effect whenever `tourist-mode' is active.
The default mouse binding is only active in the left fringe.

The format for key sequences is as defined by `kbd'."
  :type '(repeat string)
  :group 'tourist-keys)

(defcustom tourist-goto-next-similar-landmark-keystrokes '(
                                                           "C-c C-l"
                                                           "<left-fringe> <C-wheel-down>"
                                                           "<left-fringe> <C-mouse-5>"
                                                           )
  "Key sequences to search forward for the next landmark of the same type.

These bindings are in effect whenever `tourist-mode' is active.
The default mouse binding is only active in the left fringe.

The format for key sequences is as defined by `kbd'."
  :type '(repeat string)
  :group 'tourist-keys)

;;;###autoload
(defgroup tourist-global nil
  "Settings for `global-tourist-mode'."
  :group 'tourist)

(defcustom tourist-exclude-modes '(
                                   fundamental-mode
                                   )
  "The minor mode will not be activated if a buffer's `major-mode' is included in this list."
  :type '(repeat symbol)
  :group 'tourist-global)

(defcustom tourist-buffer-name-exclude-pattern "\\` "
  "Do not activate minor made in buffers matching this regular expression.

The default pattern is designed to match buffers which are
internal to Emacs."
  :type 'regexp
  :group 'tourist-global)

(defcustom tourist-buffer-include-functions '()
  "Do not activate minor mode in a buffer unless all functions evaluate non-nil.

Each function should take a single argument (a buffer).

Set this value to nil to disable."
  :type '(repeat function)
  :group 'tourist-global)

(defcustom tourist-buffer-exclude-functions '()
  "Do not activate minor mode in a buffer if any function evaluates non-nil.

Each function should take a single argument (a buffer).

Set this value to nil to disable."
  :type '(repeat function)
  :group 'tourist-global)

;;;###autoload
(defgroup tourist-plugins nil
  "Settings for `tourist-mode' plugins."
  :group 'tourist)

;;; variables

(defvar global-tourist-mode nil
  "Mode variable for `global-tourist-mode'.")

(defvar tourist-mode nil
  "Mode variable for `tourist-mode'.")
(make-variable-buffer-local 'tourist-mode)

(defvar tourist-library-path load-file-name
  "Location of the library when loaded.")

(defvar tourist-plugins-loaded nil
  "Whether plugins have been loaded.")

(defvar tourist-inits-done (make-hash-table :test 'eq)
  "Table initialization functions which have already been run.")

;;;###autoload
(defvar tourist-method-definitions  nil
  "Alist of all known navigation methods for `tourist-mode'.

Each cell is in the form

    (NAME
       '((pretty-name                 . STRING)
         (priority                    . NUMBER)
         (init-function               . FUNCTION)
         (init-buffer-function        . FUNCTION)
         (next-landmark-function      . FUNCTION)
         (previous-landmark-function  . FUNCTION))).")

(defvar tourist-last-hit nil)
(defvar tourist-navigation-commands '(
                                      tourist-goto-previous-landmark
                                      tourist-goto-next-landmark
                                      tourist-goto-previous-similar-landmark
                                      tourist-goto-next-similar-landmark
                                      )
  "A list of all interactive navigation commands for `tourist-mode'.")

(defvar tourist-global-commands nil
  "List of globally available commands.")

(defvar tourist-lighter-menu-mouse-button 1
  "Which mouse button invokes the modeline context menu.")

(defvar tourist-lighter-keymap-property 'keymap
  "Which property sets the lighter keymap.")

;;; compatibility functions

(unless (fboundp 'string-match-p)
  ;; added in 23.x
  (defun string-match-p (regexp string &optional start)
    "Same as `string-match' except this function does not change the match data."
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start))))

(unless (fboundp 'back-button-push-mark-local-and-global)
  (fset 'back-button-push-mark (symbol-function 'push-mark))
  (defun back-button-push-mark-local-and-global (&optional location nomsg activate consecutives)
  "Push mark at LOCATION, and unconditionally add to `global-mark-ring'.

This function differs from `push-mark' in that `global-mark-ring'
is always updated.

LOCATION is optional, and defaults to the current point.

NOMSG and ACTIVATE are as documented at `push-mark'.

When CONSECUTIVES is set to 'limit and the new mark is in the same
buffer as the first entry in `global-mark-ring', the first entry
in `global-mark-ring' will be replaced.  Otherwise, a new entry
is pushed onto `global-mark-ring'.

When CONSECUTIVES is set to 'allow-dupes, it is possible to push
an exact duplicate of the current topmost mark onto `global-mark-ring'."
  (callf or location (point))
  (back-button-push-mark location nomsg activate)
  (when (or (eq consecutives 'allow-dupes)
            (not (equal (mark-marker)
                        (car global-mark-ring))))
    (when (and (eq consecutives 'limit)
               (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      (move-marker (car global-mark-ring) nil)
      (pop global-mark-ring))
    (push (copy-marker (mark-marker)) global-mark-ring)
    (when (> (length global-mark-ring) global-mark-ring-max)
      (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
      (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)))))

;; todo: update this
(unless (fboundp 'express-message-logonly)
  (defun express-message-logonly (&rest args)
    "An flet'able replacement for `message' which logs but does not echo.

ARGS are as for `message', including a format-string."
    (when message-log-max
      ;; don't worry about truncating the message log, some standard
      ;; call to `message' will catch up with it later.
      (with-current-buffer "*Messages*"
        (save-excursion
          (goto-char (point-max))
          (let ((msg (apply 'format args))
                (inhibit-read-only t))
            (unless (eq (line-beginning-position) (point))
              (insert "\n"))
            (insert msg)
            (unless (eq (line-beginning-position) (point))
              (insert "\n"))
            msg))))))

;;; keymaps

(defvar tourist-mode-map        (make-sparse-keymap) "Keymap for `tourist-mode' minor-mode.")
(defvar tourist-mode-global-map (make-sparse-keymap) "Keymap for `global-tourist-mode' global minor-mode.")

(let ((smart-keys nil))
  (dolist (cmd tourist-navigation-commands)
    (dolist (k (symbol-value (intern (concat (symbol-name cmd) "-keystrokes"))))
      (when (and (not (string-match-p "mouse\\|wheel\\|button" k))
                 (not (get cmd :advertised-binding)))
        (put cmd :advertised-binding (read-kbd-macro k)))
      (if (and (featurep 'smartrep)
               (stringp tourist-smartrep-prefix)
               (> (length tourist-smartrep-prefix) 0)
               (string-match-p (concat "\\`" tourist-smartrep-prefix "\\>") k))
          (push (cons (replace-regexp-in-string
                       (concat "\\`" tourist-smartrep-prefix "\\>[ \t]*")
                       ""
                       k)
                      cmd)
                smart-keys)
        ;; else
        (define-key tourist-mode-map (read-kbd-macro k) cmd))))
  (when smart-keys
    (smartrep-define-key tourist-mode-map tourist-smartrep-prefix smart-keys)))

(dolist (cmd tourist-global-commands)
  (dolist (k (symbol-value (intern (concat (symbol-name cmd) "-keystrokes"))))
    (when (and (not (string-match-p "mouse\\|wheel\\|button" k))
               (not (get cmd :advertised-binding)))
      (put cmd :advertised-binding (read-kbd-macro k)))
    (define-key tourist-mode-map        (read-kbd-macro k) cmd)
    (define-key tourist-mode-global-map (read-kbd-macro k) cmd)))

;;; lighter

(defvar tourist-lighter-map  (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "<mode-line> <wheel-up>"      ) 'tourist-goto-previous-landmark)
                               (define-key map (kbd "<mode-line> <wheel-down>"    ) 'tourist-goto-next-landmark)
                               (define-key map (kbd "<mode-line> C-<wheel-up>"    ) 'tourist-goto-previous-similar-landmark)
                               (define-key map (kbd "<mode-line> C-<wheel-down>"  ) 'tourist-goto-next-similar-landmark)
                               (define-key map (kbd "<mode-line> <mouse-4>"       ) 'tourist-goto-previous-landmark)
                               (define-key map (kbd "<mode-line> <mouse-5>"       ) 'tourist-goto-next-landmark)
                               (define-key map (kbd "<mode-line> C-<mouse-4>"     ) 'tourist-goto-previous-similar-landmark)
                               (define-key map (kbd "<mode-line> C-<mouse-5>"     ) 'tourist-goto-next-similar-landmark)
                               (define-key map (read-kbd-macro (format "<mode-line> <down-mouse-%s>" tourist-lighter-menu-mouse-button)) 'tourist-popup)
                               map) "Keymap for the `tourist-mode' mode-line lighter.")

(when (and (stringp tourist-mode-lighter)
           (> (length tourist-mode-lighter) 0))
  (callf propertize tourist-mode-lighter
                    tourist-lighter-keymap-property tourist-lighter-map
                    'help-echo (format "tourist-mode: mouse-%s menu\nwheel down/up: next/prev\nwheel C-down/C-up: next-similar/prev-similar" tourist-lighter-menu-mouse-button)))

;;; macros

(defmacro tourist-called-interactively-p (&optional kind)
  "A backward-compatible version of `called-interactively-p'.

Optional KIND is as documented at `called-interactively-p'
in GNU Emacs 24.1 or higher."
  (cond
    ((not (fboundp 'called-interactively-p))
     '(interactive-p))
    ((condition-case nil
         (progn (called-interactively-p 'any) t)
       (error nil))
     `(called-interactively-p ,kind))
    (t
     '(called-interactively-p))))

(defmacro tourist-saving-state (&rest body)
  "Execute BODY in a special environment, saving state of many types."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (save-excursion
       (save-restriction
         (save-match-data
           (with-demoted-errors
             (let ((deactivate-mark nil)
                   (mark-ring mark-ring)
                   (global-mark-ring global-mark-ring)
                   (inhibit-point-motion-hooks nil))
               ,@body)))))))

;;; utility functions

(defun tourist-buffer-included-p (buf)
  "Return BUF if `global-tourist-mode' should enable `tourist-mode' in BUF."
  (when (and (not noninteractive)
             (bufferp buf)
             (buffer-name buf))
    (with-current-buffer buf
      (when (and (not (minibufferp buf))
                 (not (eq (aref (buffer-name) 0) ?\s))           ; overlaps with `tourist-buffer-exclude-pattern'
                 (not (memq major-mode tourist-exclude-modes))
                 (not (string-match-p tourist-buffer-name-exclude-pattern (buffer-name buf)))
                 (catch 'success
                   (dolist (filt tourist-buffer-exclude-functions)
                     (when (funcall filt buf)
                       (throw 'success nil)))
                   t)
                 (catch 'failure
                   (dolist (filt tourist-buffer-include-functions)
                     (unless (funcall filt buf)
                       (throw 'failure nil)))
                   t))
        buf))))

(defun tourist-coerce-landmark (landmark method)
  "Return a copy of list LANDMARK coerced to standard format.

Make the value conformant to '(START END DESCRIPTION METHOD),
filling in values for END and DESCRIPTION if needed.

START and END are forced to integers if they are markers.

DESCRIPTION is formatted for display as needed.

METHOD is never included in the input argument LANDMARK, but is
appended to the return value."
  (when (markerp (nth 0 landmark))
    (callf marker-position (nth 0 landmark)))
  (when (markerp (nth 1 landmark))
    (callf marker-position (nth 1 landmark)))
  (callf subseq landmark 0 3)
  (unless (nth 1 landmark)
    (setf (nth 1 landmark) (nth 0 landmark)))
  (append landmark (list method)))

;; (defun tourist-definition-sorter ()
;;   "Sort `tourist-method-definitions' in-place by the priority field."
;;   (setq tourist-method-definitions
;;         (sort tourist-method-definitions
;;               #'(lambda (a b)
;;                   (cond
;;                     ((not (numberp (cdr (assq 'priority (cdr b)))))
;;                      t)
;;                     ((not (numberp (cdr (assq 'priority (cdr a)))))
;;                      nil)
;;                     (t
;;                      (< (cdr (assq 'priority (cdr a)))
;;                         (cdr (assq 'priority (cdr b))))))))))

(defun tourist-method-priority-sorter (methods)
  "Desctructively sort METHODS by the priority field in `tourist-method-definitions'."
  (sort methods
        #'(lambda (a b)
            (cond
              ((not (numberp (cdr (assq 'priority (cdr (assq b tourist-method-definitions))))))
               t)
              ((not (numberp (cdr (assq 'priority (cdr (assq a tourist-method-definitions))))))
               nil)
              (t
               (< (cdr (assq 'priority (cdr (assq a tourist-method-definitions))))
                  (cdr (assq 'priority (cdr (assq b tourist-method-definitions))))))))))

;; plugin interface

;;;###autoload
(progn
  (defun tourist-register-method (name method)
    "Using NAME, register METHOD for finding landmarks in `tourist-mode'."
    (add-to-list 'tourist-method-definitions (cons name method))))

(defun tourist-load-plugins ()
  "Load `tourist-mode' plugins."
  (unless tourist-plugins-loaded
    (setq tourist-plugins-loaded t)
    (let ((plugin-list nil)
          (fail-list nil)
          (load-path load-path))
      (dolist (dir (delete-dups
                    (reverse (append tourist-plugin-directories
                                     (when tourist-library-path
                                       (list (file-name-directory tourist-library-path)
                                             (concat (file-name-directory tourist-library-path) "plugins")
                                             (file-name-directory (file-truename tourist-library-path))
                                             (concat (file-name-directory (file-truename tourist-library-path)) "plugins")))))))
        (when (and (file-directory-p dir)
                   (file-readable-p dir))
          (add-to-list 'load-path dir)
          (with-demoted-errors
            (callf append plugin-list
              (remove-if 'featurep
                  (mapcar 'intern
                      (remove-if #'(lambda (x) (string-match-p "-\\(autoloads\\|pkg\\)\\'" x))
                          (mapcar 'file-name-nondirectory
                              (mapcar #'(lambda (file)
                                          (replace-regexp-in-string "\\.el\\'" "" file))
                                      (directory-files
                                       (expand-file-name dir) nil "\\`tourist-.*\\.el\\'"))))))))))
      (setq plugin-list (remove-if #'(lambda (plugin)
                                       (fboundp (intern-soft (format "%s-init" plugin)))) plugin-list))
      (delete-dups plugin-list)
      (dolist (plugin plugin-list)
        (with-demoted-errors
          (require plugin))
        (unless (featurep plugin)
          (push plugin fail-list)))
      (when fail-list
        (ding)
        (message "Failed plugin load: %s" (mapconcat 'symbol-name fail-list ", "))))))

;; main driver

;; todo logging

(defun tourist-find-nearby-landmarks (&optional method reverse pos)
  "Find nearby landmarks.

If METHOD is non-nil, use only that method when searching.
Otherwise, use methods listed in `tourist-default-methods',
removing those listed in `tourist-disabled-methods'.

If METHOD and `tourist-default-methods' are both nil, use all
methods described in `tourist-method-definitions', removing
those listed in `tourist-disabled-methods'.

If REVERSE is non-nil, search backwards, otherwise search forwards.

If POS is set, search from that location, otherwise use the
current point."
  (tourist-saving-state
    (when pos
      (goto-char pos))
    (let ((methods nil))
      (if method
          (setq methods (list method))
        ;; else
        (setq methods (or tourist-default-methods (mapcar 'car tourist-method-definitions)))
        (callf2 remove-if #'(lambda (x)
                              (memq x tourist-disabled-methods)) methods)
        (callf tourist-method-priority-sorter methods))
      (delq nil
            (let ((deactivate-mark))
              (if reverse
                  (narrow-to-region (point-min) (point))
                (narrow-to-region (point) (point-max)))
              (loop for method in methods
                    collect
                    (let*  ((definition                 (assq method tourist-method-definitions))
                            (pretty-name                (cdr (assq 'pretty-name            (cdr definition))))
                            (init-function              (cdr (assq 'init-function          (cdr definition))))
                            (init-buffer-function       (cdr (assq 'init-buffer-function   (cdr definition))))
                            (nav-function               (if reverse
                                                            (cdr (assq 'previous-landmark-function (cdr definition)))
                                                          (cdr (assq 'next-landmark-function (cdr definition))))))
                      (assert definition nil "Undefined tourist method: %s" method)
                      (when (memq init-function (gethash 'global tourist-inits-done))
                        (setq init-function nil))
                      (when (memq init-buffer-function (gethash (current-buffer) tourist-inits-done))
                        (setq init-buffer-function nil))
                      ;; (message "method: %s" method) (sit-for 1)
                      (when init-function
                        (tourist-saving-state
                          ;; (message "method: %s / init: %s" method init-function) (sit-for 1)
                          (funcall init-function)
                          (push init-function (gethash 'global tourist-inits-done))))
                      (when init-buffer-function
                        (tourist-saving-state
                          ;; (message "method: %s / init-buffer: %s" method init-buffer-function) (sit-for 1)
                          (funcall init-buffer-function)
                          (push init-buffer-function (gethash (current-buffer) tourist-inits-done))))
                      (when nav-function
                        (let* ((landmark (tourist-saving-state
                                           ;; (message "%s: navigate / %s" method nav-function) (sit-for 1)
                                           (cl-flet* ((message (&rest args)
                                                               (apply 'express-message-logonly args)))
                                             (funcall nav-function))))
                               (position (nth 0 landmark)))
                          (when (and (integer-or-marker-p position)
                                     ;; must advance
                                     (not (eq position (point)))
                                     ;; sanity checks
                                     (not (= position 0))
                                     (>= position (point-min))
                                     (<= position (point-max)))
                            (if reverse
                                (narrow-to-region position (point-max))
                              (narrow-to-region (point-min) position))
                            ;; (message "%s: mark %s" method landmark) (sit-for 1)
                            (tourist-coerce-landmark landmark method)))))))))))

;;; minor mode definition

(define-minor-mode tourist-mode
  "Turn on `tourist-mode'.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle."
  :lighter tourist-mode-lighter
  :keymap tourist-mode-map
  :group 'tourist
  (cond
    (tourist-mode
     (tourist-load-plugins)
     (when (and (tourist-called-interactively-p 'interactive)
                (not tourist-less-feedback))
       (message "tourist mode enabled")))
    (t
     (when (and (tourist-called-interactively-p 'interactive)
                (not tourist-less-feedback))
       (message "tourist mode disabled")))))

;;; global minor mode definition

(defun tourist-maybe-turn-on (&optional arg)
  "Called by `global-tourist-mode' to activate `tourist-mode' in a buffer if appropriate.

`tourist-mode' will be activated in every buffer, except

   minibuffers
   buffers with names that begin with space
   buffers excluded by `tourist-exclude-modes'
   buffers excluded by `tourist-buffer-name-exclude-pattern'
   buffers that fail   `tourist-include-functions'
   buffers that pass   `tourist-exclude-functions'

If called with a negative ARG, deactivate `tourist-mode' in the buffer."
  (callf or arg 1)
  (when (or (< arg 0)
            (tourist-buffer-included-p (current-buffer)))
    (tourist-mode arg)))

(define-globalized-minor-mode global-tourist-mode tourist-mode tourist-maybe-turn-on
  :keymap tourist-mode-global-map
  :group 'tourist)

;;; interactive commands

;;;###autoload
(defun tourist-popup (ev)
  "Pop up a dynamically-generated menu to control `tourist-mode'."
  (interactive "e")
  (let ((popup-menu-map               (make-sparse-keymap "Tourist Mode"))
        (tourist-toggle-submenu-map   (make-sparse-keymap "Toggle Methods"))
        (tourist-next-submenu-map     (make-sparse-keymap "Next ..."))
        (tourist-previous-submenu-map (make-sparse-keymap "Previous ...")))
    (dolist (method (sort (mapcar 'car tourist-method-definitions) #'(lambda (a b)
                                                                  (string< (cdr (assq 'pretty-name (assq b tourist-method-definitions)))
                                                                           (cdr (assq 'pretty-name (assq a tourist-method-definitions)))))))
      (let ((menu-label (or (cdr (assq 'pretty-name (assq method tourist-method-definitions))) (symbol-name method))))
        (define-key tourist-toggle-submenu-map `[,method] `(menu-item ,menu-label
                                                            (lambda ()
                                                             (interactive)
                                                             (callf or tourist-default-methods (mapcar 'car tourist-method-definitions))
                                                             (setq tourist-default-methods
                                                                   (cond
                                                                     ((and (memq ',method tourist-default-methods)
                                                                           (= (length tourist-default-methods) 1))
                                                                      ;; refuse to disable last method
                                                                      tourist-default-methods)
                                                                     ((memq ',method tourist-default-methods)
                                                                      (delq ',method tourist-default-methods))
                                                                     (t
                                                                      (push ',method tourist-default-methods)))))
                                                           :button (:toggle . (or (null tourist-default-methods)
                                                                                  (memq ',method tourist-default-methods)))))

      (define-key tourist-next-submenu-map     `[,method] `(menu-item ,menu-label
                                                            (lambda ()
                                                              (interactive)
                                                              (tourist-goto-next-landmark ',method))))

      (define-key tourist-previous-submenu-map `[,method] `(menu-item ,menu-label
                                                            (lambda ()
                                                              (interactive)
                                                              (tourist-goto-previous-landmark ',method))))))

    (define-key popup-menu-map [customize-tourist]              '(menu-item "Customize"                (lambda (e) (interactive "e") (customize-group 'tourist))))
    (define-key popup-menu-map [turn-off-tourist-mode]          '(menu-item "Turn Off Tourist Mode"    tourist-mode))
    (define-key popup-menu-map [separator-1]                    '(menu-item "--"))
    (define-key popup-menu-map [toggle-submenu]                 `(menu-item "Toggle Methods"          ,tourist-toggle-submenu-map))
    (define-key popup-menu-map [next-submenu]                   `(menu-item "Go to Next ..."          ,tourist-next-submenu-map))
    (define-key popup-menu-map [previous-submenu]               `(menu-item "Go to Previous ..."      ,tourist-previous-submenu-map))
    (define-key popup-menu-map [tourist-goto-next-landmark]     (append '(menu-item "Go to Next Landmark" tourist-goto-next-landmark)
                                                                            ;; force :keys because of smartrep
                                                                            (when (get 'tourist-goto-next-landmark :advertised-binding)
                                                                              (list :keys
                                                                                    (format-kbd-macro
                                                                                     (get 'tourist-goto-next-landmark :advertised-binding))))))
    (define-key popup-menu-map [tourist-goto-previous-landmark] (append '(menu-item "Go to Previous Landmark" tourist-goto-previous-landmark)
                                                                        (when (get 'tourist-goto-previous-landmark :advertised-binding)
                                                                          (list :keys
                                                                                (format-kbd-macro
                                                                                 (get 'tourist-goto-previous-landmark :advertised-binding))))))
    (popup-menu popup-menu-map ev)))

;;;###autoload
(defun tourist-list-methods (&optional arg)
  "List methods known to `tourist-mode'.

By default, list all known methods.

With positive prefix ARG, list only methods which are enabled by
default.

With negative prefix ARG, list disabled methods."
  (interactive "P")
  (tourist-load-plugins)
  (let ((all-methods (mapcar 'car tourist-method-definitions))
        (methods nil)
        (description nil))
    (dolist (m (append tourist-default-methods tourist-disabled-methods))
      (assert (memq m all-methods) nil "Tourist: undefined method: %s" m))
    (cond
      ((and arg
            (> (prefix-numeric-value arg) 0))
       (setq methods (or tourist-default-methods all-methods))
       (callf2 remove-if #'(lambda (m)
                             (memq m tourist-disabled-methods)) methods)
       (setq description "Default"))
      ((and arg
            (< (prefix-numeric-value arg) 0))
       (setq methods tourist-disabled-methods)
       (setq description "Disabled"))
      (t
       (setq methods all-methods)
       (setq description "Known")))
    (message (concat (format "%s methods: " description)
                     (if methods
                         (mapconcat 'symbol-name methods ", ")
                       "<none>")))))

;;;###autoload
(defun tourist-goto-next-landmark (&optional arg method wrap landmarks)
  "Navigate to the ARGth next landmark.

If METHOD is non-nil, use only that method when searching.  Otherwise
follow the rules in `tourist-find-nearby-landmarks' to determine
which methods to use.

If WRAP is nil or unknown, stop at the end of the buffer.  If
WRAP is 'wrap, continue the search, wrapping around the ends of
the buffer.  If WRAP is 'cycle, continue the search in the next
buffer.

If LANDMARKS is set, use that set of landmarks rather than generate
landmarks from scratch.  This parameter is for internal use only."
  (interactive "p")
  (callf or arg 1)
  (when (= arg 0)
    (setq arg 1))
  (when (and (tourist-called-interactively-p 'any)
             (not (memq last-command tourist-navigation-commands))
             tourist-push-mark
             (not (use-region-p)))
    (back-button-push-mark-local-and-global nil t))
  (let ((closest-landmark-pos nil))
    (dotimes (i (abs arg))
      (when (or (> i 0)
                (not landmarks))
        (setq landmarks (tourist-find-nearby-landmarks method (< arg 0) closest-landmark-pos)))
      (setq closest-landmark-pos
            (when landmarks
              (apply (if (< arg 0) 'max 'min) (mapcar 'car landmarks)))))

    (if (and closest-landmark-pos
             (or (and (< arg 0)
                      (< closest-landmark-pos (point)))
                 (and (> arg 0)
                      (> closest-landmark-pos (point)))))
        (progn
          (when (and (not tourist-never-widen)
                     ;; the ">=" here is because of next-manpage, may not always be the right thing
                     (or (>= closest-landmark-pos (point-max))
                         (<  closest-landmark-pos (point-min))))
            (widen))
          (goto-char closest-landmark-pos)
          (setq tourist-last-hit (assoc closest-landmark-pos landmarks))
          (setf (car tourist-last-hit) (set-marker (make-marker)
                                                   (car tourist-last-hit)
                                                   (current-buffer)))
          (when (not tourist-less-feedback)
            (let ((msg (format "landmark: %s" (or (nth 2 tourist-last-hit)
                                                  (cdr
                                                   (assq 'pretty-name
                                                         (cdr
                                                          (assoc
                                                           (car (last tourist-last-hit))
                                                           tourist-method-definitions))))))))
              (if (fboundp 'string-utils-truncate-to)
                  (progn
                    (callf string-utils-compress-whitespace msg)
                    (callf string-utils-trim-whitespace msg)
                    (callf string-utils-truncate-to msg (frame-width)))
                ;; else
                (setf msg (replace-regexp-in-string "[ \t\r\f\n]+" " " msg))
                (setf msg (subseq msg 0 (min (length msg) (frame-width)))))
              (message msg)))
          (when (fboundp 'nav-flash-show)
            (nav-flash-show (line-beginning-position))))
      (cond
        ((eq wrap 'wrap)
         (let ((new-point (if (< arg 0) (point-max) (point-min))))
           (setq landmarks (tourist-find-nearby-landmarks method (< arg 0) new-point))
           (if landmarks
               (progn
                 (goto-char new-point)
                 (tourist-goto-next-landmark (signum arg) method nil landmarks))
             (message "no more landmarks"))))
        ((eq wrap 'cycle)
         (let ((new-buf (catch 'break
                          (dolist (buf (cdr (buffer-list)))
                            (when (and (buffer-local-value 'tourist-mode buf)
                                   (or (null tourist-cycle-buffer-filter)
                                       (funcall tourist-cycle-buffer-filter buf)))
                              (with-current-buffer buf
                                (let ((new-point (if (< arg 0) (point-max) (point-min))))
                                  (setq landmarks (tourist-find-nearby-landmarks method (< arg 0) new-point))
                                  (when landmarks
                                    (throw 'break buf)))))))))
           (if (and new-buf landmarks)
               (progn
                 (switch-to-buffer new-buf)
                 (goto-char (if (< arg 0) (point-max) (point-min)))
                 (tourist-goto-next-landmark (signum arg) method nil landmarks))
             (message "no more landmarks"))))
        (t
         (express "normal")
         (message "no more landmarks"))))))

;;;###autoload
(defun tourist-goto-previous-landmark (&optional arg method wrap)
  "Navigate to the ARGth previous landmark.

Optional METHOD specifies a single method to identify landmarks.

Optional WRAP is as documented at `tourist-goto-next-landmark'."
  (interactive "p")
  (callf or arg 1)
  (when (= arg 0)
    (setq arg 1))
  (when (and (tourist-called-interactively-p 'any)
             (not (memq last-command tourist-navigation-commands))
             tourist-push-mark
             (not (use-region-p)))
    (back-button-push-mark-local-and-global nil t))
    (tourist-goto-next-landmark (- arg) method wrap))

;;;###autoload
(defun tourist-goto-previous-similar-landmark (&optional arg wrap)
  "Navigate to the ARGth previous landmark of the same type.

Optional WRAP is as documented at `tourist-goto-next-landmark'."
  (callf or arg 1)
  (when (= arg 0)
    (setq arg 1))
  (when (and (tourist-called-interactively-p 'any)
             (not (memq last-command tourist-navigation-commands))
             tourist-push-mark
             (not (use-region-p)))
    (back-button-push-mark-local-and-global nil t))
  (let ((method (car (last tourist-last-hit))))
    (tourist-goto-previous-landmark arg method wrap)))

;;;###autoload
(defun tourist-goto-next-similar-landmark (&optional arg wrap)
  "Navigate to the ARGth next landmark of the same type.

Optional WRAP is as documented at `tourist-goto-next-landmark'."
  (callf or arg 1)
  (when (= arg 0)
    (setq arg 1))
  (when (and (tourist-called-interactively-p 'any)
             (not (memq last-command tourist-navigation-commands))
             tourist-push-mark
             (not (use-region-p)))
    (back-button-push-mark-local-and-global nil t))
  (let ((method (car (last tourist-last-hit))))
    (tourist-goto-next-landmark arg method wrap)))

(provide 'tourist)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: fixmee linkd flymake smartrep hilit saveplace imenu
;; LocalWords: devel args setf callf flet CONSECUTIVES NOMSG nrepl
;; LocalWords: flet'able

;;; tourist.el ends here
