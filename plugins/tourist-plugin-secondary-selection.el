;;; tourist-plugin-secondary-selection.el --- Secondary-selection plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-secondary-selection.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for the secondary-selection location
;; in `tourist-mode'.
;;
;; See Also
;;
;;    M-x customize-group RET tourist RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     If the secondary selection is not active, this plugin will
;;     have no effect.
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;; TODO
;;
;;; License
;;
;; See license in tourist.el
;;
;;; Code:
;;

;;; declarations

(declare-function tourist-register-method "tourist.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'secondary-selection
  '((pretty-name                 . "Secondary Selection")
    (priority                    . 10)
    (init-function               . tourist-plugin-secondary-selection-init)
    (init-buffer-function        . tourist-plugin-secondary-selection-init-buffer)
    (next-landmark-function      . tourist-plugin-secondary-selection-next)
    (previous-landmark-function  . tourist-plugin-secondary-selection-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-secondary-selection-init ()
  "Initialize the `tourist-mode' secondary-selection plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-secondary-selection-init-buffer ()
  "Initialize a buffer for the `tourist-mode' secondary-selection plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-secondary-selection-next ()
  "Find the next secondary-selection landmark for `tourist-mode'."
  (when (and (boundp 'mouse-secondary-overlay)
             mouse-secondary-overlay
             (eq (current-buffer) (overlay-buffer mouse-secondary-overlay)))
    (let ((start (overlay-start mouse-secondary-overlay)))
      (when (integer-or-marker-p start)
        (list
         start
         (overlay-end mouse-secondary-overlay))))))

;;;###autoload
(defun tourist-plugin-secondary-selection-previous ()
  "Find the previous secondary-selection landmark for `tourist-mode'."
  (tourist-plugin-secondary-selection-next))

(provide 'tourist-plugin-secondary-selection)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu flet
;;

;;; tourist-plugin-secondary-selection.el ends here
