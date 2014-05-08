;;; tourist-plugin-button.el --- Button-buffer plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-button.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides `tourist-mode' support for `forward-button',
;; which is available in `help-mode' and related buffers.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
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
(tourist-register-method 'button
  '((pretty-name                 . "Button or Widget")
    (priority                    . 70)
    (init-function               . tourist-plugin-button-init)
    (init-buffer-function        . tourist-plugin-button-init-buffer)
    (next-landmark-function      . tourist-plugin-button-next)
    (previous-landmark-function  . tourist-plugin-button-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-button-init ()
  "Initialize the `tourist-mode' button.el plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-button-init-buffer ()
  "Initialize a buffer for the `tourist-mode' button.el plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-button-next ()
  "Find the next button.el landmark for `tourist-mode'."
  (let ((button (ignore-errors
                  (forward-button 1))))
    (when button
      (list
       (button-start button)
       (button-end button)))))

;;;###autoload
(defun tourist-plugin-button-previous ()
  "Find the previous button.el landmark for `tourist-mode'."
  (let ((button (ignore-errors
                  (forward-button -1))))
    (when button
      (list
       (button-start button)
       (button-end button)))))

(provide 'tourist-plugin-button)

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

;;; tourist-plugin-button.el ends here
