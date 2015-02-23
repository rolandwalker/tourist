;;; tourist-plugin-saveplace.el --- Saveplace.el plugin for tourist-mode
;;
;; Copyright (c) 2014-2015 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-saveplace.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `save-place' locations in
;; `tourist-mode'.
;;
;; To use this plugin your ~/.emacs file must contain at minimum
;;
;;     (require 'saveplace)
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET save-place RET
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

(eval-when-compile
  (defvar save-place-alist))

;;; register the plugin

;;;###autoload
(tourist-register-method 'saveplace
  '((pretty-name                 . "Cursor Location at Last Close")
    (priority                    . 10)
    (init-function               . tourist-plugin-saveplace-init)
    (init-buffer-function        . tourist-plugin-saveplace-init-buffer)
    (next-landmark-function      . tourist-plugin-saveplace-next)
    (previous-landmark-function  . tourist-plugin-saveplace-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-saveplace-init ()
  "Initialize the `tourist-mode' saveplace plugin."
  ;; don't load saveplace.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-saveplace-init-buffer ()
  "Initialize a buffer for the `tourist-mode' saveplace plugin."
  ;; per-buffer setup not needed
  t)

;;;###autoload
(defun tourist-plugin-saveplace-next ()
  "Find the next saveplace landmark for `tourist-mode'."
  (when (boundp 'saveplace-marks-alist)
    (let ((place (cdr (assoc buffer-file-name save-place-alist))))
      (when (integerp place)
        (list
         place
         place)))))

;;;###autoload
(defun tourist-plugin-saveplace-previous ()
  "Find the previous saveplace landmark for `tourist-mode'."
  (tourist-plugin-saveplace-next))

(provide 'tourist-plugin-saveplace)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu saveplace Saveplace
;;

;;; tourist-plugin-saveplace.el ends here
