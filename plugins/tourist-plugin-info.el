;;; tourist-plugin-info.el --- Info-mode plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-info.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides `tourist-mode' support for `Info-mode'
;; buffers.
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
(declare-function Info-prev-reference     "info.el")
(declare-function Info-next-reference     "info.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'info
  '((pretty-name                 . "Info Reference")
    (priority                    . 60)
    (init-function               . tourist-plugin-info-init)
    (init-buffer-function        . tourist-plugin-info-init-buffer)
    (next-landmark-function      . tourist-plugin-info-next)
    (previous-landmark-function  . tourist-plugin-info-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-info-init ()
  "Initialize the `tourist-mode' Info plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-info-init-buffer ()
  "Initialize a buffer for the `tourist-mode' Info plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-info-next ()
  "Find the next Info landmark for `tourist-mode'."
  (when (derived-mode-p 'Info-mode)
    (ignore-errors
      (Info-next-reference))
    (list
     (point)
     (point))))

;;;###autoload
(defun tourist-plugin-info-previous ()
  "Find the previous Info landmark for `tourist-mode'."
  (when (derived-mode-p 'Info-mode)
    (ignore-errors
      (Info-prev-reference))
    (list
     (point)
     (point))))

(provide 'tourist-plugin-info)

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

;;; tourist-plugin-info.el ends here
