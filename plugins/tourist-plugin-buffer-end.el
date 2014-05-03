;;; tourist-plugin-buffer-end.el --- Buffer-end plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-buffer-end.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides `tourist-mode' support for
;; buffer-ends.
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
;;     The 1- and 1+ are just hacks to get around the fact that point-min and
;;     point-max are hardcoded as disallowed in tourist.el
;;
;;     Return different pretty-name for bob/eob.
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
(tourist-register-method 'buffer-end
  '((pretty-name                 . "Buffer End")
    (priority                    . 5)
    (init-function               . tourist-plugin-buffer-end-init)
    (init-buffer-function        . tourist-plugin-buffer-end-init-buffer)
    (next-landmark-function      . tourist-plugin-buffer-end-next)
    (previous-landmark-function  . tourist-plugin-buffer-end-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-buffer-end-init ()
  "Initialize the `tourist-mode' buffer-end plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-buffer-end-init-buffer ()
  "Initialize a buffer for the `tourist-mode' buffer-end plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-buffer-end-next ()
  "Find the end-of-buffer landmark for `tourist-mode'."
  (widen)
  (list
   (buffer-end 1)
   (buffer-end 1)
   "End of Buffer"))

;;;###autoload
(defun tourist-plugin-buffer-end-previous ()
  "Find the start-of-buffer landmark for `tourist-mode'."
  (widen)
  (list
   (buffer-end -1)
   (buffer-end -1)
   "Start of Buffer"))

(provide 'tourist-plugin-buffer-end)

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

;;; tourist-plugin-buffer-end.el ends here
