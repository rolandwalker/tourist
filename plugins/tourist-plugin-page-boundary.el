;;; tourist-plugin-page-boundary.el --- Page-delimiter plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-page-boundary.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides `tourist-mode' support for `page-delimiter'.
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
;;     Fails to find page boundary if started on the last position
;;     before the boundary.
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
(tourist-register-method 'page-boundary
  '((pretty-name                 . "Page Boundary")
    (priority                    . 60)
    (init-function               . tourist-plugin-page-boundary-init)
    (init-buffer-function        . tourist-plugin-page-boundary-init-buffer)
    (next-landmark-function      . tourist-plugin-page-boundary-next)
    (previous-landmark-function  . tourist-plugin-page-boundary-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-page-boundary-init ()
  "Initialize the `tourist-mode' page-boundary plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-page-boundary-init-buffer ()
  "Initialize a buffer for the `tourist-mode' page-boundary plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-page-boundary-next ()
  "Find the next page-boundary landmark for `tourist-mode'."
  (when (re-search-forward page-delimiter nil t)
    (list
     (match-beginning 0)
     (match-end 0))))

;;;###autoload
(defun tourist-plugin-page-boundary-previous ()
  "Find the previous page-boundary landmark for `tourist-mode'."
  (when (re-search-backward page-delimiter nil t)
    (list
     (match-beginning 0)
     (match-end 0))))

(provide 'tourist-plugin-page-boundary)

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

;;; tourist-plugin-page-boundary.el ends here
