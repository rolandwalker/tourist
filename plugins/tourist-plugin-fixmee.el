;;; tourist-plugin-fixmee.el --- Fixmee notice plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-fixmee.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for "fixme" notices in `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET fixmee RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     To activate `fixmee-mode' as needed by this plugin, your
;;     ~/.emacs should contain something like
;;
;;         (require 'fixmee)
;;         (global-fixmee-mode 1)
;;
;;     If `fixmee-mode' is not active in a given buffer, this
;;     plugin will have no effect.
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
(tourist-register-method 'fixmee
  '((pretty-name                 . "Fixme Notice")
    (priority                    . 20)
    (init-function               . tourist-plugin-fixmee-init)
    (init-buffer-function        . tourist-plugin-fixmee-init-buffer)
    (next-landmark-function      . tourist-plugin-fixmee-next)
    (previous-landmark-function  . tourist-plugin-fixmee-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-fixmee-init ()
  "Initialize the `tourist-mode' fixmee.el plugin."
  ;; don't load fixmee.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-fixmee-init-buffer ()
  "Initialize a buffer for the `tourist-mode' fixmee.el plugin."
  ;; don't turn on fixmee-mode here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-fixmee-next ()
  "Find the next fixmee.el landmark for `tourist-mode'."
  (when (and (fboundp 'fixmee-find-next-by-position)
             fixmee-mode)
    (let ((notice (fixmee-find-next-by-position)))
      (when notice
        (list
         (nth 2 notice)
         (nth 3 notice)
         (format "Fixme notice: urgency %s" (nth 0 notice)))))))

;;;###autoload
(defun tourist-plugin-fixmee-previous ()
  "Find the previous fixmee.el landmark for `tourist-mode'."
  (when (and (fboundp 'fixmee-find-previous-by-position)
             fixmee-mode)
    (let ((notice (fixmee-find-previous-by-position)))
      (when notice
        (list
         (nth 2 notice)
         (nth 3 notice)
         (format "Fixme notice: urgency %s" (nth 0 notice)))))))

(provide 'tourist-plugin-fixmee)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu flet fixmee Fixme
;;

;;; tourist-plugin-fixmee.el ends here
