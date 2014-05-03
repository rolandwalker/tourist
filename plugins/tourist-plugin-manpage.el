;;; tourist-plugin-manpage.el --- Man-next-manpage plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-manpage.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `Man-next-manpage',
;; as used in `Man-mode' and `woman-mode'.
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
;;     Could provide a callback in tourist-register-method
;;     which allows re-narrowing as done as usual in Man-mode.
;;     Actually not only the narrowing is missing, but the
;;     update to the modeline as usually happens with Man-next-manpage.
;;
;;; License
;;
;; See license in tourist.el
;;
;;; Code:
;;

;;; requirements

;; for remove-if-not
(require 'cl)

;;; declarations

(declare-function tourist-register-method "tourist.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'manpage
  '((pretty-name                 . "Man Page")
    (priority                    . 10)
    (init-function               . tourist-plugin-manpage-init)
    (init-buffer-function        . tourist-plugin-manpage-init-buffer)
    (next-landmark-function      . tourist-plugin-manpage-next)
    (previous-landmark-function  . tourist-plugin-manpage-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-manpage-init ()
  "Initialize the `tourist-mode' Man-page plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-manpage-init-buffer ()
  "Initialize a buffer for the `tourist-mode' Man-page plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-manpage-next (&optional reverse)
  "Find the next Man-page landmark for `tourist-mode'.

This function finds the next man page in a narrowed
buffer containing multiple man pages."
   (when (and (boundp 'Man-page-list)
              Man-page-list
              (derived-mode-p 'Man-mode))
     (let* ((minmax (if reverse 'max 'min))
            (direction (if reverse '< '>))
            (local-marks (remove-if-not #'(lambda (item)
                                            (and (number-or-marker-p item)
                                                 ;; man boundary can be outside our narrowed search region
                                                 (>= item (point-min))
                                                 (<= item (point-max))
                                                 (funcall direction item (point))))
                                        (mapcar #'car-safe Man-page-list))))
       (when local-marks
         (let ((pos (apply minmax local-marks)))
           (list
            pos
            pos))))))

;;;###autoload
(defun tourist-plugin-manpage-previous ()
  "Find the previous Man-page landmark for `tourist-mode'."
  (tourist-plugin-manpage-next 'reverse))

(provide 'tourist-plugin-manpage)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu flet manpage
;;

;;; tourist-plugin-manpage.el ends here
