;;; tourist-plugin-linkd.el --- Linkd plugin for tourist-mode
;;
;; Copyright (c) 2014-2015 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-linkd.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `linkd-mode' links in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET linkd RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     To activate linkd links as needed by this plugin, your
;;     ~/.emacs should contain something like
;;
;;         (require 'linkd)
;;
;;     Additional action may be needed to turn on `linkd-mode'
;;     in a given buffer.
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

;;; requirements

;; for callf
(require 'cl)

;;; declarations

(declare-function tourist-register-method "tourist.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'linkd
  '((pretty-name                 . "Linkd Link")
    (priority                    . 50)
    (init-function               . tourist-plugin-linkd-init)
    (init-buffer-function        . tourist-plugin-linkd-init-buffer)
    (next-landmark-function      . tourist-plugin-linkd-next)
    (previous-landmark-function  . tourist-plugin-linkd-previous)))

(defun tourist-plugin-linkd--find-end (pos)
  "Given POS, find the last contiguous position with 'linkd char-property.

The position at POS *must* have the 'linkd char-property."
  (let ((end pos))
    (while (and end
                (< end (point-max))
                (get-char-property end 'linkd))
      (callf next-single-char-property-change end 'linkd nil (point-max)))
    end))

;;; external interface

;;;###autoload
(defun tourist-plugin-linkd-init ()
  "Initialize the `tourist-mode' linkd plugin."
  ;; don't load linkd.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-linkd-init-buffer ()
  "Initialize a buffer for the `tourist-mode' linkd plugin."
  ;; don't turn on linkd-mode here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-linkd-next ()
  "Find the next linkd landmark for `tourist-mode'."
  (when (and (fboundp 'linkd-next-link)
             linkd-mode)
    (linkd-next-link)
    (when (get-char-property (point) 'linkd)
      (list
       (point)
       (tourist-plugin-linkd--find-end (point))))))

;;;###autoload
(defun tourist-plugin-linkd-previous ()
  "Find the previous linkd landmark for `tourist-mode'."
  (when (and (fboundp 'linkd-previous-link)
             linkd-mode)
    (linkd-previous-link)
    (when (get-char-property (point) 'linkd)
      (list
        (point)
        (tourist-plugin-linkd--find-end (point))))))

(provide 'tourist-plugin-linkd)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu linkd callf Linkd
;;

;;; tourist-plugin-linkd.el ends here
