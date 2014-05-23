;;; tourist-plugin-track-changes.el --- Track-changes plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-track-changes.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `hilit-chg' change tracking in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET highlight-changes RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el.
;;
;;     To activate change tracking as needed by this plugin, your
;;     ~/.emacs should contain something like
;;
;;         (require 'hilit-chg)
;;         (highlight-changes-mode t)
;;         (highlight-changes-visible-mode t)
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;; TODO
;;
;;     Give user the option of activating this even when visible-mode
;;     is not on.
;;
;;     Give user the option of turning on visible-mode transiently
;;     whenever tourist is used
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
(tourist-register-method 'track-changes
  '((pretty-name                 . "Edit Since Last Save")
    (priority                    . 50)
    (init-function               . tourist-plugin-track-changes-init)
    (init-buffer-function        . tourist-plugin-track-changes-init-buffer)
    (next-landmark-function      . tourist-plugin-track-changes-next)
    (previous-landmark-function  . tourist-plugin-track-changes-previous)))

;;; internal utility functions

(defun tourist-plugin-track-changes--find-end (pos)
  "Given POS, find the last contiguous position with 'hilit-chg property.

The position at POS *must* have the 'hilit-chg property."
  (let ((end pos))
    (while (and end
                (< end (point-max))
                (get-text-property end 'hilit-chg))
      (callf next-single-property-change end 'hilit-chg nil (point-max)))
  end))

;;; external interface

;;;###autoload
(defun tourist-plugin-track-changes-init ()
  "Initialize the `tourist-mode' track-changes plugin."
  ;; don't load hilit-chg.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-track-changes-init-buffer ()
  "Initialize a buffer for the `tourist-mode' track-changes plugin."
  ;; don't turn on highlight-changes-visible-mode here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-track-changes-next ()
  "Find the next track-changes landmark for `tourist-mode'."
  (when (and (boundp 'highlight-changes-visible-mode)
             highlight-changes-visible-mode)
    (let ((putative-start (point)))
      ;; Leave the property if we are in one.  This is different logic
      ;; than `tourist-plugin-track-changes-previous' because tourist navigates
      ;; to start-of-landmark, never to end-of-landmark.
      (while (and putative-start
                  (< putative-start (point-max))
                  (get-text-property putative-start 'hilit-chg))
        (callf next-single-property-change putative-start 'hilit-chg nil (point-max)))
      ;; find the next property start
      (while (and putative-start
                  (< putative-start (point-max))
                  (not (get-text-property putative-start 'hilit-chg)))
        (callf next-single-property-change putative-start 'hilit-chg nil (point-max)))
      ;; success
      (when (and putative-start
                 (get-text-property putative-start 'hilit-chg))

        (list
         putative-start
         (tourist-plugin-track-changes--find-end putative-start)
         nil ; todo description could say when value of prop is 'hilit-chg-delete
         )))))

;;;###autoload
(defun tourist-plugin-track-changes-previous ()
  "Find the previous track-changes landmark for `tourist-mode'."
  (when (and (boundp 'highlight-changes-visible-mode)
             highlight-changes-visible-mode)
    (let ((putative-start (previous-single-property-change (point) 'hilit-chg nil (point-min))))
    ;; find the next property start
    (while (and putative-start
                (> putative-start (point-min))
                (not (get-text-property putative-start 'hilit-chg)))
      (callf previous-single-property-change putative-start 'hilit-chg nil (point-min)))
    ;; success
    (when (and putative-start
               (get-text-property putative-start 'hilit-chg))
      (list
       putative-start
       (tourist-plugin-track-changes--find-end putative-start)
       nil ; todo description could say when value of prop is 'hilit-chg-delete
       )))))

(provide 'tourist-plugin-track-changes)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu hilit callf
;;

;;; tourist-plugin-track-changes.el ends here
