;;; tourist-plugin-global-mark-ring.el --- Global-mark-ring plugin for tourist-mode
;;
;; Copyright (c) 2014-2015 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-global-mark-ring.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `global-mark-ring' marks in
;; `tourist-mode'.
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

;;; requirements

;; for remove-if-not, subseq
(require 'cl)

;;; declarations

(declare-function tourist-register-method "tourist.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'global-mark-ring
  '((pretty-name                 . "Recent Global Mark")
    (priority                    . 10)
    (init-function               . tourist-plugin-global-mark-ring-init)
    (init-buffer-function        . tourist-plugin-global-mark-ring-init-buffer)
    (next-landmark-function      . tourist-plugin-global-mark-ring-next)
    (previous-landmark-function  . tourist-plugin-global-mark-ring-previous)))

;;; customizable variables

(defcustom tourist-plugin-global-mark-ring-recent-limit 5
  "How many recent marks from `global-mark-ring' to consider as landmarks.

This limit applies per-buffer."
  :type 'number
  :group 'tourist-plugins)

;;; internal utility functions

(defun tourist-plugin-global-mark-ring--marks-in-current-buffer ()
  "Return the members of `global-mark-ring' located in the current buffer."
  (remove-if-not #'(lambda (mark)
                     (eq (current-buffer) (marker-buffer mark)))
                 global-mark-ring))

;;; external interface

;;;###autoload
(defun tourist-plugin-global-mark-ring-init ()
  "Initialize the `tourist-mode' global-mark-ring plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-global-mark-ring-init-buffer ()
  "Initialize a buffer for the `tourist-mode' global-mark-ring plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-global-mark-ring-next (&optional reverse)
  "Find the next global-mark-ring landmark for `tourist-mode'."
  (let* ((minmax (if reverse 'max 'min))
         (direction (if reverse '< '>))
         (local-marks (remove-if-not #'(lambda (mark)
                                         (and mark
                                              ;; global-mark-ring remembers positions outside our narrowed region
                                              (>= mark (point-min))
                                              (<= mark (point-max))
                                              (funcall direction mark (point))))
                                     (subseq (tourist-plugin-global-mark-ring--marks-in-current-buffer) 0 tourist-plugin-global-mark-ring-recent-limit))))
    (when local-marks
      (let ((pos (apply minmax local-marks)))
        (list
         pos
         pos)))))

;;;###autoload
(defun tourist-plugin-global-mark-ring-previous ()
  "Find the previous global-mark-ring landmark for `tourist-mode'."
  (tourist-plugin-global-mark-ring-next 'reverse))

(provide 'tourist-plugin-global-mark-ring)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu subseq
;;

;;; tourist-plugin-global-mark-ring.el ends here
