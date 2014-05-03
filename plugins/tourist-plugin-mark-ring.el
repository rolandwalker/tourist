;;; tourist-plugin-mark-ring.el --- Mark-ring plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-mark-ring.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `mark-ring' marks in
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
(tourist-register-method 'mark-ring
  '((pretty-name                 . "Recent Mark")
    (priority                    . 10)
    (init-function               . tourist-plugin-mark-ring-init)
    (init-buffer-function        . tourist-plugin-mark-ring-init-buffer)
    (next-landmark-function      . tourist-plugin-mark-ring-next)
    (previous-landmark-function  . tourist-plugin-mark-ring-previous)))

;;; customizable variables

(defcustom tourist-plugin-mark-ring-recent-limit 5
  "How many recent marks from the mark ring to consider as landmarks."
  :type 'number
  :group 'tourist-plugins)

;;; external interface

;;;###autoload
(defun tourist-plugin-mark-ring-init ()
  "Initialize the `tourist-mode' local mark-ring plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-mark-ring-init-buffer ()
  "Initialize a buffer for the `tourist-mode' local mark-ring plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-mark-ring-next (&optional reverse)
  "Find the next local mark-ring landmark for `tourist-mode'."
  (let* ((minmax (if reverse 'max 'min))
         (direction (if reverse '< '>))
         (local-marks (remove-if-not #'(lambda (mark)
                                         (and (number-or-marker-p mark)
                                              ;; mark-ring remembers positions outside our narrowed region
                                              (>= mark (point-min))
                                              (<= mark (point-max))
                                              (funcall direction mark (point))))
                                     (subseq mark-ring 0 tourist-plugin-mark-ring-recent-limit))))
    (when local-marks
      (let ((pos (apply minmax local-marks)))
        (list
         pos
         pos)))))

;;;###autoload
(defun tourist-plugin-mark-ring-previous ()
  "Find the previous local mark-ring landmark for `tourist-mode'."
  (tourist-plugin-mark-ring-next 'reverse))

(provide 'tourist-plugin-mark-ring)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu flet subseq
;;

;;; tourist-plugin-mark-ring.el ends here
