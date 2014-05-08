;;; tourist-plugin-warning-face.el --- Warning-face plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-warning-face.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `font-lock-warning-face' in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET font-lock-faces RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;;     This plugin can be quite slow in large buffers, especially when
;;     warning-face is not present, so its priority should be late.
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
(tourist-register-method 'warning-face
  '((pretty-name                 . "Font-lock Warning Face")
    (priority                    . 90)
    (init-function               . tourist-plugin-warning-face-init)
    (init-buffer-function        . tourist-plugin-warning-face-init-buffer)
    (next-landmark-function      . tourist-plugin-warning-face-next)
    (previous-landmark-function  . tourist-plugin-warning-face-previous)))

;;; internal utility functions

(defun tourist-plugin-warning-face--has-face (&optional pos)
  "Whether POS has `font-lock-warning-face'.

POS is optional, and defaults to the current point."
  (callf or pos (point))
  (funcall
   (if (listp (get-text-property pos 'face)) 'memq 'eq)
   'font-lock-warning-face
   (get-text-property pos 'face)))

(defun tourist-plugin-warning-face--find-end (pos)
  "Given POS, find the last contiguous position with `font-lock-warning-face'.

The position at POS *must* have `font-lock-warning-face'."
  (let ((end pos))
    (while (and end
                (< end (point-max))
                (tourist-plugin-warning-face--has-face end))
      (callf next-single-property-change end 'face nil (point-max)))
    end))

;;; external interface

;;;###autoload
(defun tourist-plugin-warning-face-init ()
  "Initialize the `tourist-mode' font-lock-warning-face plugin."
  ;; don't set up font-lock -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-warning-face-init-buffer ()
  "Initialize a buffer for the `tourist-mode' font-lock-warning-face plugin."
  ;; don't turn on font-lock-mode here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-warning-face-next ()
  "Find the next font-lock-warning-face landmark for `tourist-mode'."
  (when font-lock-mode
    (let ((putative-start (point))
          (end nil))
      ;; Leave the property if we are in one.  This is different logic
      ;; than `tourist-plugin-warning-face-previous' because tourist navigates
      ;; to start-of-landmark, never to end-of-landmark.
      (while (and putative-start
                  (< putative-start (point-max))
                  (tourist-plugin-warning-face--has-face putative-start))
        (callf next-single-property-change putative-start 'face nil (point-max)))
      ;; find the next property start
      (while (and putative-start
                  (< putative-start (point-max))
                  (not (tourist-plugin-warning-face--has-face putative-start)))
        (callf next-single-property-change putative-start 'face nil (point-max)))
      ;; success
      (when (and putative-start
                 (tourist-plugin-warning-face--has-face putative-start))
        (list
         putative-start
         (tourist-plugin-warning-face--find-end putative-start))))))

;;;###autoload
(defun tourist-plugin-warning-face-previous ()
  "Find the previous font-lock-warning-face landmark for `tourist-mode'."
  (when font-lock-mode
    (let ((putative-start (previous-single-property-change (point) 'face nil (point-min))))
      ;; find the next property start
      (while (and putative-start
                  (> putative-start (point-min))
                  (not (tourist-plugin-warning-face--has-face putative-start)))
        (callf previous-single-property-change putative-start 'face nil (point-min)))
      ;; success
      (when (and putative-start
                 (tourist-plugin-warning-face--has-face putative-start))
        (list
         putative-start
         (tourist-plugin-warning-face--find-end putative-start))))))

(provide 'tourist-plugin-warning-face)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;; LocalWords: imenu flet callf
;;

;;; tourist-plugin-warning-face.el ends here
