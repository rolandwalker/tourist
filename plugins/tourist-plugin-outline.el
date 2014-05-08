;;; tourist-plugin-outline.el --- Outline plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-outline.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `outline' headings in
;; `tourist-mode'.
;;
;; It also works with org-mode, which depends on the facilities
;; provided by `outline'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET outline RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     `outline-mode' or `org-mode' must be turned on in a buffer
;;     for this plugin to have any effect.
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;;     Doesn't reach top of buffer when in reverse, seems to get stuck in todo.txt
;;     must re-implement using outline-regexp?
;;
;; TODO
;;
;;     Optionally scan into invisible text in org-mode
;;
;;     Per-mode priority system to stop effects such as imenu overriding
;;     this in org-mode.
;;
;;; License
;;
;; See license in tourist.el
;;
;;; Code:
;;

;;; requirements

;;; declarations

(declare-function tourist-register-method          "tourist.el")
(declare-function outline-previous-visible-heading "outline.el")
(declare-function outline-next-visible-heading     "outline.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'outline
  '((pretty-name                 . "Outline Heading")
    (priority                    . 60)
    (init-function               . tourist-plugin-outline-init)
    (init-buffer-function        . tourist-plugin-outline-init-buffer)
    (next-landmark-function      . tourist-plugin-outline-next)
    (previous-landmark-function  . tourist-plugin-outline-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-outline-init ()
  "Initialize the `tourist-mode' outline plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-outline-init-buffer ()
  "Initialize a buffer for the `tourist-mode' outline plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-outline-next ()
  "Find the next outline landmark for `tourist-mode'."
  (when (derived-mode-p 'outline-mode)
    (outline-next-visible-heading 1)
    (when (looking-at (concat "^" outline-regexp))
      (list
       (point)
       (line-end-position)
       nil ;; todo description
     ))))

;;;###autoload
(defun tourist-plugin-outline-previous ()
  "Find the previous outline landmark for `tourist-mode'."
  (when (derived-mode-p 'outline-mode)
    ;; fuller context is needed or the current line would always be skipped
    (let ((min (point-min))
          (max (point-max)))
      (widen)
      (narrow-to-region min (min (point-max) (+ 2 (line-end-position))))
      (unless (looking-at (concat "^" outline-regexp))
        (forward-line 1)))
    (outline-previous-visible-heading 1)
    (when (looking-at (concat "^" outline-regexp))
      (list
       (point)
       (line-end-position)
       nil ;; todo description
     ))))

(provide 'tourist-plugin-outline)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu
;;

;;; tourist-plugin-outline.el ends here
