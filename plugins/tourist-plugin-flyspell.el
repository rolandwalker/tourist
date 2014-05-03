;;; tourist-plugin-flyspell.el --- Flyspell plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-flyspell.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `flyspell' errors in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET flyspell RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     To activate flyspell as needed by this plugin, your
;;     ~/.emacs should contain something like
;;
;;         (require 'flyspell)
;;
;;     Additional action may be needed to turn on `flyspell-mode'
;;     in a given buffer.
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;;     Tourist-mode only allows line-by-line navigation.  The user might
;;     prefer to navigate to every misspelled word.
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
(declare-function flyspell-overlay-p      "flyspell.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'flyspell
  '((pretty-name                 . "Spelling Error")
    (priority                    . 50)
    (init-function               . tourist-plugin-flyspell-init)
    (init-buffer-function        . tourist-plugin-flyspell-init-buffer)
    (next-landmark-function      . tourist-plugin-flyspell-next)
    (previous-landmark-function  . tourist-plugin-flyspell-previous)))

;;; internal utility functions

(defun tourist-plugin-flyspell--has-overlay (&optional pos)
  "If POS has a flyspell overlay, return the overlay.

POS is optional, and defaults to the current point."
  (callf or pos (point))
  (catch 'saw
    (dolist (ov (overlays-at pos))
      (when (flyspell-overlay-p ov)
        (throw 'saw ov)))
    nil))

;;; external interface

;;;###autoload
(defun tourist-plugin-flyspell-init ()
  "Initialize the `tourist-mode' flyspell plugin."
  ;; don't load flyspell.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-flyspell-init-buffer ()
  "Initialize a buffer for the `tourist-mode' flyspell plugin."
  ;; don't turn on flyspell-mode here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-flyspell-next ()
  "Find the next flyspell landmark for `tourist-mode'."
  (when (and (fboundp 'flyspell-overlay-p)
             flyspell-mode)
    (let ((putative-start (point)))
      ;; leave current error if in one
      (while (and (< putative-start (point-max))
                  (tourist-plugin-flyspell--has-overlay putative-start))
        (callf next-overlay-change putative-start))
      ;; find next error
      (while (and (< putative-start (point-max))
                  (not (tourist-plugin-flyspell--has-overlay putative-start)))
        (callf next-overlay-change putative-start))
      (let ((fl-ov (tourist-plugin-flyspell--has-overlay putative-start)))
        ;; success
        (when fl-ov
          (list
           (overlay-start fl-ov)
           (overlay-end fl-ov)
           nil                  ;; todo meaningful description
           ))))))

;;;###autoload
(defun tourist-plugin-flyspell-previous ()
  "Find the previous flyspell landmark for `tourist-mode'."
  (when (fboundp 'flyspell-overlay-p)
    (let ((putative-start (previous-overlay-change (point))))
      (while (and putative-start
                (> putative-start (point-min))
                (not (tourist-plugin-flyspell--has-overlay putative-start)))
        (callf previous-overlay-change putative-start))
      (let ((fl-ov (tourist-plugin-flyspell--has-overlay putative-start)))
        (when fl-ov
          (list
           (overlay-start fl-ov)
           (overlay-end fl-ov)
           nil                  ;; todo meaningful description
           ))))))


(provide 'tourist-plugin-flyspell)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu flet callf
;;

;;; tourist-plugin-flyspell.el ends here
