;;; tourist-plugin-folding-mode.el --- Folding-mode plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.github.com/rolandwalker/tourist/master/plugins/tourist-plugin-folding-mode.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `folding-mode' markers in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET folding RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     To activate code folding as needed by this plugin, your
;;     ~/.emacs should contain something like
;;
;;         (require 'folding)
;;
;;     Additional action may be needed to turn on folding
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

;;; declarations

(declare-function tourist-register-method "tourist.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'folding-mode
  '((pretty-name                 . "Folding Marker")
    (priority                    . 60)
    (init-function               . tourist-plugin-folding-mode-init)
    (init-buffer-function        . tourist-plugin-folding-mode-init-buffer)
    (next-landmark-function      . tourist-plugin-folding-mode-next)
    (previous-landmark-function  . tourist-plugin-folding-mode-previous)))

;;; internal utility functions

(defun tourist-plugin-folding-mode--get-mode-marks ()
  "Return a string holding `folding-mode' mark regexp for the buffer."
  (if (and (boundp 'folding-mode-marks-alist)
           (assq major-mode folding-mode-marks-alist)
           (fboundp 'folding-get-mode-marks))
      (concat "^" (regexp-quote (car (folding-get-mode-marks))))
    ;; fallback
    "^\\s<\\s<\\s<{{{"))

;;; external interface

;;;###autoload
(defun tourist-plugin-folding-mode-init ()
  "Initialize the `tourist-mode' folding-mode plugin."
  ;; don't load folding.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-folding-mode-init-buffer ()
  "Initialize a buffer for the `tourist-mode' folding-mode plugin."
  ;; don't turn on folding-mode here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-folding-mode-next ()
  "Find the next folding-mode landmark for `tourist-mode'."
  (when (re-search-forward (tourist-plugin-folding-mode--get-mode-marks) nil t)
    (list
     (match-beginning 0)
     (match-end 0))))

;;;###autoload
(defun tourist-plugin-folding-mode-previous ()
  "Find the previous folding-mode landmark for `tourist-mode'."
  (when (re-search-backward (tourist-plugin-folding-mode--get-mode-marks) nil t)
    (list
     (match-beginning 0)
     (match-end 0))))

(provide 'tourist-plugin-folding-mode)

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

;;; tourist-plugin-folding-mode.el ends here
