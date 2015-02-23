;;; tourist-plugin-flymake.el --- Flymake plugin for tourist-mode
;;
;; Copyright (c) 2014-2015 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-flymake.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `flymake' overlays in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET flymake RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     To activate flymake as needed by this plugin, your
;;     ~/.emacs should contain something like
;;
;;         (require 'flymake)
;;
;;     Additional action may be required to turn on flymake in
;;     a given buffer.
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;; TODO
;;
;;     Can flymake-err-line return a line outside our narrowed region?
;;
;;; License
;;
;; See license in tourist.el
;;
;;; Code:
;;

;;; declarations

(declare-function tourist-register-method       "tourist.el")
(declare-function flymake-get-prev-err-line-no  "flymake.el")
(declare-function flymake-current-line-no       "flymake.el")
(declare-function flymake-get-next-err-line-no  "flymake.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'flymake
  '((pretty-name                 . "Flymake Error")
    (priority                    . 50)
    (init-function               . tourist-plugin-flymake-init)
    (init-buffer-function        . tourist-plugin-flymake-init-buffer)
    (next-landmark-function      . tourist-plugin-flymake-next)
    (previous-landmark-function  . tourist-plugin-flymake-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-flymake-init ()
  "Initialize the `tourist-mode' flymake plugin."
  ;; don't load flymake.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-flymake-init-buffer ()
  "Initialize a buffer for the `tourist-mode' flymake plugin."
  ;; don't turn on flymake-mode here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-flymake-next ()
  "Find the next flymake landmark for `tourist-mode'."
  (when (and (boundp 'flymake-err-info)
             flymake-mode)
    (let ((flymake-err-line (flymake-get-next-err-line-no flymake-err-info (flymake-current-line-no))))
      (when flymake-err-line
        (goto-char (point-min))
        (forward-line (1- flymake-err-line))
        (list
         (point))))))

;;;###autoload
(defun tourist-plugin-flymake-previous ()
  "Find the previous flymake landmark for `tourist-mode'."
  (when (and (boundp 'flymake-err-info)
             flymake-mode)
    (let ((flymake-err-line (flymake-get-prev-err-line-no flymake-err-info (flymake-current-line-no))))
      (when flymake-err-line
        (goto-char (point-min))
        (forward-line (1- flymake-err-line))
        (list
         (point))))))

(provide 'tourist-plugin-flymake)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu flymake Flymake
;;

;;; tourist-plugin-flymake.el ends here
