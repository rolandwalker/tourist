;;; tourist-plugin-wiki-nav.el --- Wiki-nav plugin for tourist-mode
;;
;; Copyright (c) 2014-2015 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-wiki-nav.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `wiki-nav' links in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET wiki-nav RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     To activate wiki-nav links as needed by this plugin, your
;;     ~/.emacs should contain something like
;;
;;         (require 'wiki-nav)
;;         (global-wiki-nav-mode 1)
;;
;;     If `wiki-nav-mode' is not turned on in a given buffer, this
;;     plugin will have no effect.
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
(declare-function wiki-nav-find-any-link  "wiki-nav.el")
(declare-function button-lock-find-extent "button-lock.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'wiki-nav
  '((pretty-name                 . "Wiki-nav Link")
    (priority                    . 80)
    (init-function               . tourist-plugin-wiki-nav-init)
    (init-buffer-function        . tourist-plugin-wiki-nav-init-buffer)
    (next-landmark-function      . tourist-plugin-wiki-nav-next)
    (previous-landmark-function  . tourist-plugin-wiki-nav-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-wiki-nav-init ()
  "Initialize the `tourist-mode' wiki-nav plugin."
  ;; don't load wiki-nav.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-wiki-nav-init-buffer ()
  "Initialize a buffer for the `tourist-mode' wiki-nav plugin."
  ;; don't turn on wiki-nav-mode here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-wiki-nav-next ()
  "Find the next wiki-nav landmark for `tourist-mode'."
  (when (and (fboundp 'wiki-nav-find-any-link)
             wiki-nav-mode)
    (let ((nav-flash-delay 0)
          (wiki-nav-less-feedback t)
          (orig-pos (point))
          (extent nil))
      ;; bug: w-n-find-any-link is a little too clever, discounting the current link
      ;;      if the point is on the brackets or even on the space before it.
      ;;      This causes inconsistency with tourist-plugin-wiki-nav-previous. @@@
      (wiki-nav-find-any-link 1)
      (when (and (> (point) orig-pos) ; workaround for stupid autowrapping
                 (get-text-property (point) 'wiki-nav))
        ;; @@@ todo is end of extent consistent with overlay-end and match-end, or off by 1?
        (setq extent (button-lock-find-extent (point) 'wiki-nav))
        (list
         (max (point-min) (car extent))
         (cdr extent)
         nil ;; todo meaningful description
         )))))

;;;###autoload
(defun tourist-plugin-wiki-nav-previous ()
  "Find the previous wiki-nav landmark for `tourist-mode'."
  (when (and (fboundp 'wiki-nav-find-any-link)
             wiki-nav-mode)
    (let ((nav-flash-delay 0)
          (wiki-nav-less-feedback t)
          (orig-pos (point))
          (extent (button-lock-find-extent (point) 'wiki-nav)))
      (cond
        ((and extent
              (< (car extent) (point)))
         (list
          (max (point-min) (car extent))
          (cdr extent)))
        (t
         (wiki-nav-find-any-link -1)
         (when (and (< (point) orig-pos) ; workaround for stupid autowrapping
                    (get-text-property (point) 'wiki-nav))
           (setq extent (button-lock-find-extent (point) 'wiki-nav))
           (list
            (max (point-min) (car extent))
            (cdr extent)
            nil ;; todo meaningful description
            )))))))

(provide 'tourist-plugin-wiki-nav)

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

;;; tourist-plugin-wiki-nav.el ends here
