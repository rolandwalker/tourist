;;; tourist-plugin-bm.el --- Bm bookmarks plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-bm.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for "bm" bookmarks in `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET bm RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     To activate "bm" bookmarks as needed by this plugin, your
;;     ~/.emacs should contain something like
;;
;;         (require 'bm)
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

(eval-and-compile
  ;; for flet/cl-flet*
  (require 'cl)
  (unless (fboundp 'cl-flet*)
    (defalias 'cl-flet* 'flet)
    (put 'cl-flet* 'lisp-indent-function 1)
    (put 'cl-flet* 'edebug-form-spec '((&rest (defun*)) cl-declarations body))))

;;; declarations

(declare-function tourist-register-method "tourist.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'bm
  '((pretty-name                 . "Bm Bookmark")
    (priority                    . 20)
    (init-function               . tourist-plugin-bm-init)
    (init-buffer-function        . tourist-plugin-bm-init-buffer)
    (next-landmark-function      . tourist-plugin-bm-next)
    (previous-landmark-function  . tourist-plugin-bm-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-bm-init ()
  "Initialize the `tourist-mode' bm-bookmark plugin."
  ;; don't load bm.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-bm-init-buffer ()
  "Initialize a buffer for the `tourist-mode' bm-bookmark plugin."
  ;; per-buffer setup not needed
  t)

;;;###autoload
(defun tourist-plugin-bm-next ()
  "Find the next bm-bookmark landmark for `tourist-mode'."
  (when (fboundp 'bm-next)
    (let ((bm-cycle-all-buffers nil)
          (bm-wrap-search nil)
          (nav-flash-delay 0)
          (annotation nil))
      (cl-flet* ((message (&rest _ignored)
                          t))
        (setq annotation (bm-next)))
      (list
       (point)
       (point)
       (when annotation
         (format "Bm Bookmark - %s" annotation))))))

;;;###autoload
(defun tourist-plugin-bm-previous ()
  "Find the previous bm-bookmark landmark for `tourist-mode'."
  (when (fboundp 'bm-previous)
    (let ((bm-cycle-all-buffers nil)
          (bm-wrap-search nil)
          (nav-flash-delay 0)
          (annotation nil))
      (cl-flet* ((message (&rest _ignored)
                          t))
        (setq annotation (bm-previous))
      (list
       (point)
       (point)
       (when annotation
         (format "Bm Bookmark - %s" annotation)))))))

(provide 'tourist-plugin-bm)

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

;;; tourist-plugin-bm.el ends here
