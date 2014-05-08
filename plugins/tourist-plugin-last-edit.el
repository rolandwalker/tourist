;;; tourist-plugin-last-edit.el --- Last-edit plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-last-edit.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for a "last-edit" landmark in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;
;; Notes
;;
;;     This overlaps with the `tourist-track-changes' plugin.  However,
;;     this plugin works without any dependency, in any mode.
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;; TODO
;;
;;     Learn to read the buffer-undo-tree structure directly instead of
;;     using the shenanigans below.
;;
;;     Make sure copy-tree has no problem with circular structures.
;;
;;     Probably should be using undo-copy-list to copy buffer-undo-list.
;;
;;; License
;;
;; See license in tourist.el
;;
;;; Code:
;;

;;; requirements

;; for remove-if
(require 'cl)

;;; declarations

(declare-function tourist-register-method     "tourist.el")
(declare-function undo-list-rebuild-from-tree "undo-tree.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'last-edit
  '((pretty-name                 . "Site of Last Edit")
    (priority                    . 10)
    (init-function               . tourist-plugin-last-edit-init)
    (init-buffer-function        . tourist-plugin-last-edit-init-buffer)
    (next-landmark-function      . tourist-plugin-last-edit-next)
    (previous-landmark-function  . tourist-plugin-last-edit-previous)))

;;; variables

(defvar tourist-plugin-last-edit-undo-cache nil
  "Copy of `buffer-undo-list' generated from `undo-list-rebuild-from-tree'.")
(make-variable-buffer-local 'tourist-plugin-last-edit-undo-cache)

;;; internal utility functions

;; todo learn to read buffer-undo-tree directly - all we need is the single most recent item
(defun tourist-plugin-last-edit--copy-of-undo-list ()
  "Create a temporary copy of `buffer-undo-list', pulling info from undo-tree if needed."
  (cond
    ((and tourist-plugin-last-edit-undo-cache
          (memq last-command tourist-navigation-commands))
     tourist-plugin-last-edit-undo-cache)
    ((and (boundp 'buffer-undo-tree)
          buffer-undo-tree
          (< (length (remq nil buffer-undo-list)) 4)
          (memq 'undo-tree-canary buffer-undo-list))
     (let ((buffer-undo-tree (copy-tree buffer-undo-tree))
           (buffer-undo-list (copy-tree buffer-undo-list)))
       (undo-list-rebuild-from-tree)
       (setq tourist-plugin-last-edit-undo-cache buffer-undo-list)
       buffer-undo-list))
    (t
     buffer-undo-list)))

;;; external interface

;;;###autoload
(defun tourist-plugin-last-edit-init ()
  "Initialize the `tourist-mode' last-edit plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-last-edit-init-buffer ()
  "Initialize a buffer for the `tourist-mode' last-edit plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-last-edit-next (&optional reverse)
  "Find the next last-edit landmark for `tourist-mode'."
  (when (and buffer-undo-list
             (listp buffer-undo-list)
             (not (derived-mode-p 'eshell-mode 'shell-mode 'term-mode 'comint-mode 'slime-repl-mode 'nrepl-mode)))
    (let ((buffer-undo-list (tourist-plugin-last-edit--copy-of-undo-list)))
      (let ((pos
             (let ((item (car (remove-if #'(lambda (i) (and (listp i) (null (car i)))) buffer-undo-list))))
               ;; see the various forms for `buffer-undo-list' to understand these conditions
               (cond
                 ((integer-or-marker-p item)
                  item)
                 ((not (listp item))
                  nil)
                 ((and (consp item)
                       (markerp (car item))
                       (integerp (cdr item)))
                  (car item))
                 ((and (consp item)
                       (integer-or-marker-p (car item))
                       (integer-or-marker-p (cdr item)))
                  ;; the only difference between forward and reverse -- but is this correct
                  ;; so far as finding only the start-of-landmark? @@@
                  (funcall (if reverse 'max 'min) (car item) (cdr item)))
                 ((and (consp item)
                       (stringp (car item))
                       (integer-or-marker-p (abs (cdr item))))
                  (abs (cdr item)))
                 ((and (consp item)
                       (eq 'apply (car item)))
                  (nth 2 item))))))
        (when (and pos
                   (integer-or-marker-p pos)
                   ;; last change could be outside our narrowed search region
                   (>= pos (point-min))
                   (<= pos (point-max)))
          (list
           pos
           pos))))))

;;;###autoload
(defun tourist-plugin-last-edit-previous ()
  "Find the previous last-edit landmark for `tourist-mode'."
  (tourist-plugin-last-edit-next 'reverse))

(provide 'tourist-plugin-last-edit)

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

;;; tourist-plugin-last-edit.el ends here
