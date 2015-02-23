;;; tourist-plugin-imenu.el --- Imenu plugin for tourist-mode
;;
;; Copyright (c) 2014-2015 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-imenu.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for `imenu' in `tourist-mode'.
;;
;; Unlike most tourist plugins, this does something behind the
;; user's back: it will load imenu and scan the buffer.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET imenu RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     Imenu supports many modes, but not all.  If imenu cannot
;;     parse a buffer, this plugin will have no effect.
;;
;; Bugs
;;
;;     The description "Imenu Definition" is not very useful.  For
;;     example, imenu is supported by org-mode, where it navigates
;;     to outline headings.
;;
;; TODO
;;
;; Plugin Interface - @@@ needs rewriting after changes
;;
;;     Next-landmark-function and previous-landmark-function should
;;     return a list of the form (START END DESCRIPTION) on success,
;;     where START and END are positions in the buffer usable by
;;     `goto-char', ie integers or markers.  END and DESCRIPTION are
;;     optional.  START and END describe the extent of the landmark.
;;     START should be <= END, regardless of the direction of the
;;     navigation function.  On failure, nil should be returned.
;;
;;     The caller guarantees preservation of windows, point/mark,
;;     restriction, and match-data.  No need to save-excursion within
;;     the plugin.
;;
;;     From the perspective of next-landmark-function, the point
;;     is at the user's current point in a narrowed region in which
;;     `point-max' is the position of the closest landmark which has
;;     been found (so far).  next-landmark-function should attempt
;;     to detect a landmark between `point' and `point-max'.
;;
;;     The plugin may widen the restriction if more context is
;;     needed to work correctly.
;;
;;     previous-landmark-function and next-landmark-function should
;;     not attempt to navigate to a new buffer, but only return the
;;     previous/next landmark in the current buffer.
;;
;;; License
;;
;; See license in tourist.el
;;
;;; Code:
;;

;;; requirements

(require 'cl)

;;; declarations

(declare-function tourist-register-method  "tourist.el")
(declare-function imenu--cleanup           "imenu.el")
(declare-function imenu--make-index-alist  "imenu.el")
(declare-function imenu--subalist-p        "imenu.el")

(eval-when-compile
  (defvar imenu--index-alist))

;;; register the plugin

;;;###autoload
(tourist-register-method 'imenu
  '((pretty-name                 . "Imenu Definition")                 ; used for feedback and menu, title case
    (priority                    . 15)
    (init-function               . tourist-plugin-imenu-init)          ; executed only once
    (init-buffer-function        . tourist-plugin-imenu-init-buffer)   ; executed once for each buffer
    (next-landmark-function      . tourist-plugin-imenu-next)          ; call to find the next landmark in buffer
    (previous-landmark-function  . tourist-plugin-imenu-previous)))    ; call to find the previous landmark in buffer

(defcustom tourist-plugin-imenu-submenus t
  "Whether to traverse submenus of imenu for additional definitions."
  :type 'number
  :group 'tourist-plugins)

;;; internal utility functions

(defun tourist-plugin-imenu--alist-flatten (list)
  "Flatten LIST, which may contain other lists.  Do not flatten cons cells.

It is not guaranteed that the result contains *only* cons cells.
The result could contain other data types present in LIST.

This function simply avoids flattening single conses or improper
lists where the last two elements would be expressed as a dotted
pair."
  (cond
    ((null list)
     nil)
    ((and (consp list)
          (not (listp (nthcdr (safe-length list) list))))
     list)
    ((and (listp list)
          (consp (car list))
          (listp (nthcdr (safe-length (car list)) (car list))))
     (append (tourist-plugin-imenu--alist-flatten (car list))
             (tourist-plugin-imenu--alist-flatten (cdr list))))
    ((listp list)
     (cons (car list)
           (tourist-plugin-imenu--alist-flatten (cdr list))))
    (t
     (list list))))

(defun tourist-plugin-imenu--extract-definitions ()
  "Return an alist of all imenu definitions in the current buffer.

If `tourist-plugin-imenu-submenus' is non-nil, traverse submenus
for additional definitions.  Otherwise return only top-level
definitions.

The return value is an alist of cells in the form
\(STRING . MARKER)."
  (if tourist-plugin-imenu-submenus
      (remove-if-not 'consp (tourist-plugin-imenu--alist-flatten imenu--index-alist))
    (remove-if 'imenu--subalist-p imenu--index-alist)))

;;; external interface

;;;###autoload
(defun tourist-plugin-imenu-init ()
  "Initialize the `tourist-mode' imenu plugin."
  (require 'imenu))

;;;###autoload
(defun tourist-plugin-imenu-init-buffer ()
  "Initialize a buffer for the `tourist-mode' imenu plugin."
  (ignore-errors
    (imenu--cleanup))
  (with-demoted-errors
    (setq imenu--index-alist nil)
    (imenu--make-index-alist)))

;;;###autoload
(defun tourist-plugin-imenu-next (&optional reverse)
  "Find the next imenu landmark for `tourist-mode'."
   (when (and (boundp 'imenu--index-alist)
              imenu--index-alist)
     (let* ((minmax (if reverse 'max 'min))
            (direction (if reverse '< '>))
            (local-marks (delq nil (mapcar #'(lambda (item)
                                              (when (and item
                                                         (not (imenu--subalist-p item))
                                                         ;; imenu remembers positions outside the narrowed search region
                                                         (>= (cdr item) (point-min))
                                                         (<= (cdr item) (point-max))
                                                         (funcall direction (cdr item) (point)))
                                                (cdr item))) (tourist-plugin-imenu--extract-definitions)))))
       (when local-marks
         (let ((pos (apply minmax local-marks)))
           (list
            pos
            pos
            nil    ; todo useful description
            ))))))

;;;###autoload
(defun tourist-plugin-imenu-previous ()
  "Find the previous imenu landmark for `tourist-mode'."
  (tourist-plugin-imenu-next 'reverse))


(provide 'tourist-plugin-imenu)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu Imenu submenus alist
;;

;;; tourist-plugin-imenu.el ends here
