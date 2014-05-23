;;; tourist-plugin-bookmark.el --- Bookmark plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-bookmark.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for bookmarks in `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET bookmark RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;;     file-equal-p is slow, try
;;
;; TODO
;;
;;     Handle non-file bookmarks.
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

(declare-function tourist-register-method "tourist.el")

(declare-function bookmark-get-filename "bookmark")
(declare-function bookmark-get-position "bookmark")

;;; register the plugin

;;;###autoload
(tourist-register-method 'bookmark
  '((pretty-name                 . "Bookmark")
    (priority                    . 20)
    (init-function               . tourist-plugin-bookmark-init)
    (init-buffer-function        . tourist-plugin-bookmark-init-buffer)
    (next-landmark-function      . tourist-plugin-bookmark-next)
    (previous-landmark-function  . tourist-plugin-bookmark-previous)))

;;; internal utility functions

(defun tourist-plugin-bookmark--feedback-msg (pos local-mark-alist)
  "Generate feedback message for bookmark at POS, given LOCAL-MARK-ALIST."
  (let* ((bmk (cdr (assoc pos local-mark-alist)))
         (name (bookmark-name-from-full-record bmk))
         (annotation (bookmark-get-annotation bmk))
         (msg (format "Bookmark - %s" (or name ""))))
    (when annotation
      (callf concat msg (format " / %s" annotation)))
    msg))

;;; external interface

;;;###autoload
(defun tourist-plugin-bookmark-init ()
  "Initialize the `tourist-mode' bookmark.el plugin."
  ;; don't load bookmark.el here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-bookmark-init-buffer ()
  "Initialize a buffer for the `tourist-mode' bookmark.el plugin."
  ;; per-buffer setup not needed
  t)

;;;###autoload
(defun tourist-plugin-bookmark-next (&optional reverse)
  "Find the next bookmark.el landmark for `tourist-mode'."
  (when (and (boundp 'bookmark-alist)
             bookmark-alist
             (fboundp 'bookmark-get-filename))
    (let* ((minmax (if reverse 'max 'min))
           (direction (if reverse '< '>))
           (this-file (convert-standard-filename (expand-file-name (buffer-file-name (current-buffer)))))
           (local-mark-alist (delq nil (mapcar #'(lambda (bmk)
                                                   ;; avoid slow file-equal-p
                                                   (when (equal this-file
                                                                (convert-standard-filename (expand-file-name (bookmark-get-filename bmk))))
                                                     ;; bookmark-handle-bookmark must be used for combo of position+search
                                                     (let ((pos (save-excursion
                                                                  (save-restriction
                                                                    (widen)
                                                                    (bookmark-handle-bookmark bmk)
                                                                    (point)))))
                                                       ;; bookmarks may be outside the narrowed search region
                                                       (when (and (>= pos (point-min))
                                                                  (<= pos (point-max))
                                                                  (funcall direction pos (point)))
                                                         (cons pos bmk))))) bookmark-alist))))
      (when local-mark-alist
        (let ((pos (apply minmax (mapcar 'car local-mark-alist))))
          (list
           pos
           pos
           (tourist-plugin-bookmark--feedback-msg pos local-mark-alist)))))))

;;;###autoload
(defun tourist-plugin-bookmark-previous ()
  "Find the previous bookmark.el landmark for `tourist-mode'."
  (tourist-plugin-bookmark-next 'reverse))

(provide 'tourist-plugin-bookmark)

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

;;; tourist-plugin-bookmark.el ends here
