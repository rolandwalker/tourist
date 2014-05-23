;;; tourist-plugin-comment-section.el --- Comment-section plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-comment-section.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides support for comment-section markers in
;; `tourist-mode'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
;;
;;     See tourist-plugin-imenu.el for documentation of the plugin interface.
;;
;; Bugs
;;
;;     There are brief moments when syntax-ppss gives wrong answers.
;;     How to force recalculation?
;;
;; TODO
;;
;;     This code works but it was cobbled together from multiple
;;     sources in a hacktastic way.  It is tortured and needs
;;     abstraction and cleanup.
;;
;;     Not well tested on other than Emacs Lisp.
;;
;;     Length of comment delimiters specified two different ways.
;;
;;     Mode-specific values for different languages.
;;
;;     Handle box comments.
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
(tourist-register-method 'comment-section
  '((pretty-name                 . "Section Divider in Comments")
    (priority                    . 60)
    (init-function               . tourist-plugin-comment-section-init)
    (init-buffer-function        . tourist-plugin-comment-section-init-buffer)
    (next-landmark-function      . tourist-plugin-comment-section-next)
    (previous-landmark-function  . tourist-plugin-comment-section-previous)))

;;; customizable variables

(defcustom tourist-plugin-comment-section-delimiter-count 3
  "How many leading comment delimiters identify a section divider.

The default 3, which is appropriate for Emacs Lisp."
  :type 'integer
  :group 'tourist-plugins)

;;; internal utility functions

(defun tourist-plugin-comment-section--comment-line-p (&optional pos min-chars)
  "Tell whether POS is part of a line that is only comment.

POS is optional and defaults to current point.

Leading or trailing whitespace on the line containing POS
is ignored.

Optional MIN-CHARS may be used to set a minimum number of
comment delimiter characters used to introduce the comment.

The return value is nil, or the position of the comment start
as determined by `syntax-ppss'.  Note that the start position
may be from a prior line, or may be greater than POS."
  (save-excursion
    (save-match-data
      (callf or pos (point))
      (let* ((start (save-excursion (goto-char pos) (+ (line-beginning-position) (current-indentation))))
             (size (- (save-excursion (goto-char start) (skip-syntax-forward "<") (point)) start))
             (end (save-excursion (goto-char pos) (line-end-position)))
             (syn (syntax-ppss end)))
        (when (or (not min-chars)
                  (>= size min-chars))
          (unless (nth 4 syn)
            (setq end (max (point-min)
                           (save-excursion
                             (goto-char end)
                             (skip-syntax-backward "-")
                             (1- (point)))))
            (setq syn (syntax-ppss end)))
          (when (and (<= start end)
                     (nth 4 syn)
                     (<= (nth 8 syn) start))
            (nth 8 syn)))))))

(defun tourist-plugin-comment-section--comment-block-end (&optional pos min-chars backward)
  "Return the end position of the multi-line comment containing POS.

\"Multi-line comment\" here includes contiguous single-line comments.

POS is optional and defaults to current point.

Optional MIN-CHARS may be used to set a minimum number of
comment delimiter characters used to introduce each comment
line.

If optional BACKWARD is set, finds the boundary in the reverse
direction, ie the start of the comment block.

Depending on the comment syntax, the returned position may or may
not contain whitespace."
  (callf or pos (point))
  (when (tourist-plugin-comment-section--comment-line-p pos min-chars)
    (save-excursion
      (save-match-data
        (goto-char pos)
        (forward-line (if backward -1 1))
        (while (and (not (if backward (bobp) (eobp)))
                    (tourist-plugin-comment-section--comment-line-p nil min-chars))
          (forward-line (if backward -1 1)))
        (if backward
            (progn
              (forward-line 1)
              (when (> (point) pos)
                (goto-char pos))
              (tourist-plugin-comment-section--comment-line-p nil min-chars))
          ;; else
          (while (and (not (nth 4 (syntax-ppss (point))))
                      (>= (point) pos))
            (forward-char -1))
          (when (nth 4 (syntax-ppss (point)))
            (point)))))))

;;;###autoload
(defun tourist-plugin-comment-section-init ()
  "Initialize the `tourist-mode' comment-section plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-comment-section-init-buffer ()
  "Initialize a buffer for the `tourist-mode' comment-section plugin."
  ;; needs no setup, requiring only built-in functions
  t)

;;;###autoload
(defun tourist-plugin-comment-section-next ()
  "Find the next comment-section landmark for `tourist-mode'."
  (when comment-start
    (let* ((valid-start-pat (concat "[^ \t\r\f\n" (substring comment-start 0 (min 1 (length comment-start))) "]"))
           (section-pat (concat
                         "^\\s-*"
                         (apply 'concat (make-list tourist-plugin-comment-section-delimiter-count "\\s<"))
                         "\\s<*\\s-*\\(" valid-start-pat "[^\n]+\\)"))
           (pos nil))
      (narrow-to-region 1 (point-max))
      (save-excursion
        (goto-char (or (tourist-plugin-comment-section--comment-block-end nil tourist-plugin-comment-section-delimiter-count t) (point)))
        (setq pos (catch 'pos
                    (while (re-search-forward section-pat nil t)
                      (when (not (and (eq major-mode 'emacs-lisp-mode)
                                      (string-match-p "\\`\\(?:###autoload\\s-*\\)\\'" (match-string-no-properties 1))))
                        (throw 'pos (match-beginning 1)))
                      (goto-char (or (tourist-plugin-comment-section--comment-block-end nil tourist-plugin-comment-section-delimiter-count) (point))))
                    nil)))
      (unless (and pos
                   (> pos (point)))
        (goto-char (or (tourist-plugin-comment-section--comment-block-end nil tourist-plugin-comment-section-delimiter-count) (point)))
        (setq pos (catch 'pos
                    (while (re-search-forward section-pat nil t)
                      (when (not (and (eq major-mode 'emacs-lisp-mode)
                                      (string-match-p "\\`\\(?:###autoload\\s-*\\)\\'" (match-string-no-properties 1))))
                        (throw 'pos (match-beginning 1)))
                      (goto-char (or (tourist-plugin-comment-section--comment-block-end nil tourist-plugin-comment-section-delimiter-count) (point))))
                    nil)))
      (when pos
        (list
         pos
         ;; todo end coordinate
         )))))

;;;###autoload
(defun tourist-plugin-comment-section-previous ()
  "Find the previous comment-section landmark for `tourist-mode'."
  (when comment-start
    (let* ((valid-start-pat (concat "[^ \t\r\f\n" (substring comment-start 0 (min 1 (length comment-start))) "]"))
           (section-pat (concat
                         "^\\s-*"
                         (apply 'concat (make-list tourist-plugin-comment-section-delimiter-count "\\s<"))
                         "\\s<*\\s-*\\(" valid-start-pat "[^\n]+\\)"))
           (pos nil))
      (narrow-to-region 1 (point-max))
      (save-excursion
        (goto-char (or (tourist-plugin-comment-section--comment-block-end nil tourist-plugin-comment-section-delimiter-count 'backward) (point)))
        (setq pos (car (tourist-plugin-comment-section-next))))
      (unless (and pos
                   (< pos (point)))
        (goto-char (or (tourist-plugin-comment-section--comment-block-end nil tourist-plugin-comment-section-delimiter-count 'backward) (point)))
        (setq pos (catch 'pos
                    (while (re-search-backward section-pat nil t)
                      (let ((bound (point))
                            (top (tourist-plugin-comment-section--comment-block-end nil tourist-plugin-comment-section-delimiter-count 'backward)))
                        (when (and top
                                   (< top (point)))
                          (goto-char top)
                          (re-search-forward section-pat bound t))
                        (when (not (and (eq major-mode 'emacs-lisp-mode)
                                        (string-match-p "\\`\\(?:###autoload\\s-*\\)\\'" (match-string-no-properties 1))))
                          (throw 'pos (match-beginning 1))))
                      (goto-char (or (tourist-plugin-comment-section--comment-block-end nil tourist-plugin-comment-section-delimiter-count 'backward) (point))))
                    nil)))
      (when pos
        (list
         pos
         ;; todo end coordinate
         )))))

(provide 'tourist-plugin-comment-section)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu callf ppss multi Multi
;;

;;; tourist-plugin-comment-section.el ends here
