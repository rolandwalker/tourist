;;; tourist-plugin-interactive-prompt.el --- Interactive prompt navigation plugin for tourist-mode
;;
;; Copyright (c) 2014 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/tourist
;; URL: http://raw.githubusercontent.com/rolandwalker/tourist/master/plugins/tourist-plugin-interactive-prompt.el
;; Version: 0.1.0
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This plugin provides `tourist-mode' support for interactive
;; modes such as `eshell-mode', `nrepl-mode', and others.
;;
;; Note that your interactive mode must be correctly configured
;; to recognize prompts.  See eg `shell-prompt-pattern' and
;; `term-prompt-regexp'.
;;
;; See Also
;;
;;     M-x customize-group RET tourist RET
;;     M-x customize-group RET eshell RET
;;     M-x customize-group RET shell RET
;;     M-x customize-group RET term RET
;;     M-x customize-group RET comint RET
;;     M-x customize-group RET ielm RET
;;     etc ...
;;
;; Notes
;;
;;     This plugin is automatically loaded by tourist.el
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

(declare-function tourist-register-method     "tourist.el")
(declare-function comint-next-prompt          "comint.el")
(declare-function comint-previous-prompt      "comint.el")
(declare-function eshell-next-prompt          "em-prompt.el")
(declare-function eshell-previous-prompt      "em-prompt.el")
(declare-function slime-repl-next-prompt      "slime-repl.el")
(declare-function slime-repl-previous-prompt  "slime-repl.el")
(declare-function term-next-prompt            "term.el")
(declare-function term-previous-prompt        "term.el")
(declare-function nrepl-previous-prompt       "nrepl.el")
(declare-function nrepl-next-prompt           "nrepl.el")

;;; register the plugin

;;;###autoload
(tourist-register-method 'interactive-prompt
  '((pretty-name                 . "Interactive Prompt")
    (priority                    . 60)
    (init-function               . tourist-plugin-interactive-prompt-init)
    (init-buffer-function        . tourist-plugin-interactive-prompt-init-buffer)
    (next-landmark-function      . tourist-plugin-interactive-prompt-next)
    (previous-landmark-function  . tourist-plugin-interactive-prompt-previous)))

;;; external interface

;;;###autoload
(defun tourist-plugin-interactive-prompt-init ()
  "Initialize the `tourist-mode' interactive prompt plugin."
  ;; don't load external libraries here -- let the user decide
  t)

;;;###autoload
(defun tourist-plugin-interactive-prompt-init-buffer ()
  "Initialize a buffer for the `tourist-mode' interactive prompt plugin."
  ;; per-buffer setup not needed
  t)

;;;###autoload
(defun tourist-plugin-interactive-prompt-next ()
  "Find the next interactive prompt landmark for `tourist-mode'."
  (let ((description nil))
    (cond
      ((derived-mode-p 'nrepl-mode)
       (setq description "Next nREPL prompt.")
       (nrepl-next-prompt))
      ((derived-mode-p 'slime-repl-mode)
       (setq description "Next SLIME prompt.")
       (slime-repl-next-prompt))
      ((derived-mode-p 'eshell-mode)
       (setq description "Next Eshell prompt.")
       (eshell-next-prompt 1))
      ((derived-mode-p 'term-mode)
       (setq description "Next term-mode prompt.")
       (term-next-prompt 1))
      ((derived-mode-p 'comint-mode)
       (setq description "Next comint-mode prompt.")
       (comint-next-prompt 1)))
    (when description
      (list
       (point)
       (point)
       description))))

;;;###autoload
(defun tourist-plugin-interactive-prompt-previous ()
  "Find the previous interactive prompt landmark for `tourist-mode'."
  (let ((description nil))
    (cond
      ;; todo could set a description inside the conditions
      ((derived-mode-p 'nrepl-mode)
       (setq description "Previous nREPL prompt.")
       (nrepl-previous-prompt))
      ((derived-mode-p 'slime-repl-mode)
       (setq description "Previous SLIME prompt.")
       (slime-repl-previous-prompt))
      ((derived-mode-p 'eshell-mode)
       (setq description "Previous Eshell prompt.")
       (eshell-previous-prompt 1))
      ((derived-mode-p 'term-mode)
       (setq description "Previous term-mode prompt.")
       (term-previous-prompt 1))
      ((derived-mode-p 'comint-mode)
       (setq description "Previous comint-mode prompt.")
       (comint-previous-prompt 1)))
    (when description
      (list
       (point)
       (point)
       description))))

(provide 'tourist-plugin-interactive-prompt)

;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: imenu eshell comint ielm repl nrepl Eshell nREPL
;;

;;; tourist-plugin-interactive-prompt.el ends here
