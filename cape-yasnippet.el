;;; cape-yasnippet.el --- Yasnippet Completion at Point Extension -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: August 11, 2022
;; Modified: August 11, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elken/corfu-yasnippet
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Yasnippet backend for Corfu
;;
;;; Code:

(require 'cape)
(require 'cl-lib)

(declare-function yas--table-hash "yasnippet")
(declare-function yas--get-snippet-tables "yasnippet")
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas--template-content "yasnippet")
(declare-function yas--template-expand-env "yasnippet")
(declare-function yas--warning "yasnippet")
(declare-function yas-minor-mode "yasnippet")
(declare-function yas--require-template-specific-condition-p "yasnippet")
(declare-function yas--template-can-expand-p "yasnippet")
(declare-function yas--template-condition "yasnippet")

(defun cape-yasnippet--key-prefixes ()
  "Mostly copied from `yas--templates-for-key-at-point'."
  (defvar yas-key-syntaxes)
  (save-excursion
    (let ((original (point))
          (methods yas-key-syntaxes)
          prefixes
          method)
      (while methods
        (unless (eq method (car methods))
          (goto-char original))
        (setq method (car methods))
        (cond ((stringp method)
               (skip-syntax-backward method)
               (setq methods (cdr methods)))
              ((functionp method)
               (unless (eq (funcall method original)
                           'again)
                 (setq methods (cdr methods))))
              (t
               (setq methods (cdr methods))
               (yas--warning "Invalid element `%s' in `yas-key-syntaxes'" method)))
        (let ((prefix (buffer-substring-no-properties (point) original)))
          (unless (equal prefix (car prefixes))
            (push prefix prefixes))))
      prefixes)))

(defun cape-yasnippet--completions-for-prefix (prefix key-prefix tables)
  "Get a completion candidate for PREFIX with KEY-PREFIX in TABLES."
  (cl-mapcan
   (lambda (table)
     (let ((keyhash (yas--table-hash table))
           (requirement (yas--require-template-specific-condition-p))
           res)
       (when keyhash
         (maphash
          (lambda (key value)
            (when (and (stringp key)
                       (string-prefix-p key-prefix key))
              (maphash
               (lambda (name template)
                 (when (yas--template-can-expand-p
                        (yas--template-condition template) requirement)
                   (push
                    (propertize key
                                'yas-annotation name
                                'yas-template template
                                'yas-prefix-offset (- (length key-prefix)
                                                      (length prefix)))
                    res)))
               value)))
          keyhash))
       res))
   tables))

(defvar cape-yasnippet--properties
  (list :annotation-function (lambda (_) " Yasnippet")
        :company-kind (lambda (_) 'text)
        :exclusive 'no)
  "Completion extra properties for `cape-yasnippet'.")

(defun cape-yasnippet--list (prefix)
  "Find all snippets for major-mode matching PREFIX."
(cl-loop with tables = (yas--get-snippet-tables)
           for key-prefix in (cape-yasnippet--key-prefixes)
           when (>= (length key-prefix) (length prefix))
           thereis (cape-yasnippet--completions-for-prefix prefix
                                                           key-prefix
                                                           tables)))

;;;###autoload
(defun cape-yasnippet (&optional interactive)
  "Complete with yasnippet at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-yasnippet)
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        `(,beg ,end
               ,(cape--table-with-properties
                 (cape--cached-table beg end
                                     #'cape-yasnippet--list
                                     'prefix)
                 :category 'cape-yasnippet)
               ,@cape-yasnippet--properties)))))

(provide 'cape-yasnippet)
;;; cape-yasnippet.el ends here
