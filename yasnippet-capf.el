;;; yasnippet-capf.el --- Yasnippet Completion At Point Function -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: August 11, 2022
;; Modified: August 11, 2022
;; Version: 0.0.3
;; Homepage: https://github.com/elken/yasnippet-capf
;; Package-Requires: ((emacs "25.1") (yasnippet "0.14.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Yasnippet Completion at Point Extension to lookup snippets by name
;;
;; Simply add to the list of existing `completion-at-point-functions' thus:
;;    (add-to-list 'completion-at-point-functions #'yasnippet-capf)
;;
;; If you prefer to have the lookup done by name rather than key, set
;; `yasnippet-capf-lookup-by'.
;;
;;; Code:

(require 'thingatpt)
(require 'yasnippet)
(require 'cl-lib)
(require 'subr-x)

(defgroup yasnippet-capf nil
  "Yasnippet CAPF."
  :group 'completion)

(defcustom yasnippet-capf-lookup-by 'key
  "The method in which to lookup candidates by."
  :type '(choice
          (const :tag "Key" key)
          (const :tag "Name" name)))

(defvar yasnippet-capf--properties
  (list :annotation-function (lambda (snippet) (get-text-property 0 'yas-annotation snippet))
        :company-kind (lambda (_) 'snippet)
        :company-doc-buffer #'yasnippet-capf--doc-buffer
        :exit-function (lambda (cand status)
                         (when (string= "finished" status)
                           (when-let ((snippet (yasnippet-capf--lookup-snippet cand)))
                             (delete-char (* -1 (length cand)))
                             (yas-expand-snippet snippet))))
        :exclusive 'no)
  "Completion extra properties for `yasnippet-capf'.")

(defun yasnippet-capf--doc-buffer (cand)
  "Calculate the expansion of the snippet for CAND.
Returns a buffer to be displayed by popupinfo."
  (when-let ((mode major-mode)
             (template (get-text-property 0 'yas-template cand)))
    (with-current-buffer (get-buffer-create "*yasnippet-capf-doc*")
      (erase-buffer)
      (yas-minor-mode)
      (insert "Expands to:" ?\n ?\n)
      (condition-case error
          (yas-expand-snippet (yas--template-content template))
        (error
         (message "Error expanding: %s" (error-message-string error))))
      (delay-mode-hooks
        (let ((inhibit-message t))
          (when (eq mode 'web-mode)
            (setq mode 'html-mode))
          (funcall mode)))
      (ignore-errors (font-lock-ensure))
      (current-buffer))))

(defun yasnippet-capf--lookup-snippet (name)
  "Get the snippet called NAME in MODE's tables."
  (let ((yas-choose-tables-first nil)
        (yas-choose-keys-first nil))
    (cl-find name (yas--all-templates
                   (yas--get-snippet-tables major-mode))
             :key (pcase yasnippet-capf-lookup-by
                    ('key #'yas--template-key)
                    ('name #'yas--template-name))
             :test #'string=)))

(defun yasnippet-capf--key-prefixes ()
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

(defun yasnippet-capf--completions-for-prefix (prefix key-prefix tables)
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
                    (propertize (pcase yasnippet-capf-lookup-by
                                  ('key key)
                                  ('name name))
                                'yas-annotation name
                                'yas-template template
                                'yas-prefix-offset (- (length (pcase yasnippet-capf-lookup-by
                                                                ('key key)
                                                                ('name name)))
                                                      (length prefix)))
                    res)))
               value)))
          keyhash))
       res))
   tables))

(defun yasnippet-capf-candidates (&optional prefix)
  "Return a list of candidate snippets filtered by PREFIX."
  (pcase yasnippet-capf-lookup-by
    ('key
     (cl-loop with tables = (yas--get-snippet-tables)
              for key-prefix in (yasnippet-capf--key-prefixes)
              when (>= (length key-prefix) (length prefix))
              thereis (yasnippet-capf--completions-for-prefix prefix
                                                              key-prefix
                                                              tables)))
    ('name
     (thread-last (yas--get-snippet-tables)
                  (yas--all-templates)
                  (mapcar #'yas--template-name)
                  (cl-remove-if-not (lambda (cand)
                                      (and prefix
                                           (string-prefix-p prefix cand))))))
    (_ (error "Invalid value for yasnippet-capf-lookup-by: %s" yasnippet-capf-lookup-by))))

(defun yasnippet-capf--list (input)
  "Use INPUT to compute and filter a new completion table."
  (cons (apply-partially #'string-prefix-p input)
        (yasnippet-capf-candidates input)))

(defun yasnippet-capf--completion-table ()
  "Return a suitable function to create a completion table."
  (lambda (str pred action)
    (let ((snippets (yasnippet-capf--list str)))
      (if (eq action 'metadata)
          '(metadata (category . yasnippet-capf))
        (complete-with-action action snippets str pred)))))

;;;###autoload
(defun yasnippet-capf (&optional interactive)
  "Complete with yasnippet at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions #'yasnippet-capf))
        (or (completion-at-point) (user-error "yasnippet-capf: No completions")))
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      `(,(match-beginning 0) ,(match-end 0)
        ,(yasnippet-capf--completion-table)
        ,@yasnippet-capf--properties))))

(provide 'yasnippet-capf)
;;; yasnippet-capf.el ends here
