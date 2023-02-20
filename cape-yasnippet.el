;;; cape-yasnippet.el --- Yasnippet Completion at Point Extension -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: August 11, 2022
;; Modified: August 11, 2022
;; Version: 0.0.2
;; Homepage: https://github.com/elken/cape-yasnippet
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Yasnippet Completion at Point Extenstion to lookup snippets by name
;;
;; Simply add to the list of existing `completion-at-point-functions' thus:
;;    (add-to-list 'completion-at-point-functions #'cape-yasnippet)
;;
;;; Code:

(require 'cape)
(require 'cl-lib)
(require 'subr-x)

(declare-function yas-lookup-snippet "yasnippet")
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas--template-name "yasnippet")
(declare-function yas--all-templates "yasnippet")
(declare-function yas--get-snippet-tables "yasnippet")

(defun cape-yasnippet-candidates (&optional prefix)
  "Return a list of candidate snippets."
  (thread-last (yas--get-snippet-tables)
                (yas--all-templates)
                (mapcar #'yas--template-name)
                (cl-remove-if-not (lambda (cand)
                                    (and prefix
                                         (string-prefix-p prefix cand))))))

(defvar cape-yasnippet--properties
  (list :annotation-function (lambda (_) " Yasnippet")
        :company-kind (lambda (_) 'text)
        :exit-function (lambda (cand status)
                         (when (string= "finished" status)
                           (delete-char (* -1 (length cand)))
                           (yas-expand-snippet (yas-lookup-snippet cand))))
        :exclusive 'no)
  "Completion extra properties for `cape-yasnippet'.")

;;;###autoload
(defun cape-yasnippet (&optional interactive)
  "Complete with yasnippet at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-yasnippet)
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        `(,beg ,end
          ,(cape--table-with-properties
            (cape--cached-table beg end
                                #'cape-yasnippet-candidates
                                'prefix)
            :category 'cape-yasnippet)
          ,@cape-yasnippet--properties)))))

(provide 'cape-yasnippet)
;;; cape-yasnippet.el ends here
