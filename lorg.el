;;; lorg.el --- Org-Mode based link manager -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Eyoel Tesfu
;;
;; Author: Eyoel Tesfu <EyoelYTesfu@gmail.com>
;; Maintainer: Eyoel Tesfu <EyoelYTesfu@gmail.com>
;; Created: December 17, 2025
;; Modified: December 17, 2025
;; Version: 0.0.1
;; Keywords: convenience hypermedia  matching outlines text tools
;; Homepage: https://github.com/eyoeltesfu/lorg
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar lorg-files '())
(defvar lorg-extensions '("org"))
(defvar lorg--links-cache-alist nil)

(defun lorg--scan-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (while (re-search-forward "\\[\\[\\(https?://.*\\)\\]\\[\\(.*\\)\\]\\]" nil t)
      (let ((url (match-string-no-properties 1))
            (description (match-string-no-properties 2)))
        (push (cons description url) lorg--links-cache-alist)))))

(defun lorg--get-ext-globs (ext)
  (let* ((globs))
    (dolist (el ext globs)
      (push (format "\"*.%s\"" el) globs)
      (push (format "\"*.%s.gpg\"" el) globs)
      (push (format "\"*.%s.age\"" el) globs))))

(defun lorg--scan-directory (dir)
  (when-let ((files (lorg--rg--fetch-files dir)))
    (lorg--rescan-files files)))

(defun lorg--shell-command-to-list (cmd)
  "Return shell command output in a list"
  (seq-filter (lambda (s) (not (string-blank-p s)))
              (split-string (ansi-color-filter-apply (shell-command-to-string cmd))
                            "\n")))

(defun lorg--find--fetch-files (dir)
  "Return all lorg files under DIR using executable \"find\""
  (let ((exe (executable-find "find")))
    (when (and exe lorg-extensions (file-directory-p dir))
      (let* ((globs (lorg--get-ext-globs lorg-extensions))
             (exts (string-join (mapcar (lambda (glob) (concat "-name " glob)) globs)
                                " -o "))
             (command (string-join `(,exe "-L" ,(shell-quote-argument dir) "-type f \\(" ,exts "\\)")
                                   " ")))
        (lorg--shell-command-to-list command)))))

(defun lorg--fd--fetch-files (dir)
  "Return all lorg files under DIR using executable \"fd\""
  (let ((exe (executable-find "fd")))
    (when (and exe lorg-extensions (file-directory-p dir))
      (let* ((globs (lorg--get-ext-globs lorg-extensions))
             (exts (string-join (mapcar (lambda (glob) (concat "-e " (substring glob 2 -1))) globs)
                                " "))
             (command (string-join `(,exe "-L" "--type file" ,exts "." ,dir)
                                   " ")))
        (lorg--shell-command-to-list command)))))

(defun lorg--rg--fetch-files (dir)
  (let ((exe (executable-find "rg")))
    (when (and exe lorg-extensions (file-directory-p dir))
      (let* ((globs (lorg--get-ext-globs lorg-extensions))
             (command (string-join `(,exe "-L" ,dir "--files" ,@(mapcar (lambda (glob) (concat "-g " glob)) globs))
                                   " ")))
        (lorg--shell-command-to-list command)))))

(defun lorg--rescan-files (files)
  "Reset `lorg--links-cache-alist' then populate it again"
  (setq lorg--links-cache-alist nil)
  (let* ((inhibit-read-only t))
    (while (setq file (pop files))
      (cond ((file-regular-p file)
             (lorg--scan-file file))
            ((file-directory-p file)
             (lorg--scan-directory file))))))

(defun lorg--annotation-function (str pred flag)
  (if (eq flag 'metadata)
      '(metadata (annotation-function
                  lambda (str)
                  (concat " " (propertize " " 'display '(space :align-to 40))
                          (propertize
                           (cdr (assoc str lorg--links-cache-alist))
                           'face (or 'marginalia-documentation 'font-lock-comment-face)))))
    (all-completions str (mapcar 'car lorg--links-cache-alist) pred)))

(defun lorg-menu-ask (prompt alist handler &optional arg)
  (when arg (lorg--rescan-files lorg-files))
  (let* ((choice (completing-read prompt #'lorg--annotation-function))
         (url (alist-get choice lorg--links-cache-alist nil nil 'equal)))
    (funcall handler url)))

(defun lorg-menu (&optional arg)
  (interactive "P")
  (lorg-menu-ask "Link: " lorg--links-cache-alist #'org-link-open-from-string (and arg)))

(provide 'lorg)
;;; lorg.el ends here
