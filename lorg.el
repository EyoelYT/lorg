;;; lorg.el --- Org-Mode based link manager -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Eyoel Tesfu
;;
;; Author: Eyoel Tesfu <EyoelYTesfu@gmail.com>
;; Maintainer: Eyoel Tesfu <EyoelYTesfu@gmail.com>
;; Created: December 17, 2025
;; Version: 0.0.1
;; Keywords: convenience hypermedia  matching outlines text tools
;; Homepage: https://github.com/eyoeltesfu/lorg
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Lorg provides a lightweight, Org-mode–based link manager.
;; It scans one or more directories or files for org-style links,
;; caches them up to a configurable maximum, and provides an
;; interactive menu to quickly open any link by its description.
;;
;;  Usage
;;
;;  1. Configure `lorg-files' to list the files and/or directories to scan.
;;  2. Call `lorg-menu' (or bind it to a key) to invoke the link selector.
;;     - With a prefix argument, the cache is refreshed before prompting.
;;  3. Select a link description; the target URL or file path will open.
;;
;;; Code:

(require 'org)
(require 'seq)
(require 'ansi-color)

(defvar lorg-files nil
  "List of files or directories to scan for Org links.
Each element is either a path to a file or a directory.
Directories are searched recursively for files matching
`lorg-extensions'.")

(defvar lorg-max-links 1000
  "Maximum number of links to cache.
Once this limit is reached during a scan, further links are ignored to
prevent excessive memory usage.")

(defvar lorg-extensions '("org")
  "List of file extensions (without dot) to scan.
Extensions may include encrypted variants; files ending
in \".<ext>.gpg\" or \".<ext>.age\" are also matched automatically.")

(defvar lorg-link-re org-link-any-re
  "This variable holds the regexp definition of what to capture from target files.")

(defvar lorg--links-cache-alist nil
  "Internal cache of scanned links.
An alist of (DESCRIPTION . URI) pairs collected from scanned files.")

(defun lorg--scan-file (file)
  "Scan FILE for Org links and populate `lorg--links-cache-alist'.
Stop scanning when `lorg-max-links' entries have been added. Each link's
description and URI (type & path) are stored in the cache."
  (with-temp-buffer
    (let ((org-inhibit-startup t)
          (org-element-cache-persistent)
          (org-element-use-cache)
          (org-mode-hook)
          (gc-cons-threshold 100000000) ; 100mb
          (coding-system-for-read 'utf-8))
      (org-mode)
      (buffer-disable-undo)
      (insert-file-contents file)
      (catch 'max-limit-reached
        (while (re-search-forward lorg-link-re nil t)
          (let* ((url (match-string-no-properties 2))
                 (description (or (match-string-no-properties 3) url))
                 (context (org-element-lineage (org-element-context) 'link t))
                 (type (org-element-property :type context))
                 (path (if (equal type "file")
                           (expand-file-name (org-element-property :path context))
                         (org-element-property :path context))))
            (if (and description type path)
                (let ((uri (concat type ":" path)))
                  (push (cons description uri) lorg--links-cache-alist)
                  (when (>= (length lorg--links-cache-alist) lorg-max-links)
                    (throw 'max-limit-reached t))))))))))

(defun lorg--get-ext-globs (exts)
  "Return shell glob patterns for each extension in EXTS.
For each EXT in EXTS, produce \"*.EXT\", \"*.EXT.gpg\", and
\"*.EXT.age\"."
  (let ((globs))
    (dolist (ext exts globs)
      (push (format "\"*.%s\"" ext) globs)
      (push (format "\"*.%s.gpg\"" ext) globs)
      (push (format "\"*.%s.age\"" ext) globs))))

(defun lorg--get-ext-list (exts)
  "Return list of extensions including encrypted variants.
For each EXT in EXTS, produce EXT, \"EXT.gpg\", and \"EXT.age\"."
  (append exts
          (mapcan (lambda (ext)
                    (list (concat ext ".gpg")
                          (concat ext ".age")))
                  exts)))

(defun lorg--build-ext-regex (exts)
  "Build a regex matching any extension in EXTS followed by end of string."
  (rx-to-string `(or ,@exts) t))

(defun lorg--scan-directory (dir)
  "Scan directory DIR recursively for files matching `lorg-extensions'.
Uses one of three external functions to list files, then rescans each
file's contents for links."
  (when-let* ((files (or (lorg--fd--fetch-files dir)
                         (lorg--rg--fetch-files dir)
                         (lorg--find--fetch-files dir)
                         (lorg--native--fetch-files dir))))
    (lorg--rescan-files files)))

(defun lorg--shell-command-to-list (cmd)
  "Execute shell command CMD and return non-blank output lines as a list of
strings."
  (seq-filter (lambda (s) (not (string-blank-p s)))
              (split-string
               (ansi-color-filter-apply (shell-command-to-string cmd)) "\n")))

(defun lorg--find--fetch-files (dir)
  "Use \"find\" to list all files under DIR matching `lorg-extensions'."
  (let ((exe (executable-find "find")))
    (if (and exe lorg-extensions (file-directory-p dir))
        (let* ((globs (lorg--get-ext-globs lorg-extensions))
               (exts (string-join (mapcar (lambda (glob)
                                            (concat "-name " glob))
                                          globs)
                                  " -o "))
               (command (string-join `(,exe
                                       "-L"
                                       ,(shell-quote-argument dir)
                                       "-type f \\("
                                       ,exts
                                       "\\)")
                                     " ")))
          (lorg--shell-command-to-list command))
      nil)))

(defun lorg--fd--fetch-files (dir)
  "Use \"fd\" to list all files under DIR matching `lorg-extensions'."
  (let ((exe (executable-find "fd")))
    (if (and exe lorg-extensions (file-directory-p dir))
        (let* ((globs (lorg--get-ext-globs lorg-extensions))
               (exts (string-join (mapcar (lambda (glob)
                                            (concat "-e " (substring glob 2 -1)))
                                          globs)
                                  " "))
               (command (string-join `(,exe
                                       "-L"
                                       "--type file"
                                       ,exts
                                       "."
                                       ,(shell-quote-argument dir))
                                     " ")))
          (lorg--shell-command-to-list command))
      nil)))

(defun lorg--rg--fetch-files (dir)
  "Use \"ripgrep\" to list all files under DIR matching `lorg-extensions'."
  (let ((exe (executable-find "rg")))
    (if (and exe lorg-extensions (file-directory-p dir))
        (let* ((globs (lorg--get-ext-globs lorg-extensions))
               (command (string-join `(,exe
                                       "-L"
                                       ,(shell-quote-argument dir)
                                       "--files"
                                       ,@(mapcar (lambda (glob)
                                                   (concat "-g " glob))
                                                 globs))
                                     " ")))
          (lorg--shell-command-to-list command))
      nil)))

(defun lorg--native--fetch-files (dir)
  "Recursively list all files under DIR matching `lorg-extensions'.
This is a pure Elisp implementation that doesn't require external tools."
  (when (and lorg-extensions (file-directory-p dir))
    (let* ((exts (lorg--get-ext-list lorg-extensions))
           (exts-regexp (lorg--build-ext-regex exts))
           (cands (directory-files-recursively dir exts-regexp t))
           (matches))
      (dolist (file cands matches)
        (unless (file-directory-p file)
          (push file matches)))
      matches)))

(defun lorg--rescan-files (files)
  "Clear `lorg--links-cache-alist' then scan each element of FILES.
Each element may be a regular file or a directory."
  (let* ((file)
         (inhibit-read-only t))
    (while (setq file (pop files))
      (cond ((file-regular-p file)
             (lorg--scan-file file))
            ((file-directory-p file)
             (lorg--scan-directory file))))))

(defun lorg--annotation-function (str pred flag)
  "Annotation function for `completing-read'.
When FLAG is 'metadata, return an annotation specification that shows
the target URI. Otherwise, filter completions by PRED."
  (if (eq flag 'metadata)
      `(metadata
        (annotation-function
         . ,(lambda (str)
              (let ((entry (assoc str lorg--links-cache-alist)))
                (when entry
                  (concat " "
                          (propertize " "
                                      'display '(space :align-to 40))
                          (propertize (cdr entry)
                                      'face (or 'marginalia-documentation
                                                'font-lock-comment-face))))))))
    (all-completions str (mapcar 'car lorg--links-cache-alist) pred)))

(defun lorg-menu-ask (prompt handler &optional force-rescan)
  "Prompt with PROMPT over ALIST of (DESCRIPTION . URI) pairs.
After selection, call HANDLER with the associated URI. If FORCE-RESCAN
is non-nil or `lorg--links-cache-alist' is nil, refresh the link cache
from `lorg-files' first."
  (when (or (null lorg--links-cache-alist) force-rescan)
    (setq lorg--links-cache-alist nil)
    (lorg--rescan-files lorg-files))
  (let* ((choice (completing-read prompt #'lorg--annotation-function))
         (url (alist-get choice lorg--links-cache-alist nil nil 'equal)))
    (funcall handler url)))

;;;###autoload
(defun lorg-menu (&optional arg)
  "Interactively select and open an Org link from `lorg-files'.
With prefix ARG, force a rescan before prompting."
  (interactive "P")
  (lorg-menu-ask "Link: " #'org-link-open-from-string (and arg)))

;;;###autoload
(defun lorg-menu-rescan ()
  "Force a rescan of all `lorg-files' and then prompt to open a link."
  (interactive)
  (lorg-menu-ask "Link: " #'org-link-open-from-string t))

(provide 'lorg)
;;; lorg.el ends here
