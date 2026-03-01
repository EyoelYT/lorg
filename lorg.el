;;; lorg.el --- Org-Mode based link manager -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Eyoel Tesfu
;;
;; Author: Eyoel Tesfu <EyoelYTesfu@gmail.com>
;; Maintainer: Eyoel Tesfu <EyoelYTesfu@gmail.com>
;; Created: December 17, 2025
;; Version: 0.0.1
;; Keywords: convenience hypermedia  matching outlines text tools
;; Homepage: https://github.com/eyoelyt/lorg
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
;;  3. Select a link description; the target URI or file path will open.
;;
;;; Code:

(require 'org)
(require 'seq)
(require 'ansi-color)

(defgroup lorg nil
  "Org-Mode based link manager."
  :group 'org
  :prefix "lorg-"
  :link '(url-link :tag "Github" "https://github.com/eyoelyt/lorg"))

(defcustom lorg-files nil
  "List of files or directories to scan for Org links.
Each element is either a path to a file or a directory.
Directories are searched recursively for files matching
`lorg-extensions'."
  :type '(choice
          (repeat :tag "List of files and directories" file)
          (file :tag "Store list in a file\n" :value "~/.agenda_files"))
  :group 'lorg)

(defcustom lorg-max-links 1000
  "Maximum number of links to cache.
Once this limit is reached during a scan, further links are ignored to
prevent excessive memory usage."
  :type 'natnum
  :group 'lorg)

(defcustom lorg-extensions '("org")
  "List of file extensions (without dot) to scan.
Extensions may include encrypted variants; files ending
in \".<ext>.gpg\" or \".<ext>.age\" are also matched automatically."
  :type '(repeat string)
  :group 'lorg)

(defcustom lorg-link-re org-link-any-re
  "Regexp used to match Org links during file scanning.
Defaults to `org-link-any-re'. Override to restrict which link types are
captured."
  :type 'regexp
  :group 'lorg)

(defcustom lorg-group-by '(file)
  "Groups completion candidates in completing-read.
Value is a list of symbols. Supported symbols:
  file   - include source filename (e.g., \"work.org\")
  path   - include full heading path (e.g., \"Parent/Child/Grandchild\")
  parent - include immediate parent heading only (overrides path if both present)
  nil    - no grouping (flat list)
Examples:
  nil              - flat list, no groups
  '(file)          - group by filename only
  '(file path)     - group by filename + full heading path
  '(file parent)   - group by filename + immediate parent
  '(path)          - group by full heading path only
  '(parent)        - group by immediate parent only
  '(path parent)   - parent takes precedence (same as '(parent))
  '(parent path)   - parent takes precedence (same as '(parent))

Changes to this variable take effect immediately without rescanning."
  :type '(set (const :tag "Source filename" file)
          (const :tag "Full heading path" path)
          (const :tag "Immediate parent heading" parent))
  :group 'lorg)

(defcustom lorg-group-breadcrumbs-splitter "/"
  "String needed to join breadcrumb segments.
Rescan is needed for effect."
  :type 'string
  :group 'lorg)

(defcustom lorg-group-file-splitter "::"
  "String needed to join file with breadcrumb.
Rescan is not needed for effect."
  :type 'string
  :group 'lorg)

(defvar lorg--links-cache-alist nil
  "Internal cache of scanned links.
An alist of (DESCRIPTION . ((URI . FILENAME) . HEADING)) pairs collected
from scanned files.")

(defun lorg--scan-file (file)
  "Scan FILE for Org links and populate `lorg--links-cache-alist'.
Stop scanning when `lorg-max-links' entries have been added. Each link's
description and URI (type & path) are stored in the cache."
  (with-temp-buffer
    (let* ((filename (file-name-nondirectory file))
           (org-inhibit-startup t)
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
          (let* ((uri (match-string-no-properties 2))
                 (description (or (match-string-no-properties 3) uri))
                 (heading (save-excursion
                            (org-up-heading-safe)
                            (lorg--get-breadcrumbs)))
                 (context (org-element-lineage (org-element-context) 'link t))
                 (type (org-element-property :type context))
                 (path (if (equal type "file")
                           (expand-file-name (org-element-property :path context))
                         (org-element-property :path context))))
            (if (and description type path)
                (let ((uri (concat type ":" path)))
                  (push (cons description (cons (cons uri filename) heading))
                        lorg--links-cache-alist)
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
Tries `fd', `rg', `find', and a native Elisp fallback in that order,
using the first that succeeds to list files, then scans each for links."
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
  "Scan each element of FILES for Org links.
Each element may be a regular file or a directory."
  (let* ((file)
         (inhibit-read-only t))
    (while (setq file (pop files))
      (cond ((file-regular-p file)
             (lorg--scan-file file))
            ((file-directory-p file)
             (lorg--scan-directory file))))))

(defun lorg--annotation-function (str)
  "Annotation function for completion candidates.
This function returns the URI for STR as a right aligned annotation."
  (let ((entry (assoc str lorg--links-cache-alist)))
    (when entry
      (let ((uri (caadr entry)))
        (concat " "
                (propertize " " 'display '(space :align-to 40))
                (propertize (or uri "No uri")
                            'face (or 'marginalia-documentation
                                      'font-lock-comment-face)))))))

(defun lorg--affixation-function (cands)
  "Affixation function for completion display.
Aligns link descriptions and URLs in a columnar format. CANDS is a list
of completion candidates. This function looks up each candidate in
`lorg--links-cache-alist' and returns a list of (CANDIDATE DESCRIPTION
SUFFIX)"
  (when cands
    (let* ((max-margin 50)
           (lens (mapcar (lambda (cand) (length cand)) cands))
           (margin (max (+ (apply #'max lens) 4) max-margin)))
      (mapcar
       (lambda (cand)
         (let* ((desc cand)
                (entry (assoc cand lorg--links-cache-alist))
                (len (length desc))
                (uri (caadr entry))
                (spaces (make-string (- margin len) ?\s)))
           (list desc nil
                 (concat spaces
                         (propertize uri 'face (or 'marginalia-documentation
                                                   'font-lock-comment-face))))))
       cands))))

(defun lorg--get-breadcrumbs ()
  "Return the heading breadcrumb path at point as a string.
Walks up the heading hierarchy collecting headings, then joins them with
`lorg-group-breadcrumbs-splitter'. Return nil if point is not under any
heading."
  (let ((breadcrumbs nil))
    (save-excursion
      (condition-case nil
          (org-back-to-heading t)
        (user-error (goto-char (point-min))))
      (while (not (bobp))
        (push (org-link-display-format
               (substring-no-properties (org-get-heading t t t t)))
              breadcrumbs)
        (unless (org-up-heading-safe)
          (goto-char (point-min))))
      (when breadcrumbs
        (string-join breadcrumbs lorg-group-breadcrumbs-splitter)))))

(defun lorg--group-function (cand transform)
  "Group completion candidates according to `lorg-group-by'.
When TRANSFORM is non-nil, return CAND unchanged. Otherwise build a
group label from the file, heading path, or parent heading as
configured."
  (if transform
      cand
    (if (null lorg-group-by)
        nil
      (let* ((entry (assoc cand lorg--links-cache-alist))
             (uri+file (cadr entry))    ; (URI . FILENAME)
             (file (cdr uri+file))
             (heading (cddr entry))     ; full path heading
             (use-parent (memq 'parent lorg-group-by))
             (use-file (memq 'file lorg-group-by))
             (use-path (memq 'path lorg-group-by)))
        (cond
         ((and use-file use-parent) (or (and heading (concat file lorg-group-file-splitter (car (last (split-string heading lorg-group-breadcrumbs-splitter))))) file))
         ((and use-file use-path) (or (and heading (concat file lorg-group-file-splitter heading)) file))
         (use-file (or file "(No File)"))
         (use-parent (or (and heading (car (last (split-string heading lorg-group-breadcrumbs-splitter)))) "(No Heading)"))
         (use-path (or heading "(No Heading)"))
         (t "(No Group)"))))))

(defun lorg--completion-function (str pred flag)
  "Completion function for `completing-read'.
When FLAG is \\='metadata, return a metadata specification with
annotation, affixation, and grouping.  Otherwise, filter
completions matching STR by PRED."
  (cond ((eq flag 'metadata)
         `(metadata
           (category . lorg)
           (group-function . lorg--group-function)
           (annotation-function . lorg--annotation-function)
           (affixation-function . lorg--affixation-function)))
        (t
         (all-completions str (mapcar 'car lorg--links-cache-alist) pred))))

(defun lorg-menu-ask (prompt handler &optional force-rescan)
  "Prompt with PROMPT and call HANDLER with the selected link's URI.
If FORCE-RESCAN is non-nil or `lorg--links-cache-alist' is empty,
refresh the link cache from `lorg-files' before prompting."
  (when (or (null lorg--links-cache-alist) force-rescan)
    (setq lorg--links-cache-alist nil)
    (lorg--rescan-files lorg-files))
  (let* ((choice (completing-read prompt #'lorg--completion-function nil t))
         (uri (caadr (assoc choice lorg--links-cache-alist))))
    (funcall handler uri)))

;;;###autoload
(defun lorg-menu (&optional arg)
  "Interactively select and open an Org link from `lorg-files'.
With prefix ARG, force a rescan before prompting."
  (interactive "P")
  (lorg-menu-ask "Link: " #'org-link-open-from-string (and arg)))

(provide 'lorg)
;;; lorg.el ends here
