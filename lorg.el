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

(defcustom lorg-cache-backend 'hash-table
  "Backend data structure for the link cache.
`alist' uses an association list (simple, handles duplicate
descriptions).  `hash-table' uses a hash table for O(1) lookups.
Rescan needed for effect."
  :type '(choice (const :tag "Association list" alist)
          (const :tag "Hash table" hash-table))
  :group 'lorg)

(defvar lorg--cache nil
  "Internal link cache. Structure depends on `lorg-cache-backend'")

(defun lorg--cache-clear ()
  "Initialize or clear the links cache."
  (setq lorg--cache
        (if (eq lorg-cache-backend 'hash-table)
            (make-hash-table :test 'equal :size lorg-max-links)
          nil)))

(defun lorg--cache-empty-p ()
  "Return non-nil if the cache is empty or uninitialized"
  (if (hash-table-p lorg--cache)
      (= 0 (hash-table-count lorg--cache))
    (null lorg--cache)))

(defun lorg--cache-put (description uri filename heading)
  "Store a link entry in the cache under DESCRIPTION"
  (if (hash-table-p lorg--cache)
      (puthash description (cons (cons uri filename) heading) lorg--cache)
    (push (cons description (cons (cons uri filename) heading)) lorg--cache)))

(defun lorg--cache-get (description)
  "Look up DESCRIPTION in the cache.
Return ((URI . FILENAME) . HEADING) or nil"
  (if (hash-table-p lorg--cache)
      (gethash description lorg--cache)
    (cdr (assoc description lorg--cache))))

(defun lorg--cache-keys ()
  "Return list of all description strings in the cache."
  (if (hash-table-p lorg--cache)
      (hash-table-keys lorg--cache)
    (mapcar #'car lorg--cache)))

(defun lorg--cache-count ()
  "Return the number of entries in the cache"
  (if (hash-table-p lorg--cache)
      (hash-table-count lorg--cache)
    (length lorg--cache)))

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
           (coding-system-for-read 'utf-8)
           (heading-stack nil)          ; List of '(level title) entries
           (count (lorg--cache-count)))
      (org-mode)
      (buffer-disable-undo)
      (insert-file-contents file)
      (goto-char (point-min))
      (catch ':max-limit-reached
        (while (not (eobp))
          ;; Search for links on this line
          (let ((eol (line-end-position)))
            (while (re-search-forward lorg-link-re eol t)
              (let* ((raw-uri (match-string-no-properties 2))
                     (description (or (match-string-no-properties 3) raw-uri)))
                (when raw-uri
                  (let* ((type nil) (path nil))
                    (cond
                     ((string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):\\(.*\\)" raw-uri)
                      ;; type:path (http:, file:, id:, mailto:, info:, ...)
                      (setq type (match-string 1 raw-uri)
                            path (match-string 2 raw-uri)))
                     ((string-match "\\`[/~.]" raw-uri)
                      (setq type "file"
                            path raw-uri)))
                    (let ((uri (concat type ":" path))
                          (heading (when heading-stack
                                     (mapconcat #'cdr
                                                (reverse heading-stack)
                                                lorg-group-breadcrumbs-splitter))))
                      (lorg--cache-put description uri filename heading)
                      (when (>= (setq count (+ count 1)) lorg-max-links)
                        (throw ':max-limit-reached t))))))))
          ;; Update heading stack on a heading line
          (beginning-of-line)
          (when (org-at-heading-p)
            (let* ((level (org-current-level))
                   (title (org-link-display-format
                           (substring-no-properties
                            (org-get-heading t t t t)))))
              (while (and heading-stack (>= (caar heading-stack) level))
                (pop heading-stack))
              (push (cons level title) heading-stack)))
          (forward-line 1))))))

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

(defun lorg--annotation-function (desc)
  "Annotation function for completion candidates.
This function returns the URI for DESC as a right aligned annotation."
  (let ((entry (lorg--cache-get desc)))
    (when entry
      (let ((uri (caar entry)))
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
                (entry (lorg--cache-get cand))
                (len (length desc))
                (uri (caar entry))
                (spaces (make-string (- margin len) ?\s)))
           (list desc nil
                 (concat spaces
                         (propertize uri 'face (or 'marginalia-documentation
                                                   'font-lock-comment-face))))))
       cands))))

(defun lorg--group-function (cand transform)
  "Group completion candidates according to `lorg-group-by'.
When TRANSFORM is non-nil, return CAND unchanged. Otherwise build a
group label from the file, heading path, or parent heading as
configured."
  (if transform
      cand
    (if (null lorg-group-by)
        nil
      (let* ((entry (lorg--cache-get cand))
             (uri+file (car entry))    ; (URI . FILENAME)
             (file (cdr uri+file))
             (heading (cdr entry))     ; full path heading
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
         (all-completions str (lorg--cache-keys) pred))))

(defun lorg-menu-ask (prompt handler &optional force-rescan)
  "Prompt with PROMPT and call HANDLER with the selected link's URI.
If FORCE-RESCAN is non-nil or `lorg--links-cache-alist' is empty,
refresh the link cache from `lorg-files' before prompting."
  (when (or (lorg--cache-empty-p) force-rescan)
    (lorg--cache-clear)
    (lorg--rescan-files lorg-files))
  (let* ((choice (completing-read prompt #'lorg--completion-function nil t))
         (uri (caar (lorg--cache-get choice))))
    (funcall handler uri)))

;;;###autoload
(defun lorg-menu (&optional arg)
  "Interactively select and open an Org link from `lorg-files'.
With prefix ARG, force a rescan before prompting."
  (interactive "P")
  (lorg-menu-ask "Link: " #'org-link-open-from-string (and arg)))

(provide 'lorg)
;;; lorg.el ends here
