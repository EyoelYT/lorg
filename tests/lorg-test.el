;;; lorg-test.el --- Tests for lorg.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'lorg)

(ert-deftest lorg-test-org-link-parsing ()
  "Test that `lorg--scan-org-file' correctly extracts URIs and descriptions."
  (let ((test-cases '(("[[https://example.com][Example]]" "https://example.com" "Example")
                      ("<https://example.com>" "https://example.com" "https://example.com")
                      ("https://example.com" "https://example.com" "https://example.com"))))
    (dolist (case test-cases)
      (let ((input (nth 0 case))
            (expected-uri (nth 1 case))
            (expected-desc (nth 2 case)))
        (with-temp-buffer
          (insert input)
          (goto-char (point-min))
          ;; Run the matching logic used in `lorg--scan-org-file'
          (if (re-search-forward lorg-link-re (line-end-position) t)
              (let* ((whole (match-string-no-properties 0))
                     (raw-uri (or (match-string-no-properties 2)
                                  (and (string-prefix-p "<" whole)
                                       (substring whole 1 -1))
                                  whole))
                     (description (or (match-string-no-properties 3) raw-uri)))
                (should (string= raw-uri expected-uri))
                (should (string= description expected-desc)))
            ;; Fail the test when the regex fails to match
            (ert-fail (format "Failed to match input: %s" input))))))))

(provide 'lorg-test)
;;; lorg-test.el ends here
