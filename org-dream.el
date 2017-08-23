;;; package --- Summary
;;; Commentary:
;; Version:     20170822
;;; Code:
(defvar org-dream-location nil
  "Location where your dreams will be stored in.")

(defconst org-dream-directory-format "%Y-%m-%d"
  "Format of directories in `org-dream-location'.")

(define-skeleton org-dream-file-initial-content
  "Initial content of dream files"
  ""
  "* Dream"
  ""
  "** Dream name"
  ""
  "** Dream summary")

;; Private
(defun org-dream--try-setup ()
  (unless (f-dir-p org-dream-location)
    (if (and org-dream-location
             (stringp org-dream-location)
             (f-absolute-p org-dream-location))
        (f-mkdir org-dream-location)
      (error "`org-dream-location' must be correct absolute path"))))

(defun org-dream--generate-new-filename (dream-dir)
  "Generates name for new dream file."
  (let ((--files (directory-files dream-dir nil "dream-[0-9]+.org")))
    (format "dream-%d.org" (1+ (length --files)))))

;; Public
(defun org-dream-new-dream ()
  "Create new dream entry."
  (interactive)
  (org-dream--try-setup)

  (let* ((--dream-dir (f-join org-dream-location
                              (format-time-string org-dream-directory-format)))
         (--assets-dir (f-join --dream-dir
                               "assets")))
    (unless (f-dir-p --dream-dir)
      (f-mkdir --dream-dir))
    (unless (f-dir-p --assets-dir)
      (f-mkdir --assets-dir))

    (find-file (f-join --dream-dir
                       (org-dream--generate-new-filename --dream-dir)))
    (org-dream-file-initial-content)))


(provide 'org-dream)
;;; org-dream.el ends here
