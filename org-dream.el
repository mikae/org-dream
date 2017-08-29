;;; package --- Summary
;;; Commentary:
;; Version:     20170822
;;; Code:
(defvar org-dream-home nil
  "home where your dreams will be stored in.")

(defvar org-dream-locations-dir nil
  "home where some locations in your dreams will be stored in.")

(defvar org-dream-diary-dir nil
  "home where some locations in your dreams will be stored in.")

(defconst org-dream-directory-format "%Y-%m-%d"
  "Format of directories in `org-dream-home'.")

(define-skeleton org-dream-file-initial-content
  "Initial content of dream files"
  ""
  "* Dream"
  ""
  "** Dream name"
  ""
  "** Dream summary")

(define-skeleton org-dream-location-initial-content
  "Initial content of dream files"
  ""
  "* Location"
  ""
  "** Location name"
  ""
  "** Location summary")

;; Private
(defun org-dream--try-setup ()
  (cl-macrolet ((--create-dir (loc)
                              `(let ((--location ,loc))
                                 (unless (f-dir-p --location)
                                   (if (and --location
                                            (stringp --location)
                                            (f-absolute-p --location))
                                       (f-mkdir --location)
                                     (error "`%s' must be correct absolute path" ,loc))))))
    (--create-dir org-dream-home)
    (--create-dir org-dream-locations-dir)
    (--create-dir org-dream-diary-dir)))

(defun org-dream--generate-new-filename (dream-dir)
  "Generates name for new dream file."
  (let ((--files (directory-files dream-dir nil "dream-[0-9]+.org")))
    (format "dream-%d.org" (1+ (length --files)))))

;; Public
(defun org-dream-set-home (home-loc)
  "Setup home directory of `org-dream'."
  (setq org-dream-home          home-loc)
  (setq org-dream-locations-dir (f-join home-loc
                                        "locations"))
  (setq org-dream-diary-dir     (f-join home-loc
                                        "diary")))

(defun org-dream-new-dream ()
  "Create new dream entry."
  (interactive)
  (org-dream--try-setup)

  (let* ((--dream-dir (f-join org-dream-diary-dir
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

(defun org-dream-new-location (location-name)
  (interactive)
  (org-dream--try-setup)
  (find-file (f-join org-dream-locations-dir
                     (format "%s.org" location-name)))
  (org-dream-location-initial-content))


(provide 'org-dream)
;;; org-dream.el ends here
