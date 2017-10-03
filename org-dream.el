;;; package --- Summary
;;; Commentary:
;;; Version:     20170904
;;; Code:

(defconst org-dream-directory-format "%Y-%m-%d"
  "Format of directories in `org-dream-home'.")

(defvar org-dream-home nil
  "Home where your dreams will be located in.")

(defvar org-dream-locations-dir nil
  "Directory where your dreams' locations will be located in.")

(defvar org-dream-diary-dir nil
  "Directory where your dreams will be stored in.")

(defvar org-dream-thoughts-dir nil
  "Directory where your dreams' thoughts will be located in.")

(defvar org-dream-parts-dir nil
  "Directory where your dreams' parts will be located in.")

(define-skeleton org-dream-initial-content
  "Initial content of dream files"
  "* Dream\n"
  "\n"
  "** Dream name\n"
  "\n"
  "** Dream plot\n"
  "\n"
  "** Dream situations\n"
  "\n")

(define-skeleton org-dream-part-initial-content
  "Initial content of dream files"
  "* Dream part\n"
  "\n"
  "** Plot\n"
  "\n"
  "** Situations\n"
  "\n")

(define-skeleton org-dream-location-initial-content
  "Initial content of dream files"
  "* Dream location\n"
  "\n"
  "** Location name\n"
  "\n"
  "** Location summary\n"
  "\n"
  "** Valuable dreams\n"
  "\n")

;; Private
(defun org-dream--try-setup ()
  (cl-macrolet ((--create-dir (loc)
                              `(let ((--location ,loc))
                                 (unless (f-dir-p --location)
                                   (if (and --location
                                            (stringp --location)
                                            (f-absolute-p --location))
                                       (f-mkdir --location)
                                     (error "org-dream--dry-setup: not a correct directory: %s" ,loc))))))
    (--create-dir org-dream-home)
    (--create-dir org-dream-locations-dir)
    (--create-dir org-dream-diary-dir)
    (--create-dir org-dream-parts-dir)
    (--create-dir org-dream-thoughts-dir)))

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
                                        "diary"))
  (setq org-dream-parts-dir     (f-join home-loc
                                        "parts"))
  (setq org-dream-thoughts-dir  (f-join home-loc
                                        "thoughts")))

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
    (org-dream-initial-content)))

(defun org-dream-new-location (location-name)
  (interactive)
  (org-dream--try-setup)
  (find-file (f-join org-dream-locations-dir
                     (format "%s.org" location-name)))
  (org-dream-location-initial-content))


(provide 'org-dream)
;;; org-dream.el ends here
