;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-
(require 'org-dream)
(require 'f)

(defconst --org-dream-location  "/tmp/--org-dream-location")
(defconst --test-file-location  (f-join "/tmp/--org-dream-location"
                                        "test.org"))

(defun --before ()
  (setq org-dream-location nil)
  (when (f-dir-p --org-dream-location)
    (delete-directory --org-dream-location :recursive)))

(defun --setup ()
  (setq org-dream-location nil)
  (when (f-dir-p --org-dream-location)
    (delete-directory --org-dream-location :recursive))
  (setq org-dream-location --org-dream-location)
  (org-dream-setup))

(describe "setup"
  (it "does nothing when not configuried"
    (--before)
    (expect (org-dream-setup) :to-be nil))

  (it "creates a dir"
    (--before)
    (setq org-dream-location --org-dream-location)
    (org-dream-setup)
    (expect (f-dir-p --org-dream-location) :to-be t))

  (it "doesn't delete dir"
    (--before)
    (setq org-dream-location --org-dream-location)
    (f-mkdir org-dream-location)
    (with-temp-buffer (write-file --test-file-location))
    (org-dream-setup)
    (expect (f-file-p --test-file-location) :to-be t))
  )

(describe "new-dream"
  (it "creates dream directory for a dream"
    (--setup)
    (org-dream-new-dream)
    (save-buffer)
    (kill-buffer)
    (expect (f-dir-p (f-join --org-dream-location
                             (format-time-string "%Y-%m-%d")))
            :to-be t))

  (it "creates new dream file"
    (--setup)
    (org-dream-new-dream)
    (save-buffer)
    (kill-buffer)
    (expect (f-file-p (f-join --org-dream-location
                              (format-time-string "%Y-%m-%d")
                              "dream-1.org"))
            :to-be t))

  (it "can create multiple files"
    (--setup)
    (dotimes (--i 3)
      (org-dream-new-dream)
      (save-buffer)
      (kill-buffer)
      (expect (f-file-p (f-join --org-dream-location
                                (format-time-string "%Y-%m-%d")
                                (format "dream-%d.org" (1+ --i))))
              :to-be t)))
  (it "create file with specified content"
    (--setup)
    (org-dream-new-dream)
    (save-buffer)
    (kill-buffer)
    (expect (length (f-read-text (f-join --org-dream-location
                                         (format-time-string "%Y-%m-%d")
                                         (format "dream-1.org"))))
            :to-be-greater-than 0))

  (it "creates assets directory"
    (--setup)
    (org-dream-new-dream)
    (save-buffer)
    (kill-buffer)
    (expect (f-dir-p (f-join --org-dream-location
                             (format-time-string "%Y-%m-%d")
                             "assets"))
            :to-be t)))
