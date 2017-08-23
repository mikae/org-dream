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
  (when (f-dir-p --org-dream-location)
    (delete-directory --org-dream-location :recursive))
  (setq org-dream-location --org-dream-location))

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
                      :to-be t))
          (it "setups if no directory was found"
              (--before)
              (setq org-dream-location --org-dream-location)
              (org-dream-new-dream)
              (save-buffer)
              (kill-buffer)
              (expect (f-dir-p --org-dream-location) :to-be t))

          ;; (it "when org-dream-location is incorrect"
          ;;     (--before)
          ;;     (org-dream-new-dream))
          )
