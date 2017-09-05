;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-
(require 'org-dream)
(require 'f)

(defconst --org-dream-home          "/tmp/--org-dream-home")

(defconst --org-dream-locations-dir  (f-join --org-dream-home
                                             "locations"))

(defconst --org-dream-diary-dir  (f-join --org-dream-home
                                         "diary"))

(defconst --org-dream-parts-dir  (f-join --org-dream-home
                                         "parts"))

(defconst --org-dream-thoughts-dir  (f-join --org-dream-home
                                            "thoughts"))

(defconst --test-file-home  (f-join "/tmp/--org-dream-home"
                                    "test.org"))

(defun --before ()
  (setq org-dream-home nil)
  (setq org-dream-locations-dir nil)
  (setq org-dream-diary-dir nil)
  (when (f-dir-p --org-dream-home)
    (delete-directory --org-dream-home :recursive)))

(defun --setup ()
  (when (f-dir-p --org-dream-home)
    (delete-directory --org-dream-home :recursive))
  (org-dream-set-home --org-dream-home))

(describe "set-home"
          (before-each
           (--before))

          (it "sets home directory for org-dream"
              (org-dream-set-home --org-dream-home)
              (expect org-dream-home :to-equal --org-dream-home))

          (it "sets dream locations directory for org-dream"
              (org-dream-set-home --org-dream-home)
              (expect org-dream-locations-dir :to-equal --org-dream-locations-dir))

          (it "sets dream dreams directory for org-dream"
              (org-dream-set-home --org-dream-home)
              (expect org-dream-diary-dir :to-equal --org-dream-diary-dir))

          (it "sets dream thoughts directory for org-dream"
              (org-dream-set-home --org-dream-home)
              (expect org-dream-thoughts-dir :to-equal --org-dream-thoughts-dir))

          (it "sets dream parts directory for org-dream"
              (org-dream-set-home --org-dream-home)
              (expect org-dream-parts-dir :to-equal --org-dream-parts-dir)))

(describe "new-dream"
          (it "creates dream directory for a dream"
              (--setup)
              (org-dream-new-dream)
              (save-buffer)
              (kill-buffer)
              (expect (f-dir-p (f-join --org-dream-diary-dir
                                       (format-time-string "%Y-%m-%d")))
                      :to-be t))

          (it "creates new dream file"
              (--setup)
              (org-dream-new-dream)
              (save-buffer)
              (kill-buffer)
              (expect (f-file-p (f-join --org-dream-diary-dir
                                        (format-time-string "%Y-%m-%d")
                                        "dream-1.org"))
                      :to-be t))

          (it "can create multiple files"
              (--setup)
              (dotimes (--i 3)
                (org-dream-new-dream)
                (save-buffer)
                (kill-buffer)
                (expect (f-file-p (f-join --org-dream-diary-dir
                                          (format-time-string "%Y-%m-%d")
                                          (format "dream-%d.org" (1+ --i))))
                        :to-be t)))
          (it "create file with specified content"
              (--setup)
              (org-dream-new-dream)
              (save-buffer)
              (kill-buffer)
              (expect (length (f-read-text (f-join --org-dream-diary-dir
                                                   (format-time-string "%Y-%m-%d")
                                                   (format "dream-1.org"))))
                      :to-be-greater-than 0))

          (it "creates assets directory"
              (--setup)
              (org-dream-new-dream)
              (save-buffer)
              (kill-buffer)
              (expect (f-dir-p (f-join --org-dream-diary-dir
                                       (format-time-string "%Y-%m-%d")
                                       "assets"))
                      :to-be t))
          (it "setups if no directory was found"
              (--before)
              (org-dream-set-home --org-dream-home)
              (org-dream-new-dream)
              (save-buffer)
              (kill-buffer)
              (expect (f-dir-p --org-dream-home) :to-be t)
              (expect (f-dir-p --org-dream-locations-dir) :to-be t)
              (expect (f-dir-p --org-dream-diary-dir) :to-be t)))

(describe "new location"
          (it "creates new location file"
              (--setup)
              (org-dream-new-location "house")
              (save-buffer)
              (kill-buffer)
              (expect (f-file-p (f-join --org-dream-locations-dir
                                        "house.org"))
                      :to-be t))

          (it "create file with specified content"
              (--setup)
              (org-dream-new-location "caves-1")
              (save-buffer)
              (kill-buffer)
              (expect (length (f-read-text (f-join --org-dream-locations-dir
                                                   (format "caves-1.org"))))
                      :to-be-greater-than 0))

          (it "setups if no directory was found"
              (--before)
              (org-dream-set-home --org-dream-home)
              (org-dream-new-location "house")
              (save-buffer)
              (kill-buffer)
              (expect (f-dir-p --org-dream-home) :to-be t)
              (expect (f-dir-p --org-dream-locations-dir) :to-be t)
              (expect (f-dir-p --org-dream-diary-dir) :to-be t)))

(describe "New thought"
          (it ""))
