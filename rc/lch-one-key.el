;;; One-key
(require 'one-key)

;;; Setup-one-key
(defmacro setup-one-key (key title)
  (with-temp-buffer
    (insert "(progn")
    (one-key-insert-template key title)
    (insert (format "(global-set-key (kbd \"%s\") 'one-key-menu-%s)"
                    (regexp-quote key) title))
    (insert ")")
    (goto-char (point-min))
    (read (current-buffer))))

;;; Open Directory
(defvar lch-home-directory "")
(defvar lch-mldonkey-download-directory "")
(defvar lch-default-download-directory "")
(defvar lch-resource-backup-directory "")
(defvar lch-book-directory "")
(defvar lch-reading-directory "")
(defvar lch-translate-png-directory "")
(defvar lch-picture-directory "")
(defvar lch-lyrics-directory "")
(defvar lch-emacs-lisp-package-directory "")
(defvar lch-notes-directory "")
(defvar lch-emacs-backup-directory "")
(defvar lch-screenshots-storage-directory "")
(defvar lch-emlue-download-directory "")
(defvar lch-elisp-directory "")

(defvar one-key-menu-directory-alist nil
  "The `one-key' menu alist for DIRECTORY.")

(setq one-key-menu-directory-alist
      '(
        (("h" . "Home") . (lambda () (interactive) (dired-x-find-file lch-home-directory)))
        (("e" . "Emacs Backup") . (lambda () (interactive) (dired-x-find-file lch-emacs-backup-directory)))
        (("d" . "Download") . (lambda () (interactive) (dired-x-find-file lch-default-download-directory)))
        (("b" . "Book") . (lambda () (interactive) (dired-x-find-file lch-book-directory)))
        (("i" . "Image") . (lambda () (interactive) (dired-x-find-file lch-picture-directory)))
        (("p" . "Emacs Package") . (lambda () (interactive) (dired-x-find-file lch-emacs-lisp-package-directory)))
        (("k" . "Mldonkey") . (lambda () (interactive) (dired-x-find-file lch-mldonkey-download-directory)))
        (("m" . "Music") . (lambda () (interactive) (dired-x-find-file emms-source-file-default-directory)))
        (("s" . "Screenshots") . (lambda () (interactive) (dired-x-find-file lch-screenshots-storage-directory)))
        (("r" . "Resource Backup") . (lambda () (interactive) (dired-x-find-file lch-resource-backup-directory)))
        (("n" . "Notes") . (lambda () (interactive) (dired-x-find-file lch-notes-directory)))
        (("x" . "Reading") . (lambda () (interactive) (dired-x-find-file lch-reading-directory)))
        (("l" . "Lyrics") . (lambda () (interactive) (dired-x-find-file lch-lyrics-directory)))
        (("u" . "Emule") . (lambda () (interactive) (dired-x-find-file lch-emlue-download-directory)))
        (("z" . "Elisp") . (lambda () (interactive) (dired-x-find-file lch-elisp-directory)))
        ))

(defun one-key-menu-directory ()
  "The `one-key' menu for DIRECTORY."
  (interactive)
  (one-key-menu "DIRECTORY" one-key-menu-directory-alist t))
(setup-one-key "M-g" "one-key-menu-directory")

;;; Provide
(provide 'lch-one-key)

;;; Local Vars
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End: