;; -*- coding:utf-8; -*-

;;; UTIL.EL
;;
;; Copyright (c) 2010 2011 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code
;;; Automatically add execute permission to a script file.
(defun lch-chmod-x ()
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (not (file-executable-p buffer-file-name))
       (if (= 0 (shell-command (concat "chmod u+x " buffer-file-name)))
           (message
            (concat "Saved as script: " buffer-file-name)))))
(add-hook 'after-save-hook 'lch-chmod-x)
;;; Open-archive-file
(defun lch-toggle-archive ()
  (interactive)
  (let* ((buf (buffer-file-name (current-buffer)))
        (archive-buffer (concat (buffer-file-name (current-buffer)) "_archive"))
        (origin-buffer (replace-regexp-in-string "_archive" "" buf)))
    (if (string-match "archive" buf)
        (progn (if (file-exists-p origin-buffer)
                   (switch-to-buffer (find-file origin-buffer))
                 (switch-to-buffer (get-buffer-create archive-buffer))))
        (progn (if (file-exists-p archive-buffer)
                   (switch-to-buffer (find-file archive-buffer))
                 (switch-to-buffer (get-buffer-create archive-buffer)))))))
(define-key global-map (kbd "C-c a") 'lch-toggle-archive)

;;; Start file browser
(defun lch-start-file-browser ()
  "Open current pwd with file browser.
   Currently, just work under Mac OSX."
  (interactive)
  (let (mydir)
    (setq mydir (pwd))
    (string-match "Directory " mydir)
    (setq mydir (replace-match "" nil nil mydir 0))
    (when lch-mac-p (shell-command (format "open -a Finder %s" mydir)))
    ))
(define-key global-map (kbd "<f3> <f3>") 'lch-start-file-browser)

;;; Start terminal
(defun lch-start-terminal ()
  "Open current pwd with terminal.
   Currently, just work under Mac OSX."
  (interactive)
  (let (mydir)
    (setq mydir (pwd))
    (string-match "Directory " mydir)
    (setq mydir (replace-match "" nil nil mydir 0))
    (when lch-mac-p
      (do-applescript
       (format
        "tell application \"Terminal\"
activate
do script \"cd '%s'; bash \"
end tell" mydir)))
    ))
(define-key global-map (kbd "<f1> 1") 'lch-start-terminal)

;;; Switch or create *scratch*
(defun lch-create-switch-scratch ()
  (interactive)
  (let ((buf (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (when (null buf)
      (lisp-interaction-mode))))
(define-key global-map (kbd "C-c s") 'lch-create-switch-scratch)

;;; Nuke buffers
(defun nuke-some-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and (not (string-equal name ""))
           (not (string-equal name "*Messages*"))
           ;; (not (string-equal name "*Buffer List*"))
           (not (string-equal name "*buffer-selection*"))
           (not (string-equal name "*Shell Command Output*"))
           (not (string-equal name "*scratch*"))
           (/= (aref name 0) ? )
           (if (buffer-modified-p buffer)
               (if (yes-or-no-p
                    (format "Buffer %s has been edited. Kill? " name))
                   (kill-buffer buffer))
             (kill-buffer buffer))))
    (setq list (cdr list))))
(define-key global-map (kbd "C-c n") 'nuke-some-buffers)

;;; Provide
(message "~~ lch-util: done.")
(provide 'lch-util)

;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End: