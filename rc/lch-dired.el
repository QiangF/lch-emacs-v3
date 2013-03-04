;;-*- coding:utf-8; mode:emacs-lisp; -*-
;; -*- mode: emacs-lisp -*-
;;; DIRED.EL
;;
;; Copyright (c) 2006-2013 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; keybinding
;; c: terminal;
;; f: finder;
;; r: wdired;
;; s a: sort-show-all
;; s t: sort-by-date
;; s x: sort-by-extension
;; s s: sort-by-size
;; v: w3m-find-file;
;; V: ywb-w3m-find-file;
;; w: copy filename;
;; z: compress file;
;; * f: find-name-dired;
;; * g: grep-find;


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
(message "=> lch-dired: loading...")

(require 'dired)
(require 'dired-x)
(require 'dired-aux)
(require 'dired-single)
(require 'wdired)         ;; Part of Emacs since 22, treat dired buffer as file, easy to rename
(require 'ansi-color)
(require 'xwl-util)       ;; To load xwl-shell-command-asynchronously etc.
(require 'dircolors)
(ignore-errors (require 'emms-player-mplayer))
;; (require 'dired-sort-menu)


;; Allows recursive deletes
;; (setq dired-recursive-deletes 'top)
(setq dired-recursive-deletes 'always)

;; Reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  (revert-buffer))

;; Reload dired after quitting wdired
(defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
  (revert-buffer))

(setq dired-recursive-copies 'always)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "* f") 'find-name-dired)
(define-key dired-mode-map (kbd "* g") 'grep-find)
(define-key dired-mode-map (kbd "w")
  (lambda () (interactive) (dired-copy-filename-as-kill 0)))


;;; Util
;; Open current directory in Finder, Explorer, etc.
(define-key dired-mode-map (kbd "f")
  '(lambda ()
     (interactive)
     (let ((d (dired-current-directory)))
       (case window-system
         ((w32)
          (w32-shell-execute "open" d))
         ((ns mac)
          (xwl-shell-command-asynchronously (format "open -a Finder %s" d)))
         ((x)
          (xwl-shell-command-asynchronously (concat "nautilus --browser " d)))))))

;; Open current directory in a console/terminal
(define-key dired-mode-map (kbd "c")
  '(lambda ()
     (interactive)
     (let ((d (dired-current-directory)))
       (case window-system
         ((w32)
          (xwl-shell-command-asynchronously "start cmd.exe"))
         ((ns mac)
          (do-applescript (format "
tell application \"Terminal\"
  activate
  do script \"cd '%s'; bash\"
end tell" d)))
         ((x)
          (xwl-shell-command-asynchronously "gnome-terminal"))))))

;;; Sort
(setq dired-listing-switches "-lh")

;; Sort methods that affect future sessions
(defun dired-sort-by-default ()
  (interactive)
  (setq dired-listing-switches "-lh")
  (dired-sort-other dired-listing-switches))

(defun dired-sort-by-ctime ()
  "Dired sort by create time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ct")))

(defun dired-sort-by-utime ()
  "Dired sort by access time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ut")))

(defun dired-sort-by-time ()
  "Dired sort by time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))

(defun dired-sort-by-name ()
  "Dired sort by name."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "")))

(defun dired-sort-by-show-all ()
  (interactive)
  (setq dired-listing-switches "-lhA")
  (dired-sort-other dired-listing-switches))

;; Sort methods that affect current session only
(defun dired-sort-by-date ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "t")))

;; FIXME: fix this for mac. like: ls | rev | sort | rev
(defun dired-sort-by-extenstion ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "X")))

;; FIXME
(defun dired-sort-by-invisible-only ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "d .*")))

(defun dired-sort-by-size ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "S")))

(define-key dired-mode-map (kbd "s") nil)
(define-key dired-mode-map (kbd "s RET") 'dired-sort-by-default)
(define-key dired-mode-map (kbd "s a") 'dired-sort-by-show-all)
(define-key dired-mode-map (kbd "s t") 'dired-sort-by-date)
(define-key dired-mode-map (kbd "s x") 'dired-sort-by-extenstion)
(define-key dired-mode-map (kbd "s .") 'dired-sort-by-invisible-only)
(define-key dired-mode-map (kbd "s s") 'dired-sort-by-size)

(define-key dired-mode-map (kbd "<SPC>") 'dired-count-sizes)


(provide 'lch-dired)
(message "~~ lch-dired: done.")

;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
