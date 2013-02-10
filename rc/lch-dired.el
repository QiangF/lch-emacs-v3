;;-*- coding:utf-8; mode:emacs-lisp; -*-

;;; DIRED.EL
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Chao LU
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


(provide 'lch-dired)
(message "~~ lch-dired: done.")

;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
