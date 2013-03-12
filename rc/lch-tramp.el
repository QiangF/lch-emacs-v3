;;-*- coding:utf-8; mode:emacs-lisp; -*-
;;; TRAMP.EL
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; (info "(emacs)Remote Files")
;; (info "(tramp)Top")
;;
;; TRAMP - Transparent Remote Access, Multiple Protocols
;; (other protocols than just FTP)
;;
;; Examples:
;; C-x C-f /method:user@host:/path/file
;; C-x C-f /ssh:loochao@server:/home/loochao/.bashrc
;; C-x C-f /plink:loochao@server:/home/loochao/.bashrc (from Windows)
;; C-x C-f /sudo:root@localhost:/etc/group
;; C-x C-f /su::/etc/hosts (Please note the double)

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

;;; Code:

;; (info "(tramp)Configuration") of TRAMP for use

;; (setq tramp-default-user "chaol"
;;       tramp-default-host "hats.princeton.edu")
;; (add-to-list 'tramp-default-method-alist
;;              '("hats.princeton.edu" "" "ssh"))
;; (add-to-list 'tramp-default-method-alist
;;              '("loochao" "" "sudo"))
;; (add-to-list 'tramp-default-user-alist
;;              '("" "hats.princeton.edu" "root"))

;; Default transfer method (info "(tramp)Default Method")
;; You might try out the `rsync' method, which saves the remote files
;; quite a bit faster than SSH. It's based on SSH, so it works the same,
;; just saves faster.
(setq tramp-default-method  ; `scp' by default
      (cond (lch-win32-p
             ;; (issues with Cygwin `ssh' which does not cooperate with
             ;; Emacs processes -> use `plink' from PuTTY, it definitely
             ;; does work under Windows)
             ;; C-x C-f /plink:myuser@host:/some/directory/file
             "plink")
            (t
             "ssh")))

;;; Default user (info "(tramp)Default User")
(setq tramp-default-user "chaol")
(setq tramp-default-method "ssh")                 ; 设置传送文件默认的方法
(setq tramp-default-host "hats.princeton.edu")

;;; How many seconds passwords are cached
;; (info "(tramp)Password handling") for several connections
(setq password-cache-expiry 36000)  ; default is 16

;;; String used for end of line in rsh connections
;; (info "(tramp)Remote shell setup") hints
(setq tramp-rsh-end-of-line  ; `\n' by default
      (cond (lch-win32-p
             "\n")
            (t
             "\r")))


;;; Faster auto saves (info "(tramp)Auto-save and Backup") configuration
(setq tramp-auto-save-directory temporary-file-directory)
;(setq tramp-remote-path (quote ("/usr/xpg4/bin" "/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/usr/ccs/bin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/p/perl/bin")))

;;; (info "(tramp)Traces and Profiles")
;;; Debugging
(setq tramp-verbose 9)  ; default is 0

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(define-key global-map (kbd "C-c f") 'sudo-edit)

(provide 'lch-tramp)
;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
