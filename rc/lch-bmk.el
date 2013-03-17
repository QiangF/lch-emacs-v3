;;-*- coding:utf-8; mode:emacs-lisp; -*-

;;; BOOKMARK
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; commentary

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
(message "=> lch-bmk: loading...")

(require 'bookmark)
(defun switch-to-bookmark (bname)
  "Interactively switch to bookmark as `iswitchb' does."
  (interactive (list (flet ((ido-make-buffer-list
                             (default)
                             (bookmark-all-names)))
                       (ido-read-buffer "Jump to bookmark: " nil t))))
  (bookmark-jump bname))
(define-key global-map (kbd "C-c C-b") 'list-bookmarks)

(define-key global-map (kbd "<f5> b") 'list-bookmarks)
(define-key global-map (kbd "<f5> a") 'bookmark-set)
(define-key global-map (kbd "<f5> j") 'switch-to-bookmark)
(setq bookmark-save-flag 1)

;;; breadcrumb
(require 'breadcrumb)
(lazy-set-key
 '(
   ("s-7" . bc-local-next)              ;局部下一个
   ("s-8" . bc-local-previous)          ;局部上一个
   ("s-9" . bc-next)                    ;全局下一个
   ("s-0" . bc-previous)                ;全局上一个
   ("s-o" . bc-goto-current)            ;跳到当前
   ("s-l" . bc-list)                    ;书签列表
   ("s-'" . bc-set)                     ;书签设定
   ))
;; (lazy-set-key
;;  '(
;;    ("j" . next-line)                    ;下一行
;;    ("k" . previous-line)                ;上一行
;;    ("d" . bc-menu-mark-delete)          ;标记删除当前
;;    ("D" . bc-menu-mark-all-delete)      ;标记删除所有
;;    ("x" . bc-menu-commit-deletions)     ;确认删除
;;    ("u" . bc-menu-unmark-delete)        ;去标记当前
;;    ("U" . bc-menu-unmark-all-delete)    ;去标记所有
;;    ("v" . bc-menu-visit-other)          ;在其他窗口中浏览
;;    ("f" . bc-menu-jump)                 ;跳到书签处
;;    )
;;  *bc-menu-mode-map*
;;  )


(message "~~ lch-bmk: done.")
(provide 'lch-bmk)
