;; -*- coding:utf-8; mode:emacs-lisp; -*-

;;; CALENDAR
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary

;; commentary goes here.

;;; License

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
(message "=> lch-calendar: loading...")
(require 'calendar)
(setq calendar-latitude 40.34)
(setq calendar-longitude -74.65)
(setq calendar-location-name "Princeton, NJ")
(setq calendar-time-zone -360)
(setq calendar-standard-time-zone-name "EST")
(setq calendar-daylight-time-zone-name "EDT")

;; mark dates of holidays in the calendar
(setq calendar-mark-holidays-flag t)

;;; Binding
(lazy-unset-key
 '("a")
 calendar-mode-map)                     ;卸载按键
(lazy-set-key
 '(
   ("j" . calendar-forward-week)              ;下一个星期
   ("k" . calendar-backward-week)             ;上一个星期
   ("l" . calendar-forward-day)               ;下一天
   ("h" . calendar-backward-day)              ;上一天
   ("L" . calendar-forward-month)             ;下一月
   ("H" . calendar-backward-month)            ;上一月
   ("J" . calendar-forward-year)              ;下一年
   ("K" . calendar-backward-year)             ;上一年
   ("aw" . calendar-beginning-of-week)        ;一星期的第一天
   ("ew" . calendar-end-of-week)              ;一星期的最后一天
   ("am" . calendar-beginning-of-month)       ;一月的第一天
   ("em" . calendar-end-of-month)             ;一月的最后一天
   ("ay" . calendar-beginning-of-year)        ;一年的第一天
   ("ey" . calendar-end-of-year)              ;一年的最后一天
   (";" . calendar-goto-today)                ;跳到今天
   ("," . calendar-scroll-left)               ;向左滚动一月
   ("." . calendar-scroll-right)              ;向右滚动一月
   ("<" . calendar-scroll-left-three-months)  ;向左滚动三月
   (">" . calendar-scroll-right-three-months) ;向右滚动三月
   ("q" . calendar-exit)                      ;退出
   )
 calendar-mode-map)

(message "~~ lch-calendar: done.")
(provide 'lch-calendar)

;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
