;;-*- coding:utf-8; -*-

;;; EMMS.EL
;;
;; Copyright (c) 2006-2012 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Under MAC, need to $port install mplayer mp3info(but does not work for CN)!
;; $port install amixer for volume adjust.

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
(message "=> lch-emms: loading...")
(require 'emms-setup)
(emms-standard)
(emms-default-players)
;; NEWEST FEATURE. Use this if you like living on the edge.
(emms-devel)

(defvar emms-dir (concat emacs-var-dir "/emms"))                          
(setq emms-history-file (concat emms-dir "/emms-history"))                
(setq emms-cache-file (concat emms-dir "/cache"))                         
(setq emms-stream-bookmarks-file (concat emms-dir "/streams"))            
(setq emms-score-file (concat emms-dir "/scores"))                        

(setq emms-lyric-display-p nil)
(setq emms-playlist-buffer-name "*Music*")                                
(setq emms-source-file-default-directory "~/Dropbox/Music")
;; (setq emms-player-mplayer-parameters (list "-slave" "-nortc" "-quiet" "-really-quiet")
(when (eq system-type 'windows-nt)
  (setq emms-player-mplayer-command-name
        "d:/MM/MPLAYER/MPlayer/MPLAYER.EXE"))
(setq emms-player-list                                                    
      '(emms-player-mplayer
        emms-player-timidity
        emms-player-mpg321
        emms-player-ogg123))

(setq emms-repeat-playlist t)                                             

;; (set-face-foreground 'emms-playlist-selected-face "magenta")
;; (set-face-foreground 'emms-playlist-track-face  "green")

;; Prompt in minibuffer which track is playing when switch.
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "Now Playing: %s")                                 

(setq emms-playlist-sort-function
      'emms-playlist-sort-by-natural-order)

;;; Modeline
(emms-mode-line-disable)
(setq emms-playing-time-display-format "")
(require 'emms-mode-line-icon)
(setq emms-mode-line-titlebar-function 'emms-mode-line-playlist-current)

(setq emms-mode-line-icon-before-format "["
      emms-mode-line-format " %s]"
      emms-mode-line-icon-color "black")

;;; Encoding
;; (setq emms-info-mp3info-coding-system 'gbk
;;       emms-cache-file-codixng-system 'utf-8
;;       emms-i18n-default-coding-system '(utf-8 . utf-8))

;;; Utils
(defun lch-emms-add-dir ()
  (interactive)
  (call-interactively 'emms-add-directory-tree)
  (emms-playlist-mode-go))

(defun lch-emms-toggle-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-pause)
    (emms-start)))

;;; Binding
(defun lch-emms-init ()
  (interactive)
  (if (and (boundp 'emms-playlist-buffer)
           (buffer-live-p emms-playlist-buffer))
      (emms-playlist-mode-go)
    ;; (emms-playlist-mode-go-popup)
    ;; (if (y-or-n-p "EMMS not started, start it now? ")
    (progn
      (require 'lch-emms)
      (emms-add-directory-tree emms-source-file-default-directory)
      (emms-shuffle)
      (lch-emms-toggle-playing))))

(define-key global-map (kbd "<f12> <f12>") 'lch-emms-init)
(define-key global-map (kbd "<f12> SPC") 'lch-emms-toggle-playing)
(define-key global-map (kbd "<f12> c")   'emms-start)
(define-key global-map (kbd "<f12> x")   'emms-stop)

(define-key global-map (kbd "<f12> <f10>") 'lch-emms-add-dir)

(define-key global-map (kbd "<f12> n")   'emms-next)
(define-key global-map (kbd "<f12> p")   'emms-previous)

(define-key global-map (kbd "<f12> /")   'emms-show)
(define-key global-map (kbd "<f12> s")   'emms-shuffle)

(define-key global-map (kbd "<f12> r")   'emms-toggle-repeat-track)
(define-key global-map (kbd "<f12> R")   'emms-toggle-repeat-playlist)

(define-key emms-playlist-mode-map (kbd "<left>")  (lambda () (interactive) (emms-seek -10)))
(define-key emms-playlist-mode-map (kbd "<right>") (lambda () (interactive) (emms-seek +10)))
(define-key emms-playlist-mode-map (kbd "<down>")  (lambda () (interactive) (emms-seek -60)))
(define-key emms-playlist-mode-map (kbd "<up>")    (lambda () (interactive) (emms-seek +60)))

(provide 'lch-emms)
(message "~~ lch-emms: done.")

;;; Provide
(provide 'lch-emms)

;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:


















