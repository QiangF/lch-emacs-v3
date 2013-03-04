;;-*- coding:utf-8; mode:emacs-lisp; -*-

;;; WEB.EL
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ;; Under MAC, has to install w3m through port, and add /opt/local to PATH
;; Have to use the CVS version of w3m for Emacs23
;; % cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login
;; CVS password: # No password is set.  Just hit Enter/Return key.
;; % cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m

;; `w3m-browse-url' asks Emacs-w3m to browse a URL.

;; When JavaScript is needed or the "design" is just too bad, use another
;; browser: you can open the page in your graphical browser (at your own
;; risk) by hitting `M' (`w3m-view-url-with-external-browser').
;; For what "risk" means, please see: (info "(emacs-w3m)Gnus")

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
(if lch-win32-p (add-to-list 'exec-path (concat emacs-dir "/bin/w3m")))
;; (setq w3-default-stylesheet "~/.default.css")
(require 'w3m)
(require 'w3m-lnum)

(defvar w3m-buffer-name-prefix "*w3m" "Name prefix of w3m buffer")
(defvar w3m-buffer-name (concat w3m-buffer-name-prefix "*") "Name of w3m buffer")
(defvar w3m-bookmark-buffer-name (concat w3m-buffer-name-prefix "-bookmark*") "Name of w3m buffer")

;;; General Setting
(defvar w3m-dir (concat emacs-var-dir "/w3m") "Dir of w3m.")

(setq w3m-icon-directory (concat w3m-dir "/icons"))
(setq w3m-default-save-directory "~/Downloads")

(setq w3m-use-cookies t)
(setq w3m-home-page "http://www.princeton.edu/~chaol")
(setq w3m-use-favicon nil)
(setq w3m-horizontal-shift-columns 1)

(setq w3m-use-header-line-title t)
(setq w3m-view-this-url-new-session-in-background t)
(setq w3m-new-session-in-background t)
(setq w3m-favicon-use-cache-file t)                     
(setq w3m-show-graphic-icons-in-mode-line nil)          
(setq w3m-keep-arrived-urls 50000)                      
(setq w3m-keep-cache-size 1000)                         
(setq w3m-default-display-inline-images nil)            
(setq w3m-toggle-inline-images-permanently t)           
(setq w3m-enable-google-feeling-lucky nil)              
(setq w3m-use-filter t)                                 
(setq w3m-fb-mode t)                                    
(w3m-fb-mode 1)                                         
(setq w3m-edit-function (quote find-file-other-window)) 

(defun w3m-new-tab ()
  (interactive)
  (w3m-copy-buffer nil nil nil t))
(define-key w3m-mode-map (kbd "C-t") 'w3m-new-tab)

(if lch-mac-p
    (progn
      (defun browse-url-firefox (url &optional new-window)
        (interactive (browse-url-interactive-arg "URL: "))
        (start-process (concat "open -a Firefox" url) nil "open" url))
      (setq browse-url-browser-function 'browse-url-firefox)))

;; Set browse function to be w3m
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;;; Webpages
;;; Utils
(defun lch-switch-to-w3m ()
  "Switch to an existing w3m buffer or look at bookmarks."
  (interactive)
  (let ((buf (get-buffer "*w3m*")))
    (if buf
        (switch-to-buffer buf)
      (w3m)
      (w3m-bookmark-view)
      )))
(define-key global-map (kbd "C-c w") 'lch-switch-to-w3m)

(defun lch-switch-to-w3m-goto-url ()
  (interactive)
  (let ((buf (get-buffer "*w3m*"))
        (w3m-current-url ""))
    (if buf
        (switch-to-buffer buf)
      (w3m))
    (w3m-new-tab)
    (call-interactively 'w3m-goto-url)
    ))
(define-key global-map (kbd "<f5> <f5>") 'lch-switch-to-w3m-goto-url)

(defun lch-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))
(define-key global-map (kbd "C-z g") 'lch-google)

;;; Bindings
(defun lch-w3m-mode-hook ()
  (define-key w3m-mode-map (kbd "b") '(lambda() (interactive) (w3m-new-tab) (w3m-bookmark-view)))
  (define-key w3m-mode-map (kbd "d") 'w3m-delete-buffer)
  (define-key w3m-mode-map (kbd "g") 'lch-google)
  (define-key w3m-mode-map (kbd "H") 'w3m-history)
  (define-key w3m-mode-map (kbd "M-h") 'w3m-db-history)
  (define-key w3m-mode-map (kbd "t") '(lambda() (interactive) (w3m-new-tab) (lch-w3m-goto-url)))
  (define-key w3m-mode-map (kbd "C-t") 'w3m-new-tab)
  (define-key w3m-mode-map (kbd "[") 'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "]") 'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "p") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "n") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd ",") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd ".") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "^") 'w3m-view-parent-pag)e
  (define-key w3m-mode-map (kbd "C-6") 'w3m-view-parent-page)
  (define-key w3m-mode-map (kbd "o") 'lch-w3m-goto-url)
  (define-key w3m-mode-map (kbd "O") 'w3m-goto-url-new-session)
  (define-key w3m-mode-map (kbd "s") 'w3m-search)
  (define-key w3m-mode-map (kbd "<up>") 'previous-line)
  (define-key w3m-mode-map (kbd "<down>") 'next-line)
  (define-key w3m-mode-map (kbd "<left>") 'backward-char)
  (define-key w3m-mode-map (kbd "<right>") 'forward-char)
  (define-key w3m-mode-map (kbd "<tab>") 'w3m-next-anchor)
  (define-key w3m-mode-map (kbd "}") 'w3m-next-image)
  (define-key w3m-mode-map (kbd "{") 'w3m-previous-image)
  (define-key w3m-mode-map (kbd ">") 'scroll-left)
  (define-key w3m-mode-map (kbd "<") 'scroll-right)
  (define-key w3m-mode-map (kbd "\\") 'w3m-view-source)
  (define-key w3m-mode-map (kbd "=") 'w3m-view-header)
  (define-key w3m-mode-map (kbd "C-<return>") 'w3m-view-this-url-new-session)
  ;; FIXME(define-key w3m-mode-map (kbd "<C-mouse-1>") 'w3m-view-this-url-new-session)
  (setq truncate-lines nil))
(add-hook 'w3m-mode-hook 'lch-w3m-mode-hook)


(provide 'lch-web)
;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
 