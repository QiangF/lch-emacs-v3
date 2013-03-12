;;-*- coding:utf-8; mode:emacs-lisp; -*-

;;; BINDINGS.EL
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Global bindings

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
;; (info "(emacs)Key Bindings")
(message "=> lch-binding: loading...")
;;; Require
(require 'one-key)
(require 'one-key-config)
(require 'lazy-set-key)
;; (setq one-key-popup-window nil)
;;; Super (command-test-map)
(lazy-set-key
 '(
   ("s-e" . multi-term-next)                 ;下一个终端
   ("s-w" . multi-term-prev)                 ;上一个终端
   ("s-n" . multi-term)                      ;新建一个终端
   ;; ("s-x s-x" . multi-term-dedicated-toggle) ;切换专注终端
   ;; ("s-x s-z" . multi-term-dedicated-select) ;选择专注终端
   ))
;;; Meta (command-map)
(define-key global-map (kbd "M-1") 'shell)

;; One-key-menu-meta
(defvar one-key-menu-meta-alist nil "")
(setq one-key-menu-meta-alist
      '(
        (("1" . "dumb-shell") . shell)                                          ;; => lch-binding.el
        (("<left>" . "hide-body") . hide-body)                                  ;; => lch-outline.el
        (("<right>" . "show-all") . show-all)                                   ;; => lch-outline.el
        (("<up>" . "outline-previous-heading") . outline-previous-heading)      ;; => lch-outline.el
        (("<down>" . "outline-next-heading") . outline-next-heading)            ;; => lch-outline.el
        (("j" . "tabbar-backward-tab") . tabbar-backward-tab)   
        (("k" . "tabbar-forward-tab") . tabbar-forward-tab)    
        (("8" . "tabbar-backward-group") . tabbar-backward-group) 
        (("9" . "tabbar-forward-group") . tabbar-forward-group) 
        ;; (("J" . "tabbar-select-beg-tab") . tabbar-select-beg-tab) 
        ;; (("K" . "tabbar-select-end-tab") . tabbar-select-end-tab) 
        (("s" . "lch-dict-search") . lch-dict-search)                           ;; => lch-dict.el
        ))

(defun one-key-menu-meta ()
  "The `one-key' menu for META."
  (interactive)
  (one-key-menu "META" one-key-menu-meta-alist t))
(define-key global-map (kbd "M-m") 'one-key-menu-meta)

;;; Ctrl (command-map)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(define-key global-map (kbd "C-2") 'set-mark-command)
(defun lch-dired-jump ()
  (interactive)
  (require 'lch-dired)
  (call-interactively 'dired-jump))
(define-key global-map (kbd "C-6") 'lch-dired-jump)

;; One-key-menu-ctrl
(defvar one-key-menu-ctrl-alist nil "")
(setq one-key-menu-ctrl-alist
      '(
        (("0" . "magnify-normal") . text-scale-normal)                          ;; => lch-util.el
        (("2" . "set-mark-command") . set-mark-command)                         ;; => lch-binding.el
        (("6" . "lch-dired-jump") . lch-dired-jump)                             ;; => lch-elisp.el
        (("=" . "magnify-font") . text-scale-increase)                          ;; => lch-binding.el
        (("-" . "demagnify-font") . text-scale-decrease)                        ;; => lch-binding.el
        ))

(defun one-key-menu-ctrl ()
  "The `one-key' menu for CTRL."
  (interactive)
  (one-key-menu "CTRL" one-key-menu-ctrl-alist t))
(define-key global-map (kbd "C-M-m") 'one-key-menu-ctrl)

;;; C-x (command-map)
;; One-key-menu-ctrl-x
(defvar one-key-menu-ctrl-x-alist nil "")
(setq one-key-menu-ctrl-x-alist
      '(
        (("f" . "ffap") . ffap)                                                 ;; => lch-elisp.el
        (("C-r" . "recentf-open-files") . recentf-open-files)                   ;; => lch-elisp.el
        (("C-z" . "toggle-w3m") . toggle-w3m-with-other-buffer)                 ;; => lch-web.el
        ))

(defun one-key-menu-ctrl-x ()
  "The `one-key' menu for CTRL-X."
  (interactive)
  (one-key-menu "CTRL-X" one-key-menu-ctrl-x-alist t))
(define-key global-map (kbd "C-x m") 'one-key-menu-ctrl-x)

;;; C-c (command-map)
(define-key global-map (kbd "C-c .") 'repeat-complex-command)
(define-key global-map (kbd "C-c c") 'comment-region)

(defun lch-eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "%s: EVALED" (buffer-name (current-buffer))))
(define-key global-map (kbd "C-c e") 'lch-eval-buffer)

(define-key global-map (kbd "C-c g") 'grep-find)
(define-key global-map (kbd "C-c l") 'goto-line)
(defun lch-occur ()
  (interactive)
  (call-interactively 'occur)
  (other-window 1))
(global-set-key (kbd "C-c o") 'lch-occur)

(define-key global-map (kbd "C-c p") 'print-buffer)
(define-key global-map (kbd "C-c u") 'uncomment-region)
(define-key global-map (kbd "C-c 4") 'toggle-truncate-lines)                   ;; Shift+4 == $

;; One-key-menu-ctrl-c
(defvar one-key-menu-ctrl-c-alist nil "")
(setq one-key-menu-ctrl-c-alist
      '(
        (("." . "repeat-complex-command") . repeat-complex-command)             ;; => lch-binding.el
        (("4" . "toggle-truncate-lines") . toggle-truncate-lines)               ;; => lch-binding.el
        (("a" . "lch-toggle-archive") . lch-toggle-archive)                     ;; => lch-util.el
        (("c" . "comment-region") . comment-region)                             ;; => lch-binding.el
        (("e" . "eval-buffer") . lch-eval-buffer)                               ;; => lch-binding.el
        (("g" . "grep-find") . grep-find)                                       ;; => lch-binding.el
        (("l" . "goto-line") . goto-line)                                       ;; => lch-binding.el
        (("n" . "nuke-buffer") . nuke-some-buffers)                             ;; => lch-util.el
        (("o" . "lch-occur") . lch-occur)                                       ;; => lch-binding.el
        (("p" . "print-buffer") . print-buffer)                                 ;; => lch-binding.el
        (("s" . "switch-to-scratch") . lch-create-switch-scratch)               ;; => lch-util.el
        (("u" . "uncomment-region") . uncomment-region)                         ;; => lch-binding.el
        (("v" . "view-mode") . view-mode)                                       ;; => lch-elisp.el
        (("w" . "w3m") . lch-switch-to-w3m)                                     ;; => lch-web.el
        (("C-b" . "list-bookmarks") . list-bookmarks)                           ;; => lch-bmk.el
        ))

(defun one-key-menu-ctrl-c ()
  "The `one-key' menu for CTRL-C."
  (interactive)
  (one-key-menu "CTRL-C" one-key-menu-ctrl-c-alist t))
(define-key global-map (kbd "C-c m") 'one-key-menu-ctrl-c)

;;; C-z (command-map)
;; One-key-menu-ctrl-z
(defvar one-key-menu-ctrl-z-alist nil "")
(setq one-key-menu-ctrl-z-alist
      '(
        (("e" . "erc") . lch-erc-emacs)                                         ;; => lch-erc.el
        (("g" . "google") . lch-google)                                         ;; => lch-web.el
        (("p" . "process") . process)                                           ;; => lch-util.el
        (("z" . "w3m-background") . w3m-startup-background)                     ;; => lch-web.el
        (("C-x" . "toggle-w3m") . toggle-w3m-with-other-buffer)                 ;; => lch-web.el
        (("C-z" . "w3m") . w3m)                                                 ;; => lch-web.el
        ))

(defun one-key-menu-ctrl-z ()
  "The `one-key' menu for CTRL-Z."
  (interactive)
  (one-key-menu "CTRL-Z" one-key-menu-ctrl-z-alist t))
(define-key global-map (kbd "C-z m") 'one-key-menu-ctrl-z)

;;; C-/ (command-map)
(defvar one-key-menu-ctrl-/-alist nil "")
(setq one-key-menu-ctrl-/-alist
      '(
        (("C-/" . "anything") . anything)                                              ;; => lch-erc.el
        (("/" . "anything-source") . anything-call-source)                             ;; => lch-erc.el
        ))

(defun one-key-menu-ctrl-/ ()
  "The `one-key' menu for CTRL-/."
  (interactive)
  (one-key-menu "CTRL-/" one-key-menu-ctrl-/-alist t))
(define-key global-map (kbd "C-/ m") 'one-key-menu-ctrl-/)
;;; Fn:  (command-map)
(defvar one-key-menu-fn-alist nil "")
(setq one-key-menu-fn-alist
      '(
        (("<f2>" . "goto-last-change") . goto-last-change)                      ;; => lch-elisp.el
        (("<f3>" . "lch-start-file-browser") . lch-start-file-browser)          ;; => lch-util.el
        (("<f4>" . "FIXME") . list-colors-display)
        (("<f5>" . "bm-toggle") . bm-toggle)                                    ;; => lch-elisp.el
        (("<f6>" . "FIXME") . erase-buffer)
        (("<f7>" . "dictionary-search") . dictionary-search)                    ;; => lch-dict.el
        (("<f8>" . "org-agenda") . org-agenda)                                  ;; => lch-org.el
        (("<f9>" . "highlight-symbol-at-point") . highlight-symbol-at-point)    ;; => lch-elisp.el
        (("<f10>" . "dictionary-match-words") . dictionary-match-words)
        (("<f11>" . "color-theme-arjen") . color-theme-arjen)                   ;; => lch-ui-theme.el
        (("<f12>" . "lch-emms-init") . lch-emms-init)                           ;; => lch-emms.el
        ))

(defun one-key-menu-fn ()
  "The `one-key' menu for FN."
  (interactive)
  (one-key-menu "FN" one-key-menu-fn-alist t))
(define-key global-map (kbd "<f1> M") 'one-key-menu-fn)

;;; C-Fn: (command-map)
(define-key global-map (kbd "C-<f1>") 'one-key-menu-help)
(define-key global-map (kbd "C-<f3>") 'one-key-menu-directory)
(defvar one-key-menu-C-fn-alist nil "")
(setq one-key-menu-C-fn-alist
      '(
        (("C-<f1>" . "one-key-menu-help") . one-key-menu-help)                  ;; => lch-binding.el
        (("C-<f2>" . "one-key-menu-thing-edit") . one-key-menu-thing-edit)      ;; => lch-elisp.el
        (("C-<f3>" . "one-key-menu-directory") . one-key-menu-directory)        ;; => lch-binding.el
        ))

(defun one-key-menu-C-fn ()
  "The `one-key' menu for C-FN."
  (interactive)
  (one-key-menu "C-FN" one-key-menu-C-fn-alist t))
(define-key global-map (kbd "M-C-<f1>") 'one-key-menu-C-fn)
;;; F1: (command-map)
(lazy-set-key
 '(
   ("<f1> ." . repeat-complex-command)
   ("<f1> C" . list-colors-display)
   ("<f1> e" . erase-buffer)
   ("<f1> f" . fill-region)
   ("<f1> p" . list-packages)
   ))

;; One-key-menu-command
(defvar one-key-menu-command-alist nil "")
(setq one-key-menu-command-alist
      '(
        (("<f2>" . "shell-pop") . shell-pop)                                    ;; => lch-elisp.el
        (("." . "repeat-complex-command") . repeat-complex-command)             ;; => lch-binding.el
        (("c" . "lch-cleanup-buffer") . lch-cleanup-buffer)                     ;; => lch-util.el
        (("C" . "list-color-display") . list-colors-display)                    ;; => lch-binding.el
        (("D" . "lch-delicious-url") . lch-delicious-url)                       ;; => lch-web.el
        (("e" . "erase-buffer") . erase-buffer)                                 ;; => lch-binding.el
        (("f" . "fill-region") . fill-region)                                   ;; => lch-binding.el
        (("g" . "magit-status") . magit-status)                                 ;; => lch-elisp.el
        (("i" . "lch-indent-region-or-buffer") . lch-indent-region-or-buffer)   ;; => lch-util.el
        (("C-m" . "dictionary-match-words") . dictionary-match-words)           ;; => lch-elisp.el
        (("p" . "list-package") . list-package)                                 ;; => lch-binding.el
        (("w" . "ywb-favorite-window-config") . ywb-favorite-window-config)     ;; => lch-util.el
        ))

(defun one-key-menu-command ()
  "The `one-key' menu for COMMAND."
  (interactive)
  (one-key-menu "COMMAND" one-key-menu-command-alist t))
(define-key global-map (kbd "<f1> m") 'one-key-menu-f1)

;;; F2: (mode-map)
(lazy-set-key
 '(
   ("<f2> a" . anything)
   ("<f2> c" . calendar)
   ("<f2> C" . calc)                        
   ("<f2> d" . dired)
   ("<f2> f" . auto-fill-mode)
   ("<f2> l" . lisp-mode)
   ("<f2> o" . org-mode)
   ("<f2> O" . outline-minor-mode)
   ("<f2> s" . flyspell-mode)
   ("<f2> t" . twittering-mode)
   ("<f2> w" . whitespace-mode)
   ("<f2> y" . yas-minor-mode)
   ))

;; One-key-menu-mode
(defvar one-key-menu-mode-alist nil "")
(setq one-key-menu-mode-alist
      '(
        (("a" . "anything") . anything)                                         ;; => lch-binding.el
        (("c" . "calendar") . calendar)                                         ;; => lch-binding.el
        (("C" . "calc") . calc)                                                 ;; => lch-binding.el 
        (("d" . "dired") . dired)                                               ;; => lch-binding.el
        (("e" . "evil-mode") . evil-mode)                                       ;; => lch-elisp.el
        (("E" . "start-irc") . start-irc)                                       ;; => lch-erc.el
        (("f" . "auto-fill-mode") . auto-fill-mode)                             ;; => lch-binding.el
        (("l" . "lisp-mode") . lisp-mode)                                       ;; => lch-binding.el
        (("o" . "org-mode") . org-mode)                                         ;; => lch-binding.el
        (("O" . "outline-minor-mode") . outline-minor-mode)                     ;; => lch-binding.el
        (("s" . "flyspell-mode") . flyspell-mode)                               ;; => lch-binding.el
        (("t" . "twittering-mode") . twittering-mode)                           ;; => lch-binding.el
        (("w" . "whitespace-mode") . whitespace-mode)                           ;; => lch-binding.el
        (("y" . "yas-minor-mode") . yas-minor-mode)                             ;; => lch-binding.el
        ))

(defun one-key-menu-mode ()
  "The `one-key' menu for MODE."
  (interactive)
  (one-key-menu "MODE" one-key-menu-mode-alist t))
(define-key global-map (kbd "<f2> m") 'one-key-menu-mode)

;;; F3: (local-map)
;; evoke local applications
(lazy-set-key
 '(
   ("<f3> r" . find-file-root)
   ))
;; One-key-menu-local
(defvar one-key-menu-local-alist nil
  "The `one-key' menu alist for LOCAL.")

(setq one-key-menu-local-alist
      '(
        (("<f3>" . "lch-start-file-browser") . lch-start-file-browser)          ;; => lch-util.el
        (("r" . "find-file-root") . find-file-root)                             ;; => lch-binding.el
        ))

(defun one-key-menu-local ()
  "The `one-key' menu for LOCAL."
  (interactive)
  (one-key-menu "LOCAL" one-key-menu-local-alist t))
(define-key global-map (kbd "<f3> m") 'one-key-menu-local)
;;; F4: (edit-map)
(lazy-set-key
 '(
   ("<f4> 3" . insert-line-number+)                                               ;自动在行首添加行号
   ("<f4> 4" . strip-line-number)                                                 ;删除选中区域的行号
   ("<f4> d" . insert-changelog-date)                                             ;插入日志时间 (%Y/%m/%d)
   ("<f4> D" . insert-standard-date)                                              ;插入标准时间 (%Y-%m-%d %T)
   ("<f4> e" . erase-buffer)
   ("<f4> f" . fill-region)
   ("<f4> I" . indent-region)
   ("<f4> i" . indent-buffer)                                                     ;自动格式化当前Buffer
   ("<f4> l" . strip-blank-lines)                                                 ;删除选中区域的所有空行
   ("<f4> r" . rename-file-and-buffer)
   ("<f4> R" . refresh-file)
   ("<f4> w" . delete-trailing-whitespace)                                        ;删除行末空格
   ("<f4> W" . whitespace-cleanup)                                                ;清理空格
   ))
;; One-key-menu-edit
(defvar one-key-menu-edit-alist nil "")
(setq one-key-menu-edit-alist
      '(
        (("3" . "insert-line-number+") . insert-line-number+)                     ;; => lch-binding.el
        (("4" . "strip-line-number") . strip-line-number)                         ;; => lch-binding.el
        (("d" . "insert-changelog-date") . insert-changelog-date)                 ;; => lch-binding.el
        (("D" . "insert-standard-date") . insert-standard-date)                   ;; => lch-binding.el
        (("e" . "erase-buffer") . erase-buffer)                                   ;; => lch-binding.el
        (("f" . "fill-region") . fill-region)                                     ;; => lch-binding.el
        (("i" . "indent-region") . indent-region)                                 ;; => lch-binding.el
        (("I" . "indent-buffer") . indent-buffer)                                 ;; => lch-binding.el
        (("l" . "strip-blank-lines") . strip-blank-lines)                         ;; => lch-binding.el
        (("r" . "rename-file-n-buffer") . rename-file-and-buffer)                 ;; => lch-binding.el
        (("R" . "refresh-file") . refresh-file)                                   ;; => lch-binding.el
        (("w" . "delete-trailing-whitespace") . delete-trailing-whitespace)       ;; => lch-binding.el
        (("W" . "whitespace-cleanup") . whitespace-cleanup)                       ;; => lch-binding.el
        ))

(defun one-key-menu-edit ()
  "The `one-key' menu for EDIT."
  (interactive)
  (one-key-menu "EDIT" one-key-menu-edit-alist t))
(define-key global-map (kbd "<f4> m") 'one-key-menu-edit)

;;; F5: (web-map)
;; One-key-menu-web
(defvar one-key-menu-web-alist nil
  "The `one-key' menu alist for WEB.")

(setq one-key-menu-web-alist
      '(
        (("," . "bm-previous") . bm-previous)                                   ;; => lch-elisp.el
        (("." . "bm-next") . bm-next)                                           ;; => lch-elisp.el
        (("/" . "bm-toggle") . bm-toggle)                                       ;; => lch-elisp.el
        (("<f5>" . "Switch-to-w3m") . lch-switch-to-w3m-goto-url)               ;; => lch-web.el
        (("a" . "bookmark-set (a:add)") . bookmark-set)                         ;; => lch-bmk.el
        (("b" . "list-bookmarks") . list-bookmarks)                             ;; => lch-bmk.el
        (("e" . "start-irc") . start-irc)                                       ;; => lch-web.el
        (("j" . "switch-to-bookmark (j:jump)") . switch-to-bookmark)            ;; => lch-bmk.el
        (("s" . "w3m-search-menu") . one-key-menu-w3m-search)                   ;; => lch-web.el
        ))

(defun one-key-menu-web ()
  "The `one-key' menu for WEB."
  (interactive)
  (one-key-menu "WEB" one-key-menu-web-alist t))
(define-key global-map (kbd "<f5> m") 'one-key-menu-web)

;;; F6: (erc-map)
(define-key global-map (kbd "<f6> c") (lambda () (interactive) (switch-to-buffer "##c")))
(define-key global-map (kbd "<f6> e") (lambda () (interactive) (switch-to-buffer "#emacs")))
(define-key global-map (kbd "<f6> p") (lambda () (interactive) (switch-to-buffer "#perl")))
(define-key global-map (kbd "<f6> P") (lambda () (interactive) (switch-to-buffer "#python")))
(define-key global-map (kbd "<f6> r") (lambda () (interactive) (switch-to-buffer "#ruby")))

;; One-key-menu-erc
(defvar one-key-menu-erc-alist nil
  "The `one-key' menu alist for ERC.")

(setq one-key-menu-erc-alist
      '(
        (("c" . "C/C++") . (lambda () (interactive) (switch-to-buffer "##c")))
        (("e" . "Emacs") . (lambda () (interactive) (switch-to-buffer "#emacs")))
        (("p" . "Perl") . (lambda () (interactive) (switch-to-buffer "#perl")))
        (("P" . "Python") . (lambda () (interactive) (switch-to-buffer "#python")))
        (("r" . "Ruby") . (lambda () (interactive) (switch-to-buffer "#ruby")))
        ))

(defun one-key-menu-erc ()
  "The `one-key' menu for ERC."
  (interactive)
  (one-key-menu "ERC" one-key-menu-erc-alist t))
(define-key global-map (kbd "<f6> m") 'one-key-menu-erc)

;;; F7: (skeleton-map)
(defvar one-key-menu-skeleton-alist nil
  "`One-Key' menu list for SKELETON.")

(setq one-key-menu-skeleton-alist
      '(
        (("c" . "org-center-skeleton") . org-center-skeleton)
        (("e" . "org-example-skeleton") . org-example-skeleton)
        (("f" . "Org File") . org-file-skeleton)
        (("h" . "org-head-skeleton") . org-head-skeleton)
        (("H" . "Org Html") . org-html-skeleton)
        (("i" . "org-image-skeleton") . org-image-skeleton)
        (("l" . "org-src-lisp-skeleton") . org-src-lisp-skeleton)
        ))

(defun one-key-menu-skeleton ()
  "`One-Key' menu for SKELETON."
  (interactive)
  (one-key-menu "skeleton" one-key-menu-skeleton-alist t))
(define-key global-map (kbd "<f7> m") 'one-key-menu-skeleton)

;;; F8: (org-agenda-map)
;; (define-key global-map (kbd "<f8>") 'org-agenda)                             ;; => lch-org.el
;;; F9: (file-map)
(define-key global-map (kbd "<f9> 1") (lambda() (interactive) (dired org-source-dir)))
;; One-key-menu-file
(defvar one-key-menu-file-alist nil
  "The `one-key' menu alist for FILE.")

(setq one-key-menu-file-alist
      '(
        (("1" . "org-file") . (lambda() (interactive) (dired org-source-dir)))
        ))

(defun one-key-menu-file ()
  "The `one-key' menu for FILE."
  (interactive)
  (one-key-menu "FILE" one-key-menu-file-alist t))
(define-key global-map (kbd "<f9> m") 'one-key-menu-file)

;;; F10 (conf-file-map)
(define-key global-map (kbd "<f10> 1") (lambda() (interactive) (dired (concat emacs-dir "/rc"))))
(define-key global-map (kbd "<f10> 2") (lambda() (interactive) (dired dropbox-path)))
(define-key global-map (kbd "<f10> 9") (lambda() (interactive) (find-file "/sudo::/Users/LooChao")))
(define-key global-map (kbd "<f10> 0") (lambda() (interactive) (find-file "/ssh:chaol@nobel.princeton.edu:/u/chaol")))
(define-key global-map (kbd "<f10> b") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-binding.el"))))
(define-key global-map (kbd "<f10> e") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-elisp.el"))))
(define-key global-map (kbd "<f10> u") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-util.el"))))

;; One-key-menu-conf
(defvar one-key-menu-conf-alist nil
  "The `one-key' menu alist for CONF.")

(setq one-key-menu-conf-alist
      '(
        (("1" . "emacs-conf") . (lambda() (interactive) (dired (concat emacs-dir "/rc"))))
        (("2" . "dropbox") . (lambda() (interactive) (dired dropbox-path)))
        (("8" . "remote-chili") . (lambda() (interactive) (dired "/scpc:chaol@chili.princeton.edu:/home/chaol/Research")))
        (("9" . "remote-nobel") . (lambda() (interactive) (dired "/scpc:chaol@nobel.princeton.edu:/u/chaol")))
        (("0" . "local-sudo") . (lambda() (interactive) (find-file "/sudo::/Users/LooChao")))
        ))

(defun one-key-menu-conf ()
  "The `one-key' menu for CONF."
  (interactive)
  (one-key-menu "CONF" one-key-menu-conf-alist t))
(define-key global-map (kbd "<f10> m") 'one-key-menu-conf)

;;; F11 (ui-map)
(lazy-set-key
 '(
   ("S-<f11>" . tabbar-mode)
   ("C-S-<f11>" . tool-bar-mode)
   ("<f11> c" . scroll-bar-mode)
   ("<f11> h" . hl-line-mode)
   ("<f11> l" . setnu-mode)
   ("<f11> n" . narrow-to-region)
   ("<f11> M" . menu-bar-mode)
   ("<f11> r" . ruler-mode)
   ("<f11> t" . tool-bar-mode)
   ("<f11> T" . tabbar-mode)
   ("<f11> w" . widen)
   ("<f11> W" . whitespace-mode)
   ))
;; One-key-menu-ui
(defvar one-key-menu-ui-alist nil "")
(setq one-key-menu-ui-alist
      '(
        (("=" . "modeline-style") . lch-modeline)                               ;; => lch-ui.el
        (("1" . "lch-cycle-fg-forward") . lch-cycle-fg-forward)                 ;; => lch-ui.el
        (("2" . "lch-cycle-fg-backward") . lch-cycle-fg-backward)               ;; => lch-ui.el
        (("3" . "lch-cycle-bg-forward") . lch-cycle-bg-forward)                 ;; => lch-ui.el
        (("4" . "lch-cycle-bg-backward") . lch-cycle-bg-backward)               ;; => lch-ui.el
        (("5" . "lch-cycle-font-forward") . cycle-font-forward)                 ;; => lch-ui.el
        (("6" . "lch-cycle-font-backward") . cycle-font-backward)               ;; => lch-ui.el
        (("c" . "Scroll-Bar") . scroll-bar-mode)                                ;; => lch-binding.el
        (("h" . "hl-line-mode") . hl-line-mode)                                 ;; => lch-binding.el
        (("l" . "setnu-mode") . setnu-mode)                                     ;; => lch-binding.el
        (("n" . "narrow-to-region") . narrow-to-region)                         ;; => lch-binding.el
        (("M" . "menu-bar-region") . men-bar-region)                            ;; => lch-binding.el
        (("r" . "ruler-mode") . ruler-mode)                                     ;; => lch-binding.el
        (("t" . "tool-bar-mode") . tool-bar-mode)                               ;; => lch-binding.el
        (("T" . "tabbar-mode") . tabbar-mode)                                   ;; => lch-binding.el
        (("w" . "widen") . widen)                                               ;; => lch-binding.el
        (("W" . "whitespace-mode") . whitespace-mode)                           ;; => lch-binding.el
        (("L" . "toggle-line-spacing") . toggle-line-spacing)                   ;; => lch-ui.el
        ))

(defun one-key-menu-ui ()
  "The `one-key' menu for UI."
  (interactive)
  (one-key-menu "UI" one-key-menu-ui-alist t))
(define-key global-map (kbd "<f11> m") 'one-key-menu-ui)

;;; F12 (emms-map)
;; One-key-menu-emms
(defvar one-key-menu-emms-alist nil
  "The `one-key' menu alist for EMMS.")

(setq one-key-menu-emms-alist
      '(
        (("<f12>" . "emms-init") . lch-emms-init)
        (("<f10>" . "emms-add-dir") . lch-emms-add-dir)
        (("SPC" . "toggle-playing") . lch-emms-toggle-playing)
        (("," . "emms-previous") . emms-previous)
        (("." . "emms-next") . emms-next)
        (("c" . "emms-start") . emms-start)
        (("x" . "emms-stop") . emms-stop)
        (("n" . "emms-next") . emms-next)
        (("p" . "emms-previous") . emms-previous)
        (("/" . "emms-show") . emms-show)
        (("s" . "emms-shuffle") . emms-shuffle)
        (("r" . "repeat-one") . emms-toggle-repeat-track)
        (("R" . "repeat-playlist") . emms-toggle-repeat-playlist)
        ))

(defun one-key-menu-emms ()
  "The `one-key' menu for EMMS."
  (interactive)
  (one-key-menu "EMMS" one-key-menu-emms-alist t))
(define-key global-map (kbd "<f12> m") 'one-key-menu-emms)

;;; Vi-mode-key
(defvar vi-move-key-alist nil
  "The key alist that like vi move.")
(setq vi-move-key-alist
      '(("j" . next-line)               ;上一行
        ("k" . previous-line)           ;下一行
        ("h" . backward-char)           ;向后移动
        ("l" . forward-char)            ;向前移动
        ("e" . scroll-down)             ;向下滚动一屏
        ("SPC" . scroll-up)))           ;向上滚动一屏

;;; Provide.
(provide 'lch-binding)
(message "~~ lch-binding: done.")

;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; eval: (add-hook 'before-save-hook 'time-stamp)
;; time-stamp-start: "Version: "
;; time-stamp-format: "%:y-%02m-%02d for GNU Emacs 23.1.90 (x86_64-pc-linux-gnu)"
;; time-stamp-end: "$"
;; time-stamp-line-limit: 15
;; End:
































