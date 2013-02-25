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
(require 'one-key)

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
        (("s" . "lch-dict-search") . lch-dict-search)                           ;; => lch-dict.el
        ))

(defun one-key-menu-meta ()
  "The `one-key' menu for META."
  (interactive)
  (one-key-menu "META" one-key-menu-meta-alist t))
(define-key global-map (kbd "M-m") 'one-key-menu-meta)

;;; Ctrl (command-map)
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
        (("2" . "set-mark-command") . set-mark-command)                         ;; => lch-binding.el
        (("6" . "lch-dired-jump") . lch-dired-jump)                             ;; => lch-elisp.el
        ))

(defun one-key-menu-ctrl ()
  "The `one-key' menu for CTRL."
  (interactive)
  (one-key-menu "CTRL" one-key-menu-ctrl-alist t))
(define-key global-map (kbd "C-z m") 'one-key-menu-ctrl)

;;; C-x (command-map)
;; One-key-menu-ctrl-x
(defvar one-key-menu-ctrl-x-alist nil "")
(setq one-key-menu-ctrl-x-alist
      '(
        (("f" . "ffap") . ffap)                                                 ;; => lch-elisp.el
        (("C-r" . "recentf-open-files") . recentf-open-files)                   ;; => lch-elisp.el
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
(define-key global-map (kbd "C-c u") 'uncomment-region)

;; One-key-menu-ctrl-c
(defvar one-key-menu-ctrl-c-alist nil "")
(setq one-key-menu-ctrl-c-alist
      '(
        (("." . "repeat-complex-command") . repeat-complex-command)             ;; => lch-binding.el
        (("a" . "lch-toggle-archive") . lch-toggle-archive)                     ;; => lch-util.el
        (("c" . "comment-region") . comment-region)                             ;; => lch-binding.el
        (("e" . "eval-buffer") . lch-eval-buffer)                               ;; => lch-binding.el
        (("g" . "grep-find") . grep-find)                                       ;; => lch-binding.el
        (("n" . "nuke-buffer") . nuke-some-buffers)                             ;; => lch-util.el
        (("s" . "switch-to-scratch") . lch-create-switch-scratch)               ;; => lch-util.el        
        (("u" . "uncomment-region") . uncomment-region)                         ;; => lch-binding.el
        (("v" . "view-mode") . view-mode)                                       ;; => lch-elisp.el
        ))

(defun one-key-menu-ctrl-c ()
  "The `one-key' menu for CTRL-C."
  (interactive)
  (one-key-menu "CTRL-C" one-key-menu-ctrl-c-alist t))
(define-key global-map (kbd "C-c m") 'one-key-menu-ctrl-c)

;;; C-z (command-map)
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
(define-key global-map (kbd "<f1> m") 'one-key-menu-fn)

;;; F1: (command-map)
(define-key global-map (kbd "<f1> .") 'repeat-complex-command)
(define-key global-map (kbd "<f1> C") 'list-colors-display)
(define-key global-map (kbd "<f1> e") 'erase-buffer)
(define-key global-map (kbd "<f1> f") 'fill-region)
(define-key global-map (kbd "<f1> p") 'list-packages)

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
        (("s" . "lch-search-w3m-google") . lch-search-w3m-google)               ;; => lch-web.el
        (("w" . "ywb-favorite-window-config") . ywb-favorite-window-config)     ;; => lch-util.el
        ))

(defun one-key-menu-command ()
  "The `one-key' menu for COMMAND."
  (interactive)
  (one-key-menu "COMMAND" one-key-menu-command-alist t))
(define-key global-map (kbd "M-<f1>") 'one-key-menu-command)

;;; F2: (mode-map)
(define-key global-map (kbd "<f2> c") 'calendar)
(define-key global-map (kbd "<f2> d") 'dired)
(define-key global-map (kbd "<f2> f") 'auto-fill-mode)
(define-key global-map (kbd "<f2> l") 'lisp-mode)
(define-key global-map (kbd "<f2> o") 'org-mode)
(define-key global-map (kbd "<f2> O") 'outline-minor-mode)
(define-key global-map (kbd "<f2> s") 'flyspell-mode)
(define-key global-map (kbd "<f2> t") 'twittering-mode)
(define-key global-map (kbd "<f2> w") 'whitespace-mode)
(define-key global-map (kbd "<f2> y") 'yas-minor-mode)

;; One-key-menu-mode
(defvar one-key-menu-mode-alist nil "")
(setq one-key-menu-mode-alist
      '(
        (("c" . "calendar") . calendar)                                         ;; => lch-binding.el
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
(define-key global-map (kbd "M-<f2>") 'one-key-menu-mode)

;;; F5: (bookmark-map)
;; One-key-menu-bookmark
(defvar one-key-menu-bookmark-alist nil
  "The `one-key' menu alist for BOOKMARK.")

(setq one-key-menu-bookmark-alist
      '(
        (("<f4>" . "bm-previous") . bm-previous)                                ;; => lch-elisp.el
        (("<f5>" . "bm-toggle") . bm-toggle)                                    ;; => lch-elisp.el
        (("<f6>" . "bm-next") . bm-next)                                        ;; => lch-elisp.el
        (("a" . "bookmark-set (a:add)") . bookmark-set)                         ;; => lch-bmk.el
        (("b" . "list-bookmarks") . list-bookmarks)                             ;; => lch-bmk.el
        (("j" . "switch-to-bookmark (j:jump)") . switch-to-bookmark)            ;; => lch-bmk.el
        ))

(defun one-key-menu-bookmark ()
  "The `one-key' menu for BOOKMARK."
  (interactive)
  (one-key-menu "BOOKMARK" one-key-menu-bookmark-alist t))
(define-key global-map (kbd "M-<f5>") 'one-key-menu-bookmark)
(define-key global-map (kbd "<f5> m") 'one-key-menu-bookmark)

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
(define-key global-map (kbd "M-<f6>") 'one-key-menu-erc)
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
(define-key global-map (kbd "M-<f7>") 'one-key-menu-skeleton)
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
(define-key global-map (kbd "M-<f9>") 'one-key-menu-file)
(define-key global-map (kbd "<f9> m") 'one-key-menu-file)

;;; F10 (conf-file-map)
(define-key global-map (kbd "<f10> 1") (lambda() (interactive) (dired (concat emacs-dir "/rc"))))
(define-key global-map (kbd "<f10> 2") (lambda() (interactive) (dired dropbox-path)))
(define-key global-map (kbd "<f10> 9") (lambda() (interactive) (find-file "/sudo::/Users/LooChao")))
(define-key global-map (kbd "<f10> 0") (lambda() (interactive) (find-file "/ssh:chaol@nobel.princeton.edu:/u/chaol")))
(define-key global-map (kbd "<f10> b") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-binding.el"))))
(define-key global-map (kbd "<f10> e") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-elisp.el"))))
(define-key global-map (kbd "<f10> u") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-util.el"))))

;; F10+v Vimperator
;; One-key-menu-conf
(defvar one-key-menu-conf-alist nil
  "The `one-key' menu alist for CONF.")

(setq one-key-menu-conf-alist
      '(
        (("1" . "emacs-conf") . (lambda() (interactive) (dired (concat emacs-dir "/rc"))))
        (("2" . "dropbox") . (lambda() (interactive) (dired dropbox-path)))            
        (("9" . "remote-nobel") . (lambda() (interactive) (find-file "/ssh:chaol@nobel.princeton.edu:/u/chaol")))
        (("0" . "local-sudo") . (lambda() (interactive) (find-file "/sudo::/Users/LooChao")))
        ))

(defun one-key-menu-conf ()
  "The `one-key' menu for CONF."
  (interactive)
  (one-key-menu "CONF" one-key-menu-conf-alist t))
(define-key global-map (kbd "M-<f10>") 'one-key-menu-conf)
(define-key global-map (kbd "<f10> m") 'one-key-menu-conf)

;;; F11 (ui-map)
(define-key global-map (kbd "S-<f11>") 'tool-bar-mode)
(define-key global-map (kbd "C-<f11>") 'setnu-mode)
(define-key global-map (kbd "C-M-<f11>") 'tabbar-mode)
(define-key global-map (kbd "C-S-<f11>") 'ruler-mode)

(define-key global-map (kbd "<f11> h") 'hl-line-mode)
(define-key global-map (kbd "<f11> l") 'setnu-mode)
(define-key global-map (kbd "<f11> n") 'narrow-to-region)
(define-key global-map (kbd "<f11> M") 'menu-bar-mode)
(define-key global-map (kbd "<f11> r") 'ruler-mode)
(define-key global-map (kbd "<f11> t") 'tool-bar-mode)
(define-key global-map (kbd "<f11> T") 'tabbar-mode)
(define-key global-map (kbd "<f11> w") 'widen)
(define-key global-map (kbd "<f11> W") 'whitespace-mode)

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
(define-key global-map (kbd "M-<f11>") 'one-key-menu-ui)

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
(define-key global-map (kbd "M-<f12>") 'one-key-menu-emms)
(define-key global-map (kbd "<f12> m") 'one-key-menu-emms)

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
































