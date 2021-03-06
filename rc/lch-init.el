;; -*- coding:utf-8; -*-

;;; INIT.EL
;;
;; Copyright (c)  Chao LU 2005 2006-2011
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Initialization settings

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

;;; (info "(emacs)Customization")
(message "=> lch-init: loading...")

;;; (info "(emacs)Kill Ring")
;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode))
                  (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode))
                  (indent-region (region-beginning) (region-end) nil)))

;;; Customization
(setq enable-local-eval t
      redisplay-dont-pause t
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      modeline-click-swaps-buffers t
      blink-matching-paren-distance 32768
      tab-width 8
      read-file-name-completion-ignore-case t
      completion-ignore-case t
      undo-outer-limit 5000000          ; undo-limit 100000
      message-log-max t                 ; Don't truncate the message log buffer when it becomes large
      indicate-buffer-boundaries t      ; ?? visually indicate buffer boundaries and scrolling
      inhibit-startup-message t         ; No splash screen please ... jeez
      mark-ring-max 1024                ; # of marks kept in the mark ring.
      enable-recursive-minibuffers t    ; Allow recursive minibuffer ops.
                                        ; minibuffer 递归调用命令
      scroll-step 1                     ; Move down 1 line instead of multi.
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      next-line-add-newlines nil        ; Don't add newlines at the end.
      message-log-max 500               ; Show lots of *message*.
      which-function-mode t             ; Display which func point is in
                                        ; 在状态条上显示当前光标在哪个函数体内部
      ;; kill-whole-line t              ; Remove the newlines as well.
      x-stretch-cursor t                ; 光标在 TAB 字符上会显示为一个大方块
      message-log-max t                 ; 设置message记录全部消息, 而不用截去      
                  )

;; Show me empty lines after buffer end
;; (set-default 'indicate-empty-lines t)

(setq sentence-end "\\([。！？。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; (setq safe-local-variable-values (quote ((unibyte . t) (flyspell-mode . -1) (allout-layout * 0 :))))

(setq tab-stop-list
      (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))

;; Default major mode for new buffers and any files with unspecified mode
(when (locate-library "org.el")
  (setq-default major-mode 'org-mode))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.02)

;; Enable winner mode for C-c-(<left>|<right>) to navigate the history
;; of buffer changes i.e. undo a split screen
(when (fboundp 'winner-mode)
  (winner-mode t))

;;; Info directory
;; FIXME
;; (defvar info-dir (concat emacs-dir "/info"))
;; (require 'info)
;; (setq Info-directory-list
;;       (cons (expand-file-name info-dir)
;;             Info-directory-list))

;;; Dialog
(setq use-file-dialog t)
(setq use-dialog-box nil)
;;; Transparently open compressed files
;; 打开压缩文件时自动解压缩
(auto-compression-mode t)
;;; Transient mark
(when window-system (transient-mark-mode 1))
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;;; Set default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; (setq left-fringe-width 12)


;;; Minibuffer

;; Ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; Dim the ignored part of the file name
(file-name-shadow-mode 1)

;; Minibuffer window expands vertically as necessary to hold the text that you
;; put in the minibuffer
(setq resize-mini-windows t)

;; From Babel.el: "If the output is short enough to display in the echo area
;; (which is determined by the variables `resize-mini-windows' and
;; `max-mini-window-height'), it is shown in echo area."


;;; Mouse Jump away
(mouse-avoidance-mode 'animate)
;; (mouse-avoidance-mode 'jump)


;;; Run at full power please
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'overwrite-mode   'disabled t)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Turn on all the disabled functions
(setq disabled-command-function nil)


;;; Display page delimiter ^L as a horizontal line
;; (aset standard-display-table ?\^L (vconcat (make-vector 64 ?-) "^L"))

;;; Death to the tabs!
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq tab-width 8)

;; if indent-tabs-mode is off, untabify before saving
;; (add-hook 'write-file-hooks
;;           (lambda () (if (not indent-tabs-mode)
;;                          (untabify (point-min) (point-max)))))

;;; 'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;;; Alter the scratch message
(setq initial-scratch-message "")
;; (setq initial-scratch-message "Welcome to the world of Emacs")


;;; Don't beep at me
(setq visible-bell t)
;; No ring no screen shaking.
;;(setq ring-bell-function 'ignore)


;;; Line trancation enable
(setq truncate-partial-width-windows nil)

;;; Display column & line number
(when (fboundp 'line-number-mode)
  (line-number-mode 1))
(when (fboundp 'column-number-mode)
  (column-number-mode 1))

;;; Time stamp support
;; when there's "Time-stamp: <>" in the first 10 lines of the file
;; (setq time-stamp-active t
;;       time-stamp-warn-inactive t
;;       ;; check first 10 buffer lines for Time-stamp: <>
;;       time-stamp-line-limit 10
;;       time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
;; (add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S Lu Chao")

;;; New line
;; Interchange these two keys.
;; Under most cases, indent is needed after enter.
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-j") 'newline)

;;; Directly delete current line
;; (define-key global-map (kbd "C-k") 'kill-whole-line)

;;; Set default major mode org-mode
;; Enabled in Org-mode
;; (setq major-mode 'org-mode)

;;; Display picture
(auto-image-file-mode)

;;; Grammar highlight
;; Significant functionality depends on font-locking being active.
;; For all buffers
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; For Org buffers only
;; (add-hook 'org-mode-hook 'turn-on-font-lock)

;;; Bookmark file
;; Not only on exit, but on every modification
(setq bookmark-save-flag 1)

;;; Savehist
;; keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat emacs-var-dir "/savehist"))
(savehist-mode t)

;;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;; Start the emacs server only if another instance of the server is not running.
(require 'server)
(if (eq (server-running-p server-name) nil)
                (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; Delete the selection with a keypress
;; Remove text in active region if inserting text
(delete-selection-mode 1)

;;; Backup policies
(setq make-backup-files t
      version-control t
      kept-old-versions 2
      kept-new-versions 5
      delete-old-versions t
      backup-by-copying t
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t)

;; Backup path
(setq backup-directory-alist '(("" . "~/.emacs.var/backup")))
;; Don't make backup files
;; (setq make-backup-files nil backup-inhibited t)


;;; Diary file
;; (setq diary-file "~/.emacs.var/.diary")
;; (add-hook 'diary-hook 'appt-make-list)
;; (setq diary-mail-addr "loochao@gmail.com")

;;; Auto fill
;; Turn on auto-fill mode for all major modes
;; (setq-default auto-fill-function 'do-auto-fill)
;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)
;; Automatically turn on auto-fill-mode when editing text files
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'tex-mode-hook 'turn-on-auto-fill)

;;; Truncate lines
;; t means aaaaa->
(set-default 'truncate-lines nil)
;; Toggles between line wrapping in the current buffer.
(defun lch-toggle-line-wrapping ()
  "Toggles between line wrapping in the current buffer."
  (interactive)
  (if (eq truncate-lines nil)
      (progn
        (setq truncate-lines t)
        (redraw-display)
        (message "Setting truncate-lines to t"))
    (setq truncate-lines nil)
    (redraw-display)
    (message "Setting truncate-lines to nil")))
(define-key global-map (kbd "C-c ^") 'lch-toggle-line-wrapping)

;;; Auto compile el files
(defun elisp-compile-hook ()
  (add-hook 'after-save-hook (lambda () (byte-compile-file (buffer-file-name
                                                            (current-buffer)))) nil t))
(add-hook 'emacs-lisp-mode-hook 'elisp-compile-hook)

(defun byte-recompile-directory-all (bytecomp-directory &optional bytecomp-force)
  (interactive "DByte recompile directory: ")
  (byte-recompile-directory bytecomp-directory 0 bytecomp-force))

(defun byte-recompile-special-directory (&optional bytecomp-force)
  (interactive)
  (byte-recompile-directory "~/.emacs.d/rc" 0 bytecomp-force))


;;; Aliases
(defalias 'wku 'w3m-print-this-url)
(defalias 'wkl 'w3m-print-current-url)
(defalias 'afm 'auto-fill-mode)


;;; Time setting
;; Display format in 24hr format
(setq display-time-24hr-format t)
;; Display time date
(setq display-time-day-and-date t)
;; Time altering frequency
(setq display-time-interval 10)

;; (setq display-time-format "<%V-%u> %m/%d/%H:%M")
(setq display-time-format "%a(%V) %m-%d/%H:%M")

(display-time)

;;; Local variables
;; (info "(emacs)Variables")
;; (info "(emacs)Directory Variables")

;; file local variables specifications are obeyed, without query -- RISKY!
(setq enable-local-variables t)

;; obey `eval' variables -- RISKY!
(setq enable-local-eval t)

;; record safe values for some local variables
(setq safe-local-variable-values
      '((TeX-master . t)
        (balloon-help-mode . -1)
        (flyspell-mode . t)
        (flyspell-mode . -1)
        (ispell-local-dictionary . "en_US")
        (ispell-mode . t)
        (byte-compile . nil)
        (auto-compile-lisp . nil)
        ;; (org-export-latex-title-command . "\\maketitle[logo=Forem]")
        ))


;;; Hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;; Yet another way
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-dabbrev
;;         try-expand-whole-kill
;;         ;senator-try-expand-semantic
;;         try-expand-dabbrev-visible
;;         try-expand-dabbrev-from-kill
;;         try-expand-dabbrev-all-buffers
;;         try-expand-all-abbrevs
;;         try-complete-file-name-partially
;;         try-complete-file-name
;;         try-expand-list
;;         ;try-complete-lisp-symbol-partially
;;         ;try-complete-lisp-symbol
;;         try-expand-line
;;      try-expand-line-all-buffers))

;;; User info
(setq user-full-name "LooChao<LooChao@gmail.com>")
(setq user-mail-address "LooChao@gmail.com")


;;; Grep & find
;; (info "(emacs)Dired and Find")
;; Search for files with names matching a wild card pattern and Dired the output
(define-key global-map (kbd "C-c 1") 'find-name-dired)

;; Search for files with contents matching a wild card pattern and Dired the output
(define-key global-map (kbd "C-c 2") 'find-grep-dired)

;; Run grep via find, with user-specified arguments
(define-key global-map (kbd "C-c 3") 'grep-find)

(setq grep-find-command "find . -type f ! -regex \".*/\\({arch}\\|\\.arch-ids\\|\\.svn\\|_darcs\\|\\.bzr\\|\\.git\\|\\.hg\\)/.*\" -print0 | xargs -0 grep -nH -e ")
(define-key global-map (kbd "C-c 1") 'find-name-dired)

;; Search for files with contents matching a wild card pattern and Dired the output
(define-key global-map (kbd "C-c 2") 'find-grep-dired)

;; Run grep via find, with user-specified arguments
(define-key global-map (kbd "C-c 3") 'grep-find)

(setq grep-find-command "find . -type f ! -regex \".*/\\({arch}\\|\\.arch-ids\\|\\.svn\\|_darcs\\|\\.bzr\\|\\.git\\|\\.hg\\)/.*\" -print0 | xargs -0 grep -nH -e ")


;;; Set non-file buffer based on buffer name
(setq-default major-mode
              (lambda ()
                (if buffer-file-name
                    (fundamental-mode))
                (let ((buffer-file-name (buffer-name))))
                (set-auto-mode)))
;;; Auto save files in one place
;; Put autosave files (i.e. #foo#) in one place, *NOT*
;; scattered all over the file system!

;; auto-save every 100 input events
(setq auto-save-interval 100)

;; auto-save after 15 seconds idle time
(setq auto-save-timeout 15)

(defvar autosave-dir
  (concat "~/.emacs.var/auto-save-list/" (user-login-name) "/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

(provide 'lch-init)
(message "~~ lch-init: done.")

;;; Local Vars
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End: