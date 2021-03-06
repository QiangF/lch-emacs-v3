;;-*- coding:utf-8; mode:emacs-lisp; -*-

;;; ORG.EL
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Chao LU
;;
;; Author: Chao LU <loochao@gmail.com>
;; URL: http://www.princeton.edu/~chaol

;; This file is not part of GNU Emacs.

;;; Commentary
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
;(defvar org-dir "~/Dropbox/org" "org dir")                                     => dotEmacs
;(defvar org-source-dir "~/Dropbox/org/org" "org source dir")                   => dotEmacs
;(defvar pub-html-dir "~/Dropbox/org/public_html" "html dir")                   => dotEmacs
;(defvar org-mobile-dir (concat emacs-path "/MobileOrg") "org mobile dir")      => dotEmacs
;(defvar org-private-dir (concat org-dir "/private")  "org private dir")        => dotEmacs
;(defvar prv-html-dir (concat org-dir "/private_html") "private html dir")      => dotEmacs
;(defvar worg-dir (concat git-dir "/worg")  "worg source dir")                  => dotEmacs
;(defvar worg-html-dir (concat git-dir "/worg_html") "worg html dir")           => dotEmacs

(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(add-to-list 'auto-mode-alist '("README$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-tags-column -90)

;(define-key global-map (kbd "C-c l") 'org-store-link)                 ;; conflict with others, so disabled
;(define-key global-map (kbd "C-c a") 'org-agenda)                     ;; conflict with others, so disabled

(setq org-completion-use-ido t)
(setq org-hide-leading-stars t)        ;; Hide the first N-1 stars in a headline.
(setq org-log-done t)
(setq org-export-email-info t)
(setq org-reverse-note-order t)        ;; New notes on top.
(setq org-deadline-warning-days 14)    ;; No. of days to display before expiration of a deadline.

(setq org-directory org-private-dir)
(setq org-agenda-files
      (append
;       (file-expand-wildcards (concat org-private-dir "/*.org"))
       (file-expand-wildcards (concat org-source-dir "/*.org"))))
(setq org-export-default-language "EN")

(setq org-mobile-directory org-mobile-dir)
(setq org-mobile-force-id-on-agenda-items nil)
(setq org-mobile-inbox-for-pull (concat org-mobile-dir "/mobileOrg.org"))


;(define-key global-map (kbd "<f8>") 'org-remember)

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords '((sequence "QUEUE(q)" "ACTIVE(a)" "PENDING(p@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")
                          (sequence "TOFNSH(t)" "|" "DONE")
                          (sequence "INVOICE(i)" "SENT(n)" "|" "RCVD(r)")
                          (sequence "PROJECT(P)" "|" "DONE(d)")
                          (type "APPT(a)")
                          (type "OBTAIN(o)" "EDU_OBT(e)" "NET_OBT(n)")
                          (type "TOSORT(S)")
                          (type "DELEGATED(d@!)")
                          (type "WAITING(w@/!)")
                          (type "SCHEDULED(s@/!)")
 ))

(setq org-todo-keyword-faces '(
                       ("QUEUE" :foreground "Lavender" :weight bold)
                       ("ACTIVE" :foreground "Cyan" :weight bold)
                       ("TOFNSH" :foreground "PINK" :weight bold)
                       ("DONE" :foreground "PeachPuff2" :weight bold)
                       ;; ("WAITING" :foreground "medium blue" :weight bold)
                       ("APPT" :foreground "medium blue" :weight bold)
                       ("NOTE" :foreground "brown" :weight bold)
                       ("STARTED" :foreground "dark orange" :weight bold)
                       ;; ("TODO" :foreground "red" :weight bold)
                       ("DELEGATED" :foreground "dark violet" :weight bold)
                       ("DEFERRED" :foreground "dark blue" :weight bold)
                       ("SOMEDAY" :foreground "dark blue" :weight bold)
                       ("PROJECT" :height 1.5 :weight bold :foreground "black")
                       ))

;;; Org-tag
(setq org-tag-alist '(
                      ("#A" . ?[)
                      ("#B" . ?])
                      ("#C" . ?\\)

                      ("Audio" . ?a)
                      ("Book" . ?b)
                      ("BIB" . ?B)
                      ("Culture" . ?c)
                      ("ComputerSE" . ?C)
                      ("Doc" . ?d)
                      ;; ("Cookery" . ?)
                      ("EBook" . ?e)
                      ;; ("English" . ?E)
                      ("EDU" . ?E)
                      ("GuoXue" . ?g)
                      ("Humor" . ?h)
                      ("HDoc" . ?H)
                      ("IDEA" . ?i)
                      ("Library" . ?l)
                      ("Life" . ?L)
                      ;; ("Love" . ?)
                      ;; ("List" . ?)
                      ("Mathematics" . ?m)
                      ("OBTAIN" . ?o)
                      ("Org" . ?O)
                      ("PLAN" . ?p)
                      ("Physics" . ?P)
                      ("Question" . ?q)
                      ("Research" . ?r)

                      ("Video" . ?v)
                      ("Web" . ?w)

                      ("ACTIVE" . ?1)
                      ("MOBILE" . ?2)
                      ("AUDIO" . ?3)
                      ("CAR" . ?4)

                      (:startgroup . nil)
                      ("BIB" . ?0)
                      ("STAR3" . ?,)
                      ("STAR4" . ?.)
                      ("STAR5" . ?/)
                      (:endgroup . nil)
                      ))

(setq org-tag-faces
      '(
        ("Audio" . (:foreground "Noccasin" :weight bold))
        ("Book" . (:foreground "Siennal1" :weight bold))
        ("Doc" . (:foreground "PaleGreen" :weight bold))
        ("EBook" . (:foreground "Gold1" :weight bold))
        ("Video" . (:foreground "Violet" :weight bold))

        ("BIB" . (:foreground "DeepSkyBlue" :background "OldLace" :weight bold))
        ("STAR3" . (:foreground "Black" :background "Grey" :weight bold))
        ("STAR4" . (:foreground "Black" :background "SandyBrown" :weight bold))
        ("STAR5" . (:foreground "Black" :background "MistyRose" :weight bold))

        ("Library" . (:foreground "LightCyan" :weight bold))
        ("Mathematics" . (:foreground "Tomato" :weight bold))
        ("Physics" . (:foreground "Peru" :weight bold))
        ("Question" . (:foreground "GreenYellow" :weight bold))
        ("ProbSet" . (:foreground "IndianRed" :weight bold))
        ("DATA" . (:foreground "NavyBlue" :background "OldLace" :weight bold))
        ("IMAGE" . (:foreground "DarkViolet" :background "OldLace" :weight bold))
        ("OBTAIN" . (:foreground "Moccasin" :weight bold))
        ("STUDY" . (:foreground "Gold" :weight bold))
        ("#A" . (:foreground "OrangeRed" :weight bold))
        ("#B" . (:foreground "Pink" :weight bold))
        ("#C" . (:foreground "Light Green" :weight bold))
        ("QUEUE" . (:foreground "Lavender" :weight bold))
        ("ACTIVE" . (:foreground "Cyan" :weight bold))
        ("DONE" . (:foreground "PeachPuff2" :weight bold))

        ("AUDIO" . (:foreground "Cyan" :weight bold))
        ("MOBILE" . (:foreground "Cyan" :weight bold))
        ("CAR" . (:foreground "Cyan" :weight bold))
        ("RECUR" . (:foreground "Cyan" :weight bold))
        ("DAILY" . (:foreground "Cyan" :weight bold))
        ("DUALLY" . (:foreground "Cyan" :weight bold))
        ("TRIPLY" . (:foreground "Cyan" :weight bold))
        ("WEEKLY" . (:foreground "Cyan" :weight bold))
        ))

(setq org-fast-tag-selection-single-key t)
(setq org-export-with-LaTeX-fragments t)

(add-hook 'org-mode-hook
          (lambda()
            (auto-fill-mode t)
            (set-fill-column 80)
            (flyspell-mode 1)
            (setq truncate-lines nil)
            ))

(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
;; (add-hook 'org-mode-hook 'turn-on-iimage-mode)
(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
      (set-face-underline-p 'org-link t))
  (iimage-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (ocaml . nil)
   (python . t)
   (ruby . t)
   (screen . nil)
   (sh . t)
   (sql . nil)
   (sqlite . t)))
;; I don't want to be prompted on every code block evaluation
(setq org-confirm-babel-evaluate nil)

(setq org-refile-targets '(
                           ("iPrv.org" :level . 2)
                           ("iDea.org" :level . 2)
                           ("Emacs.org" :level . 1)))

;; (setq org-refile-targets '((nil :maxlevel . 2)
;;                                         ; all top-level headlines in the
;;                                         ; current buffer are used (first) as a
;;                                         ; refile target
;;                         (org-agenda-files :maxlevel . 2)
;;                            ))
;; (add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; YASnippet support
(defun lch-yas-in-org ()
  "Define H-y as yas/expand in org"
  (define-key org-mode-map (kbd "C-i") 'yas/expand))

(add-hook 'org-mode-hook 'lch-yas-in-org)

;;; Org Capture
;; %a          annotation, normally the link created with org-store-link
;; %i          initial content, the region when remember is called with C-u.
;; %t          timestamp, date only
;; %T          timestamp with date and time
;; %u, %U      like the above, but inactive timestamps

(define-key global-map (kbd "M-0") 'org-capture)

(setq org-capture-templates
      '(
        ("a" "TODO-#A" entry (file+olp (concat org-private-dir "/iPrv.org") "TODO-#A" "TODO-#A-") "* %? :#A:\n%U" :prepend t)
;       ("A" "TODO-#A~" entry (file+olp (concat org-private-dir "/Refile.org") "TODOs" "TODO-#A") "* %? :#A:\n%U" :prepend t)
        ("b" "TODO-#B" entry (file+olp (concat org-private-dir "/iPrv.org") "TODO-#B" "TODO-#B-") "* %? :#B:\n%U" :prepend t)
        ("B" "ACTIVE-#B" entry (file+olp (concat org-private-dir "/iPrv.org") "TODO-#B" "TODO-#B-") "* ACTIVE %? :#B:\n%U" :prepend t)
        ("8" "BIB" entry (file+olp (concat org-private-dir "/Refile.org") "BIBs" "BIB") "* %? \n%U" :prepend t)
        ("c" "TODO-#C" entry (file+olp (concat org-private-dir "/iPrv.org") "TODO-#C" "TODO-#C-") "* %? :#C:\n%U" :prepend t)
        ;; ("C" "TODO-#C~" entry (file+olp (concat org-private-dir "/Refile.org") "TODOs" "TODO-#C") "* %? :#C:\n%U" :prepend t)
        ;; ("c" "COUNT" entry (file+datetree (concat org-private-dir "/iCount.org")) "* %? \n%U" :prepend t)
        ("d" "IDEA" entry (file+olp (concat org-private-dir "/iDea.org") "IDEAs" "IDEA") "* %? \n%U" :prepend t)
        ("g" "GOOD" entry (file+olp (concat org-private-dir "/iPrv.org") "GOOD" "-GOOD-") "* %? :#B:\n%U" :prepend t)
        ("i" "INBOX" entry (file+olp (concat org-private-dir "/Refile.org") "INBOXs" "INBOX") "* %? \n%U" :prepend t)
        ("n" "NOTES" entry (file+olp (concat org-private-dir "/Refile.org") "NOTEs" "NOTE") "* %? \n%U" :prepend t)
        ("q" "QUESTION" entry (file+olp (concat org-private-dir "/iPrv.org") "QUESTIONs" "QUESTION") "* %? \n%U" :prepend t)
        ("l" "LOG" entry (file+datetree (concat org-private-dir "/iLog.org")) "* %U\n%?" :prepend t)
        ("m" "MUSIC" entry (file+olp (concat org-private-dir "/iPrv.org") "TODO-#C" "MUSICs") "* %? :#C:\n%U" :prepend t)
        ("t" "TODO" entry (file+olp (concat org-private-dir "/Refile.org") "TODOs" "TODO") "* %? \n%U" :prepend t)
        ("o" "OBTAIN" entry (file+olp (concat org-private-dir "/iPrv.org") "TODO-#B" "OBTAINs") "* %? \n%U" :prepend t)
        ("p" "PLAN" entry (file+olp (concat org-private-dir "/iLog.org") "PLANs" "PLAN") "* %u\np%?" :prepend t)
        ("q" "QUESTION" entry (file+olp (concat org-private-dir "/Refile.org") "QUESTIONs" "QUESTION") "* %? \t%U" :prepend t)
        ("v" "VERBAL" entry (file+olp (concat org-private-dir "/English.org") "Verbal" "Verbal") "* %? \n %U" :prepend t)
        ;; ("n" "note" entry (file (concat org-private-dir "/iPrv.org")) "* %? :NOTE: %U %a :CLOCK: :END:" :clock-in t :clock-resume t)
        ;; ("f" "appointment" entry (file+datetree (concat org-private-dir "/iPrv.org")) "* %? %U" :clock-in t :clock-resume t)
        ;; ("p" "Phone call" entry (file (concat org-private-dir "iPrv.org")) "* Phone %(bh/phone-call) - %(gjg/bbdb-company) :PHONE:\n%U\n\n%?" :clock-in t :clock-resume t)
        ;; ("w" "org-protocol" entry (file (concat org-private-dir "iPrv.org")) "* TODO Review %c %U" :immediate-finish t :clock-in t :clock-resume t)
        ))

;; (define-key global-map (kbd "<f7> l") (lambda () (interactive) (org-capture nil "l")))

;;; Google Weather
(require 'google-weather)
(require 'org-google-weather)

;;; Trigger Agenda
;; (defun lch-agenda-showup()
;;   (interactive)
;;   (split-window-horizontally)
;;   (org-agenda nil "1")
;; ;  (other-window 1)
;; ;  (org-agenda nil "`")
;;   )

;; (defvar rsch-dir (concat dropbox-path "/RESEARCH/Research"))
;; (defvar rsch-bib-dir (concat rsch-dir "/Bibtex"))
;; (setq reftex-default-bibliography
;;       (append
;;        (file-expand-wildcards (concat rsch-bib-dir "/*.bib")
;;                            (concat rsch-dir "Research.bib"))))

;;; Org-Util
;; (defun lch-org-property ()
;;   (interactive)
;;   (let ((ext (file-name-extension (buffer-file-name)))
;;     (buf (buffer-name)))
;;     )
;;   (replace-string)
;;   (buffer-name)
;;   (insert ":PROPERTIES:\n")
;;   (insert ":CATEGORY: \n")
;;   (insert ":END:\n")
;;   )

(provide 'lch-org)
;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
