;;; Tabbar
(require 'tabbar)
(require 'tabbar-extension)
(tabbar-mode 1)
;; (setq tabbar-cycling-scope (quote tabs))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name)
             '("*scratch*" "*Messages*")) "Common")
    ((member (buffer-name)
             '("gtd.org" "home.org" "other.org" "study.org" "work.org")) "GTD")
    ((eq major-mode 'dired-mode) "Dired")
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode)) "Help")
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail")
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))


;; NOT working under win32
(lazy-set-key
 '(
   ("M-j" . tabbar-backward-tab)              ;移动到后一个标签
   ("M-k" . tabbar-forward-tab)               ;移动到前一个标签
   ("M-8" . tabbar-backward-group)            ;移动到后一个标签组
   ("M-9" . tabbar-forward-group)             ;移动到前一个标签组
   ;; ("M-J" . tabbar-select-beg-tab)          ;移动到最左边的标签
   ;; ("M-K" . tabbar-select-end-tab)          ;移动到最右边的标签
   ))

(provide 'lch-tabbar)