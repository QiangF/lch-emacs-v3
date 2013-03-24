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
;;; Require
(if lch-win32-p (add-to-list 'exec-path (concat emacs-dir "/bin/w3m")))
(require 'w3m)
(require 'w3m-lnum)
(require 'w3m-search)
(require 'wget-extension)
(require 'w3m-extension)

;;; Var
;; (setq w3-default-stylesheet "~/.default.css")
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
(lazy-set-key
 '(
   ;; ("b" . (lambda() (interactive) (w3m-new-tab) (w3m-bookmark-view)))
   ("d" . w3m-delete-buffer)
   ("g" . lch-google)
   ("H" . w3m-history)
   ("M-h" . w3m-db-history)
   ;; ("t" . (lambda() (interactive) (w3m-new-tab) (lch-w3m-goto-url)))
   ("C-t" . w3m-new-tab)
   ("[" . w3m-view-previous-page)
   ("]" . w3m-view-next-page)
   ("p" . w3m-previous-buffer)
   ("n" . w3m-next-buffer)
   ("," . w3m-previous-buffer)
   ("." . w3m-next-buffer)
   ("^" . w3m-view-parent-pag)
   ("C-6" . w3m-view-parent-page)
   ("o" . lch-w3m-goto-url)
   ("O" . w3m-goto-url-new-session)
   ("s" . w3m-search)
   ("<up>" . previous-line)
   ("<down>" . next-line)
   ("<left>" . backward-char)
   ("<right>" . forward-char)
   ("<tab>" . w3m-next-anchor)
   ("}" . w3m-next-image)
   ("{" . w3m-previous-image)
   (">" . scroll-left)
   ("<" . scroll-right)
   ("\\" . w3m-view-source)
   ("=" . w3m-view-header)
   ("C-<return>" . w3m-view-this-url-new-session)
   ("1" . emms-play-online)                             ;在线听音乐
   ("2" . kill-google-define-windows)                   ;关闭Google定义窗口
   ("3" . google-define)                                ;查找输入单词的Google定义
   ("4" . google-define-pointer)                        ;查找当前光标处的Google定义
   ("5" . w3m-open-rcirc-window)                        ;打开RCIRC窗口
   ("6" . w3m-session-save)                             ;保存浏览纪录
   ("7" . w3m-session-select)                           ;加载退出前的浏览器纪录
   ("8" . w3m-emacswiki-view-other-version)             ;查看当前wiki页面的其他版本
   ("9" . w3m-auto-install-elisp)                       ;自动安装elisp文件
   ("0" . w3m-gmail-toggle-mark)                        ;切换标记选项框
   ("(" . w3m-gmail-mark-all)                           ;标记选项框
   (")" . w3m-gmail-unmark-all)                         ;取消标记选项框
   ("c" . w3m-delete-buffer-and-select-right)           ;关闭当前标签并选择右边的标签
   ("/" . w3m-next-form)                                ;下一个表格处
   ("e" . w3m-scroll-down-or-previous-url)              ;向上翻页
   ("b" . w3m-edit-current-url)                         ;编辑当前页面
   ("z" . w3m-zoom-in-image)                            ;放大图片
   ("x" . w3m-zoom-out-image)                           ;缩小图片
   ("O" . w3m-goto-linknum)                             ;数字连接快速跳转
   ("f" . w3m-view-this-url)                            ;在当前标签打开
   ("o" . w3m-view-this-url-new-session)                ;在后台标签打开
   ("M" . w3m-open-link-in-chromium)                    ;Open link in chromium browser
   ("M-o" . w3m-open-link-file-under-current-directory) ;open link file under current directory
   ("m" . tabbar-forward-tab)                           ;切换到右边的标签
   ("n" . tabbar-backward-tab)                          ;切换到左边的标签
   ("'" . w3m-open-dead-link-with-external-browser)     ;打开死的连接
   ("s-j" . w3m-visual-scroll-up)                       ;可视化向上滚动
   ("s-k" . w3m-visual-scroll-down)                     ;可视化向下滚动
   ("b" . w3m-history)                                  ;历史
   ("D" . w3m-dtree)                                    ;显示本地目录树
   ("B" . w3m-view-previous-page)                       ;后退
   ("F" . w3m-view-next-page)                           ;前进
   ("S" . w3m-google-desktop-url-open)                  ;Google桌面打开连接
   ("L" . w3m-submit-form)                              ;提交form中的内容
   ("C" . w3m-delete-other-buffers)                     ;关闭后台标签
   ("d" . w3m-download-with-wget-current-position)      ;用Wget异步下载当前地连接
   ("Y" . wget-web-page)                                ;网页下载
   ("-" . org-w3m-copy-for-org-mode)                    ;转换网页成 `org-mode' 的链接格式
   ("_" . w3m-copy-link-in-region)                      ;拷贝w3m buffer 的所有链接
   ("&" . yaoddmuse-w3m-edit-emacswiki-page)            ;编辑 emacswiki 页面
   ("*" . w3m-emacswiki-view-diff)                      ;查看当前wiki页面的不同
   ("\"" . w3m-emacswiki-recent-changes)                ;最近的修改
   ("C-u s" . w3m-db-history)                           ;历史数据库
   ("<up>" . emms-volume-mode-plus)                     ;增加音量
   ("<down>" . emms-volume-mode-minus)                  ;减少音量
   ("<left>" . emms-seek-backward)                      ;后退
   ("<right>" . emms-seek-forward)                      ;前进
   ("<" . w3m-shift-left)                               ;向左滚动屏幕一像素
   (">" . w3m-shift-right)                              ;向右滚动屏幕一像素
   ("." . go-to-char-forward-word)                      ;向后查找某一个字符, 以单词为单位前进
   ("," . go-to-char-backward-word)                     ;向前查找某一个字符, 以单词为单位后退
   ("M-s" . lazy-search-menu)                           ;懒惰搜索
   ("C-M-7" . w3m-tab-move-left)                        ;移动当前标签到左边
   ("C-M-8" . w3m-tab-move-right)                       ;移动当前标签到右边
   ("C-S-7" . w3m-delete-left-tabs)                     ;删除左边的标签
   ("C-S-8" . w3m-delete-right-tabs)                    ;删除右边的标签
   )
 w3m-mode-map
 )

(lazy-set-key
 '(
   ("C-z C-z" . w3m)                          ;启动W3M
   ("C-z z" . w3m-startup-background)         ;启动W3M, 后台
   ("C-z C-x" . toggle-w3m-with-other-buffer) ;在W3M和buffer间切换
   ("C-x C-z" . toggle-w3m-with-other-buffer) ;在W3M和buffer间切换   
   ("<f5> s" . one-key-menu-w3m-search)       ;w3m 搜索菜单
   ))

(message "~~ lch-web: done.")
(provide 'lch-web)
;;; Local Vars.
;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
 