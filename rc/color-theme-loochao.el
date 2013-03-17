;; -*- coding:utf-8 -*-

;;;###autoload
;; (set-face-background 'isearch "darkCyan")
;; (set-face-foreground 'isearch "white")
;; (set-face-background 'region "gray50")
;; (set-foreground-color "MistyRose3")
;; (set-background-color "Black")

;; (set-face-background 'ac-candidate-face "lightgray")
;; (set-face-background 'ac-selection-face "IndianRed")

(defun color-theme-loochao ()
  (interactive)
  (color-theme-install
   '(color-theme-loochao
     ((background-color . "Black")
      (background-mode . dark)
      (border-color . "Black")
      (cursor-color . "Sienna1")
      (foreground-color . "MistyRose")
      (mouse-color . "MistyRose"))
     (fringe ((t (:background "Black"))))

     (region ((t (:background "#454545"))))     
     ;; (region ((t (:background "#5b3338"))))
     (font-lock-builtin-face ((t (:foreground "#3ac70a"))))
     (font-lock-comment-delimiter-face ((t (:foreground "#999988"))))
     (font-lock-comment-face ((t (:foreground "#999988"))))     
     (font-lock-function-name-face ((t (:foreground "YellowGreen"))))     
     (font-lock-keyword-face ((t (:foreground "SlateBlue"))))     
     (font-lock-string-face ((t (:foreground "Orange"))))     
     (font-lock-type-face ((t (:foreground"#59842e"))))
     (font-lock-constant-face ((t (:foreground "AquaMarine"))))     
     (font-lock-variable-name-face ((t (:foreground "DarkSeaGreen"))))     
     (minibuffer-prompt ((t (:foreground "DarkSeaGreen" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     
     (show-paren-match-face ((t (:background "#454545" :foreground "White"))))
     (show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))

     (isearch                              ;搜索关键字
      ((t (:background "DarkSeaGreen" :foreground "white"))))
     (isearch-fail                         ;搜索失败
      ((t (:background "red4" :foreground "white"))))
     
     ;; Dired colors
     (dired-directory                      ;目录
      ((t (:inherit font-lock-function-name-face :foreground "DarkSeaGreen"))))

     (dired-ignored                        ;忽略文件
      ((t (:inherit shadow :foreground "grey50"))))
     (dired-header                         ;当前路径
      ;; ((t (:inherit font-lock-type-face :foreground "#fc751b"))))
      ((t (:inherit font-lock-type-face :foreground "Green3"))))
     (dired-symlink
      ((t (:inherit font-lock-keyword-face :foreground "SlateBlue"))))
     (diredp-date-time                     ;修改时间
      ((t (:foreground "Grey60"))))
     (diredp-deletion                      ;删除标记
      ((t (:background "Black" :foreground "red"))))
     (diredp-deletion-file-name            ;删除文件
      ((t (:foreground "red"))))
     (diredp-dir-heading                   ;目录
      ((t (:background "Black" :foreground "Gold"))))
     (diredp-dir-priv                      ;目录掩码
      ((t (:background "Black" :foreground "DodgerBlue"))))
     (diredp-display-msg                   ;路径
      ((t (:foreground "Gold"))))
     (diredp-exec-priv                     ;可执行掩码
      ((t (:background "Black" :foreground "DeepSkyBlue3"))))
     (diredp-file-name                     ;文件
      ((t (:foreground "Green3"))))
     (diredp-file-suffix                   ;文件扩展名
      ((t (:foreground "Green4"))))
     (diredp-flag-mark                     ;选中标记
      ((t (:background "Black" :foreground "Cyan"))))
     (diredp-flag-mark-line                ;选中文件
      ((t (:background "Black" :foreground "Cyan"))))
     (diredp-ignored-file-name             ;忽略的文件
      ((t (:foreground "grey40"))))
     (diredp-no-priv                       ;无掩码
      ((t (:background "Black" :foreground "Green"))))
     (diredp-other-priv                    ;其他掩码
      ((t (:background "Black" :foreground "khaki"))))
     (diredp-rare-priv                     ;稀有的掩码
      ((t (:background "Black" :foreground "Red"))))
     (diredp-read-priv                     ;读取掩码
      ((t (:background "Black" :foreground "IndianRed"))))
     (diredp-write-priv                    ;写入掩码
      ((t (:background "Black" :foreground "Gold3"))))

     ;; Tabbar
     (tabbar-default                       ;默认
      (((:inherit variable-pitch :height 0.95 :family "monaco"))))
     (tabbar-separator                     ;分隔线
      ((t (
           :inherit tabbar-default
                    :background "black"
                    :foreground "brown" :height 0.1
                    ))))
     (tabbar-button-highlight              ;按钮
      ((t (
           :inherit tabbar-default
                    :background "black"
                    :foreground "green"
                    :box (:color "red")
                    ))))
     (tabbar-button
      ((t (
           :inherit tabbar-default
                    :background "black"
                    :foreground "red"
                    :box (
                          :line-width 1
                                      :color "black"
                                      :style released-button)))))
     (tabbar-selected                      ;当前正在使用的标签
      ((t (
           :inherit tabbar-default
                    :background "black"
                    :foreground "LawnGreen"
                    :box (
                          :line-width 1
                                      :color "#014500"
                                      :style released-button)))))
     (tabbar-selected-face
      ((t (
           :inherit tabbar-default-face
                    :background "black"
                    :foreground "grey"
                    :box (
                          :line-width -1
                                      :color "grey"
                                      :style released-button)))))
     (tabbar-unselected                    ;未使用的标签
      ((t (
           :inherit tabbar-default
                    :background "black"
                    :foreground "#10650F"
                    :box (
                          :line-width 1
                                      :color "Grey10"
                                      :style pressed-button)))))
     (tabbar-unselected-face
      ((t (
           :inherit tabbar-default-face
                    :background "black"
                    :foreground "white"
                    :box (
                          :line-width -1
                                      :color "black"
                                      :style pressed-button)))))

     ;; w3m color
     (w3m-anchor                           ;未访问的标题
      ((t (:foreground "SlateBlue" :underline t))))
     (w3m-arrived-anchor                   ;已访问的标题
      ((t (:foreground "Purple4" :underline t))))
     (w3m-bold                             ;高亮光键字
      ((t (:foreground "Green3" :weight bold))))
     (w3m-current-anchor                   ;当前标题
      ((t (:box (:line-width -1 :color "Grey30") :underline t))))
     (w3m-form                             ;表格
      ((t (:foreground "Red" :box nil :underline "DarkRed"))))
     (w3m-form-button                      ;表格按钮
      ((t (:background "black" :foreground "LawnGreen"
                     :box (:line-width -1 :color "#014500" style released-button)))))
     (w3m-form-button-mouse                ;表格按钮鼠标经过
      ((((type x w32 mac) (class color))
        (:background "Black"
                     :foreground "Red"
                     :box (:line-width -1
                                       :color "Grey30"
                                       :style released-button)))))
     (w3m-form-button-pressed              ;表格按钮鼠标按下
      ((((type x w32 mac) (class color))
        (:background "Black"
                     :foreground "DarkRed"
                     :box (:line-width -1
                                       :color "Grey60"
                                       :style pressed-button)))))
     (w3m-form-face                        ;表格中字体
      ((((class color) (background dark))
        (:foreground "khaki2"
                     :underline "brown"
                     ))) t)
     (w3m-header-line-location-content     ;地址内容
      ((((class color) (background dark))
        (:background "black"
                     :foreground "Green"))))
     (w3m-header-line-location-title       ;地址标题
      ((((class color) (background dark))
        (:background "black"
                     :foreground "brown"))))
     (w3m-history-current-url              ;当前历史连接
      ((t (:background "black"
                       :foreground "DodgerBlue"))))
     (w3m-image                            ;图像
      ((((class color) (background dark))
        (:background "Black"
                     :foreground "DarkRed"))))
     (w3m-image-anchor                     ;图像锚定
      ((((class color) (background dark))
        (:background "Black"))))
     (w3m-session-select                   ;任务选择
      ((((class color) (background dark))
        (:foreground "grey50"))))
     (w3m-tab-background                   ;标签背景
      ((((type x w32 mac) (class color))
        (:background "black"
                     :foreground "black"))))
     ;; (w3m-tab-background
     ;;  ((((type x w32 mac ns) (class color))
     ;;    (:background "grey" :foreground "black"))))
     (w3m-tab-selected-background          ;标签选择背景
      ((((type x w32 mac) (class color))
        (:background "black"
                     :foreground "black"))))
     (w3m-tab-mouse                        ;鼠标点击标签
      ((((type x w32 mac) (class color))
        (:background "#454545"
                     :foreground "LawnGreen"
                     :box (:line-width -1
                                       :color "Red"
                                       :style released-button)))))
     (w3m-tab-selected                     ;选择的浏览过的标签
      ((((type x w32 mac) (class color))
        (:background "black"
                     :foreground "LawnGreen"
                     :box (:line-width -1
                                       :color "#014500"
                                       :style released-button)))))
     (w3m-tab-selected-retrieving          ;选择的死掉的标签
      ((((type x w32 mac) (class color))
        (:background "black"
                     :foreground "grey80"
                     :box (:line-width -1
                                       :color "Grey40"
                                       :style released-button)))))
     (w3m-tab-unselected                   ;未选择已浏览的标签
      ((((type x w32 mac) (class color))
        (:background "black"
                     :foreground "#10650F"
                     :box (:line-width 1
                                       :color "Black"
                                       :style pressed-button)))))
     (w3m-tab-unselected-retrieving        ;未选择的死掉的标签
      ((((type x w32 mac) (class color))
        (:background "black"
                     :foreground "grey30"
                     :box (:line-width 1
                                       :color "Black"
                                       :style pressed-button)))))
     (w3m-tab-unselected-unseen            ;未选择的没有浏览过的标签
      ((((type x w32 mac) (class color))
        (:background "black"
                     :foreground "SlateBlue"
                     :box (:line-width 1
                                       :color "black"
                                       :style pressed-button)))))
     (w3m-link-numbering                   ;数字连接
      ((((class color) (background dark))
        (:background "Black" :foreground "Grey"))))
     
     ;; Org-mode
     (org-todo                             ;TODO
      ((t (:foreground "Red" :weight bold))))
     (org-date                             ;日期
      ((((class color) (background dark))
        (
         :foreground "#454545"
                     :underline t))))
     (org-special-keyword                  ;关键字
      ((((class color) (min-colors 16) (background dark))
        (:foreground "rosybrown1"))))
     (org-level-3                          
      ((t (
           :inherit outline-3
                    :foreground "#5b3338"))))
     (org-level-5                          
      ((t (
           :inherit outline-5
                    :foreground "VioletRed3"))))
     (org-level-6             
      ((t (:inherit outline-6 :foreground "violet"))))
     (org-level-7                       
      ((t (
           :inherit outline-7
                    :foreground "khaki3"))))
     (org-level-8                       
      ((t (
           :inherit outline-8
                    :foreground "DarkSeaGreen"))))
     (org-hide                             ;隐藏星号
      ((((background dark))
        (:foreground "black"))))
     (org-ellipsis                         ;省略号
      ((((class color) (background dark))
        (
         :background "black"
                     :foreground "Cyan"
                     :strike-through nil
                     ))))
     (org-link                             ;链接
      ((((class color) (background dark))
        (:foreground "Cyan"))))

     ;; mode-line
     ;; (mode-line ((t (:foreground "#eeeeec" :background "DarkRed"))))
     ;; 正在使用的modeline
     (mode-line
      ((t (:foreground "White"
                       :background "DarkRed"
                       :box (:line-width -1 :style released-button)))))
     
     (mode-line-inactive                   
      ((default (:inherit mode-line))
       (((class color) (min-colors 88) (background dark))
        (:background "#454545" :foreground "white" :weight light
                     :box (:line-width -1 :color "#013500" :style released-button)))))
     
     (mode-line-highlight              
       ((((class color) (min-colors 88))
         (:box (:line-width 1 :color "Green4" :style released-button)))))

     ;; Comint
     ;; input of the command line
     (comint-highlight-input               
      ((t (:background "black" :foreground "khaki3" :weight bold))))
     ;; 命令行提示
     (comint-highlight-prompt ((t ((:foreground "Green")))))
     
     ;; Info
     (info-menu-header                  ; 菜单标题
      ((t (:inherit variable-pitch :foreground "khaki3" :weight bold))))
     (info-title-1                         
      ((t (:inherit info-title-2 :foreground "Gold" :height 1.1))))
     (info-title-2                         
      ((t (:inherit info-title-3 :foreground "red" :height 1.1))))
     (info-title-3                         
      ((t (:inherit info-title-4 :foreground "DodgerBlue" :height 1.1))))
     (info-title-4
      ((t (:inherit variable-pitch :foreground "Green" :weight bold))))
     (info-elisp-command-ref-item       ; elisp命令引用项目
      ((t (:background "Black"
                       :foreground "yellow3"))))
     (info-elisp-function-ref-item      ; elisp函数引用项目
      ((t (:background "Black"
                       :foreground "Gold3"))))
     (info-elisp-macro-ref-item         ; elisp宏引用项目
      ((t (:background "Black"
                       :foreground "Yellow3"))))
     (info-elisp-reference-item         ; elisp引用项目
      ((t (:background "Black"
                       :foreground "DarkRed"))))
     (info-elisp-special-form-ref-item  ; elisp特殊表格引用项目
      ((t (:background "Black"
                       :foreground "OrangeRed2"))))
     (info-elisp-syntax-class-item      ; elisp语法类型项目
      ((t (:background "Black"
                       :foreground "Khaki3"))))
     (info-elisp-user-option-ref-item   ; elisp用户选项引用项目
      ((t (:background "Black"
                       :foreground "LawnGreen"))))
     (info-elisp-variable-ref-item      ; elisp变量引用项目
      ((t (:background "Black"
                       :foreground "#0048FF"))))
     (info-file                         ; 文件
      ((t (:background "Black"
                       :foreground "Blue"))))
     (info-menu                         ; 菜单
      ((t (:foreground "DarkRed"))))
     (info-quoted-name                  ; 引用名字
      ((t (:foreground "Purple"))))
     (info-string                       ; 字符串
       ((t (:foreground "Grey50"))))

     ;; hl-line+
     (hl-line                           ; 当前行高亮背景色
      ((t (:background "grey5"))))
     ;; col-highlight
     (col-highligh                      ; 当前列的高亮背景色
      ((t (:background "Grey5"))))
     ;; hl-sexp
     (hl-sexp-face                      ; 高亮 sexp
      ((((class color) (background dark))
        (:background "gray2"))))
     
     ;; Emms Playlist
     (emms-playlist-selected-face       ; 设定选中项目文字的颜色
      ((t (:foreground "Green"))))
     (emms-playlist-track-face          ; 设定播放列表文字的底色
      ((t (:foreground "DarkGreen"))))
     ;; Emms Browser
     (emms-browser-album-face           ; 专辑
      ((((class color) (background dark))
        (:foreground "Green3" :height 1.1))))
     (emms-browser-artist-face          ; 艺术家
      ((((class color) (background dark))
        (:foreground "Gold3" :height 1.3))))
     (emms-browser-track-face           ; 歌曲
      ((((class color) (background dark))
        (:foreground "khaki3" :height 1.0))))
     
     ;; Auto-complete
     (ac-candidate-face ((t (:background "#454545" :foreground "MistyRose"))))
     ;; (ac-selection-face ((t (:background "#5b3338" :foreground "White"))))
     (ac-candidate-mouse-face ((t (:background "gray" :foreground "Black"))))
     (ac-menu-face                      ; 菜单颜色
      ((t (:background "Grey10" :foreground "Grey40"))))
     (ac-selection-face                 ; 选择颜色
      ((t (:background "Green4" :foreground "Green"))))
     (ac-yasnippet-menu-face            ; Yasnippet 菜单颜色
      ((t (:background "Grey10" :foreground "Grey40"))))
     (ac-yasnippet-selection-face       ; Yasnippet 选择颜色
      ((t (:background "DarkRed" :foreground "Grey"))))
     
     ;; ERC
     (erc-direct-msg-face               ; 直接消息
      ((t (:foreground "DodgerBlue"))))
     (erc-input-face                    ; 输入
      ((t (:foreground "Green2"))))
     (erc-my-nick-face                  ; 我的昵称
      ((t (:foreground "DarkRed" :weight bold))))
                                        
     (erc-notice-face                   ; 注意
      ((t (:foreground "Gray20" :weight bold))))
     ;; 提示
     (erc-prompt-face ((t (:background "Black" :foreground "Gold" :weight bold))))
     
     ;; Diff
     (diff-header ((((class color) (min-colors 88) (background dark))
                    (:background "grey30" :foreground "gold"))))
     ;; Linum
     (linum ((t (:background "black" :foreground "#10650F" :height 0.8 :family "monaco"))))     

     (fringe                            ; 窗口边缘
      ((((class color) (background dark)) (:background "grey10"))))

     ;; Speedbar
     (speedbar-file-face                ; 文件
      ((((class color) (background dark))
        (:foreground "SeaGreen2"))))
     (speedbar-highlight-face           ; 高亮
      ((((class color) (background dark))
        (:background "LightGoldenrod" :foreground "black"))))
     (speedbar-selected-face            ; 选中
      ((((class color) (background dark))
        (:foreground "Cyan" :underline t))))
     (speedbar-separator-face           ; 分隔线
      ((((class color) (background dark))
        (:background "DarkRed" :foreground "white" :overline "gray"))))

     
)))
(provide 'color-theme-loochao)
