;;; calfw.el --- Calendar view framework on Emacs

;; Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program is a framework for the Calendar component. In the
;; Emacs, uses can show schedules in the calendar views, like iCal,
;; Outlook and Google Calendar.

;;; Installation:

;; Place this program in your load path and add following code.

;; (require 'calfw)

;;; Usage:

;; Executing the command `cfw:open-calendar-buffer', switch to the calendar buffer.
;; You can navigate the date like calendar.el.

;; Schedule data which are shown in the calendar view, are collected
;; by the `cfw:source' objects. See the function `cfw:open-debug-calendar' for example.

;; This program gets the holidays using the function
;; `calendar-holiday-list'. See the document of the holidays.el and
;; the Info text for customizing the holidays.

;;; Add-ons:

;; Following programs are also useful:
;; - calfw-howm.el : Display howm schedules.
;; - calfw-ical.el : Display schedules of the iCalendar format.
;; - calfw-org.el  : Display orgmode schedules.

;;; Code:

(eval-when-compile (require 'cl))
(require 'calendar)
(require 'holidays)



;;; Constants

(defconst cfw:week-sunday    0)
(defconst cfw:week-monday    1)
(defconst cfw:week-tuesday   2)
(defconst cfw:week-wednesday 3)
(defconst cfw:week-thursday  4)
(defconst cfw:week-friday    5)
(defconst cfw:week-saturday  6)
(defconst cfw:week-days      7)

;;; Customs

(defcustom cfw:fchar-vertical-line ?|
  "The character used for drawing vertical lines."
  :group 'cfw
  :type 'character)

(defcustom cfw:fchar-horizontal-line ?-
  "The character used for drawing horizontal lines."
  :group 'cfw
  :type 'character)

(defcustom cfw:fchar-junction ?+
  "The character used for drawing junction lines."
  :group 'cfw
  :type 'character)

(defcustom cfw:fchar-top-right-corner ?+
  "The character used for drawing the top-right corner."
  :group 'cfw
  :type 'character)

(defcustom cfw:fchar-top-left-corner ?+
  "The character used for drawing the top-left corner."
  :group 'cfw
  :type 'character)

(defcustom cfw:fchar-left-junction ?+
  "The character used for drawing junction lines at the left side."
  :group 'cfw
  :type 'character)

(defcustom cfw:fchar-right-junction ?+
  "The character used for drawing junction lines at the right side."
  :group 'cfw
  :type 'character)

(defcustom cfw:fchar-top-junction ?+
  "The character used for drawing junction lines at the top side."
  :group 'cfw
  :type 'character)

;;; Faces

(defface cfw:face-title
  '((((class color) (background light))
     :foreground "DarkGrey" :weight bold :height 2.0 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "darkgoldenrod3" :weight bold :height 2.0 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for title" :group 'calfw)

(defface cfw:face-header
  '((((class color) (background light))
     :foreground "Slategray4" :background "Gray90" :weight bold)
    (((class color) (background dark))
     :foreground "maroon2" :weight bold))
  "Face for headers" :group 'calfw)

(defface cfw:face-sunday
  '((((class color) (background light))
     :foreground "red2" :background "#ffd5e5" :weight bold)
    (((class color) (background dark))
     :foreground "red" :weight bold))
  "Face for Sunday" :group 'calfw)

(defface cfw:face-saturday
  '((((class color) (background light))
     :foreground "Blue" :background "#d4e5ff" :weight bold)
    (((class color) (background light))
     :foreground "Blue" :weight bold))
  "Face for Saturday" :group 'calfw)

(defface cfw:face-holiday
  '((((class color) (background light))
     :background "#ffd5e5")
    (((class color) (background dark))
     :background "grey10" :foreground "purple" :weight bold))
  "Face for holidays" :group 'calfw)

(defface cfw:face-grid
  '((((class color) (background light))
     :foreground "SlateBlue")
    (((class color) (background dark))
     :foreground "DarkGrey"))
  "Face for grids"
  :group 'calfw)

(defface cfw:face-default-content
  '((((class color) (background light))
     :foreground "#2952a3")
    (((class color) (background dark))
     :foreground "green2"))
  "Face for default contents"
  :group 'calfw)

(defface cfw:face-periods
  '((((class color) (background light))
     :background "#668cd9" :foreground "White" :slant italic)
    (((class color) (background dark))
     :foreground "cyan"))
  "Face for period" :group 'calfw)

(defface cfw:face-day-title
  '((((class color) (background light))
     :background "#f8f9ff")
    (((class color) (background dark))
     :background "grey10"))
  "Face for day title"
  :group 'calfw)

(defface cfw:face-default-day
  '((((class color) (background light))
     :weight bold :inherit cfw:face-day-title)
    (((class color) (background dark))
     :weight bold :inherit cfw:face-day-title))
  "Face for default day" :group 'calfw)

(defface cfw:face-annotation
  '((((class color)) :foreground "RosyBrown" :inherit cfw:face-day-title))
  "Face for annotations"
  :group 'calfw)

(defface cfw:face-disable
  '((((class color)) :foreground "DarkGray" :inherit cfw:face-day-title))
  "Face for days out of focused period"
  :group 'calfw)

(defface cfw:face-today-title
  '((((class color) (background light))
     :background "#fad163")
    (((class color) (background dark))
     :background "red4" :weight bold))
  "Face for today" :group 'calfw)

(defface cfw:face-today
  '((((class color) (background light))
     :background "#fff7d7")
    (((class color) (background dark))
     :foreground "Cyan" :weight bold))
  "Face for today" :group 'calfw)

(defface cfw:face-select
  '((((class color) (background light))
     :background "#c3c9f8")
    (((class color) (background dark))
     :background "Blue4"))
  "Face for selection" :group 'calfw)



;;; Utilities

(defun cfw:k (key alist)
  "[internal] Get a content by key from the given alist."
  (cdr (assq key alist)))

(defun cfw:rt (text face)
  "[internal] Put a face to the given text."
  (unless (stringp text) (setq text (format "%s" (or text ""))))
  (put-text-property 0 (length text) 'face face text)
  (put-text-property 0 (length text) 'font-lock-face face text)
  text)

(defun cfw:tp (text prop value)
  "[internal] Put a text property to the entire text string."
  (if (< 0 (length text))
    (put-text-property 0 (length text) prop value text))
  text)

(defun cfw:define-keymap (keymap-list)
  "[internal] Key map definition utility.
KEYMAP-LIST is a source list like ((key . command) ... )."
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro (car i)) (car i))
         (cdr i)))
     keymap-list)
    map))

(defun cfw:trim (str)
  "[internal] Trim the space char-actors."
  (if (string-match "^[ \t\n\r]*\\(.*?\\)[ \t\n\r]*$" str)
      (match-string 1 str)
    str))



;;; Date Time Transformation

(defun cfw:date (month day year)
  "Construct a date object in the calendar format."
  (and month day year
       (list month day year)))

(defun cfw:emacs-to-calendar (time)
  "Transform an emacs time format to a calendar one."
  (let ((dt (decode-time time)))
    (list (nth 4 dt) (nth 3 dt) (nth 5 dt))))

(defun cfw:calendar-to-emacs (date)
  "Transform a calendar time format to an emacs one."
  (encode-time 0 0 0
               (calendar-extract-day date)
               (calendar-extract-month date)
               (calendar-extract-year date)))

(defun cfw:month-year-equal-p (date1 date2)
  "Return `t' if numbers of month and year of DATE1 is equals to
ones of DATE2. Otherwise is `nil'."
  (and
   (= (calendar-extract-month date1)
      (calendar-extract-month date2))
   (= (calendar-extract-year date1)
      (calendar-extract-year date2))))

(defun cfw:date-less-equal-p (d1 d2)
  "Return `t' if date value D1 is less than or equals to date value D2."
  (let ((ed1 (cfw:calendar-to-emacs d1))
        (ed2 (cfw:calendar-to-emacs d2)))
    (or (equal ed1 ed2)
        (time-less-p ed1 ed2))))

(defun cfw:date-between (begin end date)
  "Return `t' if date value DATE exists between BEGIN and END."
  (and (cfw:date-less-equal-p begin date)
       (cfw:date-less-equal-p date end)))

(defun cfw:month-year-contain-p (month year date2)
  "Return `t' if date value DATE2 is included in MONTH and YEAR."
  (and
   (= month (calendar-extract-month date2))
   (= year (calendar-extract-year date2))))

(defun cfw:date-after (date num)
  "Return the date after NUM days from DATE."
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date) num)))

(defun cfw:strtime-emacs (time)
  "Format emacs time value TIME to the string form YYYY/MM/DD."
  (format-time-string "%Y/%m/%d" time))

(defun cfw:strtime (date)
  "Format calendar date value DATE to the string form YYYY/MM/DD."
  (cfw:strtime-emacs (cfw:calendar-to-emacs date)))

(defun cfw:parsetime-emacs (str)
  "Transform the string format YYYY/MM/DD to an emacs time value."
  (when (string-match "\\([0-9]+\\)\\/\\([0-9]+\\)\\/\\([0-9]+\\)" str)
     (apply 'encode-time
            (let (ret)
              (dotimes (i 6)
                (push (string-to-number (or (match-string (+ i 1) str) "0")) ret))
              ret))))

(defun cfw:parsetime (str)
  "Transform the string format YYYY/MM/DD to a calendar date value."
  (cfw:emacs-to-calendar (cfw:parsetime-emacs str)))

(defun cfw:enumerate-days (begin end)
  "Enumerate date objects between BEGIN and END."
  (when (> (calendar-absolute-from-gregorian begin)
           (calendar-absolute-from-gregorian end))
    (error "Invalid period : %S - %S" begin end))
  (let ((d begin) ret (cont t))
    (while cont
      (push (copy-sequence d) ret)
      (setq cont (not (equal d end)))
      (setq d (cfw:date-after d 1)))
    (nreverse ret)))

(defun cfw:week-begin-date (date)
  "Return date of beginning of the week in which DATE is."
  (let ((num (- calendar-week-start-day
                (calendar-day-of-week date))))
    (cfw:date-after date (if (< 0 num) (- num cfw:week-days) num))))

(defun cfw:week-end-date (date)
  "Return date of end of the week in which DATE is."
  (let ((num (+ (- calendar-week-start-day 1)
              (- cfw:week-days (calendar-day-of-week date)))))
    (cfw:date-after date (cond
                          ((> 0 num) (+ num cfw:week-days))
                          ((<= cfw:week-days num) (- num cfw:week-days))
                          (t num)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component

;; This structure defines attributes of the calendar component.
;; These attributes are internal use. Other programs should access
;; through the functions of the component interface.

;; [cfw:component]
;; dest                   : an object of `cfw:dest'
;; model                  : an object of the calendar model
;; selected               : selected date
;; view                   : a symbol of view type (month, week, two-weeks, ...)
;; update-hooks           : a list of hook functions for update event
;; selectoin-change-hooks : a list of hook functions for selection change event
;; click-hooks            : a list of hook functions for click event

(defstruct cfw:component dest model selected view
  update-hooks selection-change-hooks click-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Source

;; This structure defines data sources of the calendar.

;; [cfw:source]
;; name   : data source title
;; data   : a function that generates an alist of date-contents
;; update : a function that is called when the user needs to update the contents (optional)
;; color  : foreground color for normal items (optional)
;; period-fgcolor  : foreground color for period items (optional)
;; period-bgcolor  : background color for period items (optional)
;; opt-face        : a plist of additional face properties for normal items (optional)
;; opt-period-face : a plist of additional face properties for period items (optional)
;;
;; If `period-bgcolor' is nil, the value of `color' is used.
;; If `period-fgcolor' is nil, the black or white (negative color of `period-bgcolor') is used.

(defstruct cfw:source name data update color period-bgcolor period-fgcolor opt-face opt-period-face)

(defun cfw:source-period-bgcolor-get (source)
  "[internal] Return a background color for period items.
If `cfw:source-period-bgcolor' is nil, the value of
`cfw:source-color' is used."
  (or (cfw:source-period-bgcolor source)
      (let ((c (cfw:source-color source)))
        (when c
          (setf (cfw:source-period-bgcolor source) c))
        c)))

(defun cfw:source-period-fgcolor-get (source)
  "[internal] Return a foreground color for period items.
If `cfw:source-period-fgcolor' is nil, the black or
white (negative color of `cfw:source-period-bgcolor') is used."
  (or (cfw:source-period-fgcolor source)
      (let ((c (destructuring-bind
                   (r g b) (color-values (or (cfw:source-period-bgcolor-get source) "black"))
                 (if (< 147500 (+ r g b)) "black" "white")))) ; (* 65536 3 0.75)
        (setf (cfw:source-period-fgcolor source) c)
        c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rendering Destination

;; This structure object is the abstraction of the rendering
;; destinations, such as buffers, regions and so on.

;; [cfw:dest]
;; type        : identify symbol for destination type. (buffer, region, text)
;; buffer      : a buffer object of rendering destination.
;; min-func    : a function that returns upper limit of rendering destination.
;; max-func    : a function that returns lower limit of rendering destination.
;; width       : width of the reference size.
;; height      : height of the reference size.
;; clear-func  : a function that clears the rendering destination.
;; before-update-func : a function that is called at the beginning of rendering routine.
;; after-update-func  : a function that is called at the end of rendering routine.
;; select-ol   : a list of overlays for selection
;; today-ol    : a list of overlays for today

(defstruct cfw:dest
  type buffer min-func max-func width height
  clear-func before-update-func after-update-func select-ol today-ol)

;; shortcut functions

(eval-when-compile
  (defmacro cfw:dest-with-region (dest &rest body)
    (let (($dest (gensym)))
      `(let ((,$dest ,dest))
         (with-current-buffer (cfw:dest-buffer ,$dest)
           (save-restriction
             (narrow-to-region
              (cfw:dest-point-min ,$dest) (cfw:dest-point-max ,$dest))
             ,@body))))))
(put 'cfw:dest-with-region 'lisp-indent-function 1)

(defun cfw:dest-point-min (c)
  (funcall (cfw:dest-min-func c)))

(defun cfw:dest-point-max (c)
  (funcall (cfw:dest-max-func c)))

(defun cfw:dest-clear (c)
  (funcall (cfw:dest-clear-func c)))

(defun cfw:dest-before-update (c)
  (when (cfw:dest-before-update-func c)
    (funcall (cfw:dest-before-update-func c))))

(defun cfw:dest-after-update (c)
  (when (cfw:dest-after-update-func c)
    (funcall (cfw:dest-after-update-func c))))

;; private functions

(defun cfw:dest-ol-selection-clear (dest)
  "[internal] Clear the selection overlays on the current calendar view."
  (loop for i in (cfw:dest-select-ol dest)
        do (delete-overlay i))
  (setf (cfw:dest-select-ol dest) nil))

(defun cfw:dest-ol-selection-set (dest date)
  "[internal] Put a selection overlay on DATE. The selection overlay can be
 put on some days, calling this function many times.  If DATE is
 not included on the current calendar view, do nothing. This
 function does not manage the selections, just put the overlay."
  (lexical-let (ols)
    (cfw:dest-with-region dest
      (cfw:find-all-by-date
       date
       (lambda (begin end)
         (let ((overlay (make-overlay begin end)))
           (overlay-put overlay 'face
                        (if (eq 'cfw:face-day-title
                                (get-text-property begin 'face))
                            'cfw:face-select))
           (push overlay ols)))))
    (setf (cfw:dest-select-ol dest) ols)))

(defun cfw:dest-ol-today-clear (dest)
  "[internal] Clear decoration overlays."
  (loop for i in (cfw:dest-today-ol dest)
        do (delete-overlay i))
  (setf (cfw:dest-today-ol dest) nil))

(defun cfw:dest-ol-today-set (dest)
  "[internal] Put a highlight face on today."
  (lexical-let (ols)
    (cfw:dest-with-region dest
      (cfw:find-all-by-date
       (calendar-current-date)
       (lambda (begin end)
         (let ((overlay (make-overlay begin end)))
           (overlay-put overlay 'face
                        (if (eq 'cfw:face-day-title
                                (get-text-property begin 'face))
                            'cfw:face-today-title 'cfw:face-today))
           (push overlay ols)))))
    (setf (cfw:dest-today-ol dest) ols)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low level API

;; Buffer

(defconst cfw:calendar-buffer-name "*cfw-calendar*" "[internal] Default buffer name for the calendar view.")

(defun cfw:dest-init-buffer (&optional buf width height custom-map)
  "Create a buffer destination.
This destination uses an entire buffer and set up the major-mode
`cfw:calendar-mode' and the key map `cfw:calendar-mode-map'.  BUF
is a buffer name to render the calendar view. If BUF is nil, the
default buffer name `cfw:calendar-buffer-name' is used.  WIDTH
and HEIGHT are reference size of the calendar view. If those are
nil, the size of calendar is calculated from the window that
shows BUF or the selected window.  The component
object is stored at the buffer local variable `cfw:component'.
CUSTOM-MAP is the additional keymap that is added to default
keymap `cfw:calendar-mode-map'."
  (lexical-let
      ((buffer (or buf (get-buffer-create cfw:calendar-buffer-name)))
       (window (or (and buf (get-buffer-window buf)) (selected-window)))
       dest)
    (setq dest
          (make-cfw:dest
           :type 'buffer
           :min-func 'point-min
           :max-func 'point-max
           :buffer buffer
           :width (or width (window-width window))
           :height (or height (window-height window))
           :clear-func (lambda ()
                         (with-current-buffer buffer
                           (erase-buffer)))))
    (with-current-buffer buffer
      (unless (eq major-mode 'cfw:calendar-mode)
        (cfw:calendar-mode custom-map)))
    dest))

;; Region

(defun cfw:dest-init-region (buf mark-begin mark-end &optional width height)
  "Create a region destination.  The calendar is drew between
MARK-BEGIN and MARK-END in the buffer BUF.  MARK-BEGIN and
MARK-END are separated by more than one character, such as a
space.  This destination is employed to be embedded in the some
application buffer.  Because this destination does not set up
any modes and key maps for the buffer, the application that uses
the calfw is responsible to manage the buffer and key maps."
  (lexical-let
      ((mark-begin mark-begin) (mark-end mark-end)
       (window (or (get-buffer-window buf) (selected-window))))
    (make-cfw:dest
     :type 'region
     :min-func (lambda () (marker-position mark-begin))
     :max-func (lambda () (marker-position mark-end))
     :buffer buf
     :width (or width (window-width window))
     :height (or height (window-height window))
     :clear-func
     (lambda ()
         (cfw:dest-region-clear (marker-position mark-begin)
                                (marker-position mark-end)))
     )))

(defun cfw:dest-region-clear (begin end)
  "[internal] Clear the content text."
  (when (< 2 (- end begin))
    (delete-region begin (1- end)))
  (goto-char begin))

;; Inline text

(defconst cfw:dest-background-buffer " *cfw:dest-background*")

(defun cfw:dest-init-inline (width height)
  "Create a text destination."
  (lexical-let
      ((buffer (get-buffer-create cfw:dest-background-buffer))
       (window (selected-window))
       dest)
    (setq dest
          (make-cfw:dest
           :type 'text
           :min-func 'point-min
           :max-func 'point-max
           :buffer buffer
           :width (or width (window-width window))
           :height (or height (window-height window))
           :clear-func (lambda ()
                         (with-current-buffer buffer
                           (erase-buffer)))))
    dest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component API

;; Create

(defun cfw:cp-new (dest model view &optional selected-date)
  "[internal] Create a new component object.
DEST is a cfw:dest object.  MODEL is a model object.  VIEW is a
symbol of the view type: month, two-week, week and day.
SELECTED-DATE is a selected date initially.  This function is
called by the initialization functions,
`cfw:create-calendar-component-buffer',
`cfw:create-calendar-component-region' and
`cfw:get-calendar-text'."
  (let ((cp (make-cfw:component
             :dest  dest
             :model model
             :view  (or view 'month)
             :selected (or selected-date (calendar-current-date)))))
    (cfw:cp-update cp)
    cp))

;; Getting the component instance

(defun cfw:cp-get-component ()
  "Return the component object on the current cursor position.
Firstly, getting a text property `cfw:component' on the current
position. If no object is found in the text property, the buffer
local variable `cfw:component' is tried to get. If no object is
found at the variable, return nil."
  (let ((component (get-text-property (point) 'cfw:component)))
    (unless component
      (unless (local-variable-p 'cfw:component (current-buffer))
        (error "Not found cfw:component attribute..."))
      (setq component (buffer-local-value 'cfw:component (current-buffer))))
    component))

;; Getter

(defun cfw:cp-get-selected-date (component)
  "Return the selected date of the component."
  (cfw:component-selected component))

(defun cfw:cp-get-contents-sources (component)
  "Return a list of the content sources."
  (cfw:model-get-contents-sources (cfw:component-model component)))

(defun cfw:cp-get-annotation-sources (component)
  "Return a list of the annotation sources."
  (cfw:model-get-annotation-sources (cfw:component-model component)))

(defun cfw:cp-get-view (component)
  "Return a symbol of the current view type."
  (cfw:component-view component))

(defun cfw:cp-get-buffer (component)
  "Return a buffer object on which the component draws the content."
  (cfw:dest-buffer (cfw:component-dest component)))

(defun cfw:cp-displayed-date-p (component date)
  "If the date is displayed in the current view, return `t'. Otherwise return `nil'."
  (let* ((model (cfw:component-model component))
         (begin (cfw:k 'begin-date model))
         (end (cfw:k 'end-date model)))
    (unless (and begin end) (error "Wrong model : %S" model))
    (cfw:date-between begin end date)))

;; Setter

(defun cfw:cp-move-cursor (date)
  "[internal] Just move the cursor onto the date. This function
is called by `cfw:cp-set-selected-date'."
  (let ((pos (cfw:find-by-date date)))
    (when pos
      (goto-char pos)
      (unless (eql (selected-window) (get-buffer-window (current-buffer)))
        (set-window-point (get-buffer-window (current-buffer)) pos)))))

(defun cfw:cp-set-selected-date (component date)
  "Select the date on the component. If the current view doesn't contain the date,
this function updates the view to display the date."
  (let ((last (cfw:component-selected component))
        (dest (cfw:component-dest component))
        (model (cfw:component-model component)))
    (cond
     ((cfw:cp-displayed-date-p component date)
      (setf (cfw:component-selected component) date)
      (cfw:dest-before-update dest)
      (cfw:dest-ol-selection-clear dest)
      (cfw:dest-ol-selection-set dest date)
      (cfw:dest-after-update dest)
      (cfw:cp-move-cursor date)
      (unless (equal last date)
        (cfw:cp-fire-selection-change-hooks component)))
     (t
      (cfw:model-set-init-date date model)
      (setf (cfw:component-selected component) date)
      (cfw:cp-update component)
      (cfw:cp-fire-selection-change-hooks component)
      ;; Because this function will be called from cfw:cp-update, do nothing here.
      ))))

(defun cfw:cp-set-contents-sources (component sources)
  "Set content sources for the component.
SOURCES is a list of content sources."
  (cfw:model-set-contents-sources
   (cfw:component-model component) sources))

(defun cfw:cp-set-annotation-sources (component sources)
  "Set annotation sources for the component.
SOURCES is a list of annotation sources."
  (cfw:model-set-annotation-sources
   (cfw:component-model component) sources))

(defun cfw:cp-set-view (component view)
  "Change the view type of the component and re-draw the content.
VIEW is a symbol of the view type."
  (setf (cfw:component-view component) view)
  (cfw:cp-update component))

(defun cfw:cp-resize (component width height)
  "Resize the component size and re-draw the content."
  (let* ((dest (cfw:component-dest component))
         (buf (cfw:dest-buffer dest))
         (window (or (and buf (get-buffer-window buf)) (selected-window))))
    (setf (cfw:dest-width dest) (or width (window-width window))
          (cfw:dest-height dest) (or height (window-height window))))
  (cfw:cp-update component))

;; Hook

(defun cfw:cp-add-update-hook (component hook)
  "Add the update hook function to the component.
HOOK is a function that has no argument."
  (push hook (cfw:component-update-hooks component)))

(defun cfw:cp-add-selection-change-hook (component hook)
  "Add the selection change hook function to the component.
HOOK is a function that has no argument."
  (push hook (cfw:component-selection-change-hooks component)))

(defun cfw:cp-add-click-hook (component hook)
  "Add the click hook function to the component.
HOOK is a function that has no argument."
  (push hook (cfw:component-click-hooks component)))



;;; private methods

(defun cfw:cp-dispatch-view-impl (view)
  "[internal] Return a view function which is corresponding to the view symbol.
VIEW is a symbol of the view type."
  (cond
   ((eq 'month     view)  'cfw:view-month)
   ((eq 'week      view)  'cfw:view-week)
   ((eq 'two-weeks view)  'cfw:view-two-weeks)
   ((eq 'day       view)  'cfw:view-day)
   (t (error "Not found such view : %s" view))))

(defun cfw:cp-update (component)
  "[internal] Clear and re-draw the component content."
  (let* ((buf (cfw:cp-get-buffer component))
         (dest (cfw:component-dest component)))
    (with-current-buffer buf
      (cfw:dest-before-update dest)
      (cfw:dest-ol-selection-clear dest)
      (cfw:dest-ol-today-clear dest)
      (let ((buffer-read-only nil))
        (cfw:dest-with-region dest
          (cfw:dest-clear dest)
          (funcall (cfw:cp-dispatch-view-impl
                    (cfw:component-view component))
                   component)))
      (cfw:dest-ol-today-set dest)
      (cfw:cp-set-selected-date
       component (cfw:component-selected component))
      (cfw:dest-after-update dest)
      (cfw:cp-fire-update-hooks component))))

(defun cfw:cp-fire-click-hooks (component)
  "[internal] Call click hook functions of the component with no arguments."
  (loop for f in (cfw:component-click-hooks component)
        do (condition-case err
               (funcall f)
             (nil (message "Calfw: Click / Hook error %S [%s]" f err)))))

(defun cfw:cp-fire-selection-change-hooks (component)
  "[internal] Call selection change hook functions of the component with no arguments."
  (loop for f in (cfw:component-selection-change-hooks component)
        do (condition-case err
               (funcall f)
             (nil (message "Calfw: Selection change / Hook error %S [%s]" f err)))))

(defun cfw:cp-fire-update-hooks (component)
  "[internal] Call update hook functions of the component with no arguments."
  (loop for f in (cfw:component-update-hooks component)
        do (condition-case err
               (funcall f)
             (nil (message "Calfw: Update / Hook error %S [%s]" f err)))))



;;; Models

(defun cfw:model-abstract-new (date contents-sources annotation-sources)
  "Return an abstract model object.
DATE is initial date for the calculation of the start date and end one.
CONTENTS-SOURCES is a list of contents functions.
ANNOTATION-SOURCES is a list of annotation functions."
  (unless date (setq date (calendar-current-date)))
  `((init-date . ,date)
    (contents-sources . ,contents-sources)
    (annotation-sources . ,annotation-sources)))

(defun cfw:model-abstract-derived (date org-model)
  "Return an abstract model object. The contents functions and annotation ones are copied from ORG-MODEL.
DATE is initial date for the calculation of the start date and end one.
ORG-MODEL is a model object to inherit."
  (cfw:model-abstract-new
   date
   (cfw:model-get-contents-sources org-model)
   (cfw:model-get-annotation-sources org-model)))

(defun cfw:model-create-updated-view-data (model view-data)
  "[internal] Clear previous view model data from MODEL and return a new model with VIEW-DATA."
  (append
   (cfw:model-abstract-derived
    (cfw:k 'init-date model) model)
   view-data))

;; public functions

(defun cfw:model-get-holiday-by-date (date model)
  "Return a holiday title on the DATE."
  (cfw:contents-get date (cfw:k 'holidays model)))

(defun cfw:model-get-contents-by-date (date model)
  "Return a list of contents on the DATE."
  (cfw:contents-get date (cfw:k 'contents model)))

(defun cfw:model-get-annotation-by-date (date model)
  "Return an annotation on the DATE."
  (cfw:contents-get date (cfw:k 'annotations model)))

(defun cfw:model-get-periods-by-date (date model)
  "Return a list of periods on the DATE."
  (loop for period in (cfw:k 'periods model)
        for (begin end content) = period
        if (cfw:date-between begin end date)
        collect period))

;; private functions

(defun cfw:model-get-contents-sources (model)
  "[internal] Return a list of content sources of the model."
  (cfw:k 'contents-sources model))

(defun cfw:model-get-annotation-sources (model)
  "[internal] Return a list of annotation sources of the model."
  (cfw:k 'annotation-sources model))

(defun cfw:model-set-init-date (date model)
  "[internal] Set the init-date that is used to calculate the
display period of the calendar."
  (let ((cell (assq 'init-date model)))
    (cond
     (cell (setcdr cell date))
     (t (push (cons 'init-date date) model))))
  date)

(defun cfw:model-set-contents-sources (sources model)
  "[internal] Set the content sources of the model."
  (let ((cell (assq 'contents-sources model)))
    (cond
     (cell (setcdr cell sources))
     (t (push (cons 'contents-sources sources) model))))
  sources)

(defun cfw:model-set-annotation-sources (sources model)
  "[internal] Set the annotation sources of the model."
  (let ((cell (assq 'annotation-sources model)))
    (cond
     (cell (setcdr cell sources))
     (t (push (cons 'annotation-sources sources) model))))
  sources)

(defun cfw:contents-get (date contents)
  "[internal] Return a list of contents on the DATE."
  (cdr (cfw:contents-get-internal date contents)))

(defun cfw:contents-get-internal (date contents)
  "[internal] Return a cons cell that has the key DATE.
One can modify the returned cons cell destructively."
  (cond
   ((or (null date) (null contents)) nil)
   (t (loop for i in contents
            if (equal date (car i))
            return i
            finally return nil))))

(defun cfw:contents-add (date content contents)
  "[internal] Add a record, DATE as a key and CONTENT as a body,
to CONTENTS destructively. If CONTENTS has a record for DATE,
this function appends CONTENT to the record. Return the modified
contents list."
  (let* ((prv (cfw:contents-get-internal date contents))
         (lst (if (listp content) (copy-sequence content) (list content))))
    (if prv
        (setcdr prv (append (cdr prv) lst))
      (push (cons date lst) contents)))
  contents)

(defun cfw:contents-merge (begin end sources)
  "[internal] Return an contents alist between begin date and end one,
calling functions `:data' function."
  (cond
   ((null sources) nil)
   ((= 1 (length sources))
    (cfw:contents-put-source
     (funcall (cfw:source-data (car sources)) begin end)
     (car sources)))
   (t
    (loop for s in sources
          for f = (cfw:source-data s)
          for cnts = (cfw:contents-put-source
                      (funcall f begin end) s)
          with contents = nil
          do
          (loop for c in cnts
                for (d . line) = c
                do (setq contents (cfw:contents-add d line contents)))
          finally return contents))))

(defun cfw:contents-put-source (contents source)
  "[internal] Put the source object to the text property
`cfw:source' in the contents list. During rendering, the source
object is used to put some face property."
  (cond
   ((null source) contents)
   (t
    (loop for (k . lst) in contents
          if (eq k 'periods)
          collect ; periods
          (cons k
                (loop for (begin end summaries) in lst
                      for summary-text = (if (listp summaries)
                                             (mapconcat 'identity summaries " ")
                                           summaries)
                      collect (list
                               begin end
                               (cfw:tp summary-text 'cfw:source source))))
          else
          collect ; contents
          (cons k
                (loop for i in lst
                      collect (cfw:tp i 'cfw:source source)))))))

(defun cfw:annotations-merge (begin end sources)
  "[internal] Return an annotation alist between begin date and end one,
calling functions `cfw:annotations-functions'."
  (cond
   ((null sources) nil)
   ((= 1 (length sources))
    (funcall (cfw:source-data (car sources)) begin end))
   (t
    (loop for s in sources
          for f = (cfw:source-data s)
          for cnts = (funcall f begin end)
          with annotations = nil
          do
          (loop for c in cnts
                for (d . line) = c
                for prv = (cfw:contents-get-internal d annotations)
                if prv
                do (setcdr prv (concat (cdr prv) "/" line))
                else
                do (push (cons d line) annotations))
          finally return annotations))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rendering Utilities

(defun cfw:render-title-month (date)
  "Render the calendar title for the monthly view."
  (format "%4s / %s"
          (calendar-extract-year date)
          (aref calendar-month-name-array
                (1- (calendar-extract-month date)))))

(defun cfw:render-title-period (begin-date end-date)
  "Render the calendar title for the period view between BEGIN-DATE and END-DATE."
  (cond
   ((eql (calendar-extract-month begin-date) (calendar-extract-month end-date))
    (format "%4s / %s %s - %s"
            (calendar-extract-year begin-date)
            (aref calendar-month-name-array (1- (calendar-extract-month begin-date)))
            (calendar-extract-day begin-date)
            (calendar-extract-day end-date)))
   (t
    (format "%4s / %s %s - %s %s"
            (calendar-extract-year begin-date)
            (aref calendar-month-name-array (1- (calendar-extract-month begin-date)))
            (calendar-extract-day begin-date)
            (aref calendar-month-name-array (1- (calendar-extract-month end-date)))
            (calendar-extract-day end-date)))))

(defun cfw:render-title-day (date)
  "Render the calendar title for the day view on DATE."
  (format "%4s / %s %s"
          (calendar-extract-year date)
          (aref calendar-month-name-array
                (1- (calendar-extract-month date)))
          (calendar-extract-day date)))

(defun cfw:render-center (width string &optional padding)
  "[internal] Format STRING in the center, padding on the both
sides with the character PADDING."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string
                       (cfw:render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (/ (- width len) 2)))
    (concat
     (make-string margin padding) cnt
     (make-string (- width len margin) padding))))

(defun cfw:render-left (width string &optional padding)
  "[internal] Format STRING, padding on the right with the character PADDING."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string
                       (cfw:render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (- width len)))
    (concat cnt (make-string margin padding))))

(defun cfw:render-right (width string &optional padding)
  "[internal] Format STRING, padding on the left with the character PADDING."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string
                       (cfw:render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (- width len)))
    (concat (make-string margin padding) cnt)))

(defun cfw:render-add-right (width left right &optional padding)
  "[internal] Layout strings LEFT and RIGHT within WIDTH."
  (let* ((padding (or padding ?\ ))
         (lcnt (or (and left
                        (cfw:render-truncate left width t))
                   ""))
         (llen (string-width lcnt))
         (rmargin (- width llen))
         (right (cfw:trim right))
         (rcnt (or (and right (> rmargin 0)
                        (cfw:render-truncate right rmargin))
                   ""))
         (cmargin (- width llen (string-width rcnt))))
    (concat lcnt (if (< 0 cmargin) (make-string cmargin padding)) rcnt)))

(defun cfw:render-sort-contents (lst)
  "[internal] Sort the string list LST. Maybe need to improve the sorting rule..."
  (sort (copy-sequence lst) 'string-lessp))

(defun cfw:render-get-face-period (text default-face)
  "[internal] Return a face for the source object of the period text."
  (let* ((src (get-text-property 0 'cfw:source text))
         (bg-color (and src (cfw:source-period-bgcolor-get src)))
         (fg-color (and src (cfw:source-period-fgcolor-get src))))
    (cond
     ((or (null src) (null bg-color)) default-face)
     (t (append (list ':background bg-color ':foreground fg-color)
                (cfw:source-opt-period-face src))))))

(defun cfw:render-get-face-content (text default-face)
  "[internal] Return a face for the source object of the content text."
  (let* ((src (get-text-property 0 'cfw:source text))
         (fg-color (and src (cfw:source-color src))))
    (cond
     ((or (null src) (null fg-color)) default-face)
     (t (append (list ':foreground fg-color) (cfw:source-opt-face src))))))

(defun cfw:render-default-content-face (str &optional default-face)
  "[internal] Put the default content face. If STR has some
faces, the faces are remained."
  (loop for i from 0 below (length str)
        with ret = (substring str 0)
        with face = (or default-face
                        (cfw:render-get-face-content
                         str 'cfw:face-default-content))
        unless (get-text-property i 'face ret)
        do
        (put-text-property i (1+ i) 'face face ret)
        (put-text-property i (1+ i) 'font-lock-face face ret)
        finally return ret))

(defun cfw:render-get-week-face (daynum &optional default-face)
  "[internal] Put the default week face."
  (cond
   ((= daynum cfw:week-saturday)
    'cfw:face-saturday)
   ((= daynum cfw:week-sunday)
    'cfw:face-sunday)
   (t default-face)))

(defun cfw:render-truncate (org limit-width &optional ellipsis)
  "[internal] Truncate a string ORG with LIMIT-WIDTH, like `truncate-string-to-width'."
  (setq org (replace-regexp-in-string "\n" " " org))
  (if (< limit-width (string-width org))
      (let ((str (truncate-string-to-width
                  (substring org 0) limit-width 0 nil ellipsis)))
        (cfw:tp str 'mouse-face 'highlight)
        (cfw:tp str 'help-echo org)
        str)
    org))

(defface cfw:face-toolbar
  '((((class color) (background light))
     :foreground "Gray90" :background "Gray90")
    (((class color) (background dark))
     :foreground "Steelblue4" :background "Steelblue4"))
  "Face for toolbar" :group 'calfw)

(defface cfw:face-toolbar-button-off
  '((((class color) (background light))
     :foreground "Lightskyblue4" :background "White")
    (((class color) (background dark))
     :foreground "Gray10" :weight bold))
  "Face for button on toolbar" :group 'calfw)

(defface cfw:face-toolbar-button-on
  '((((class color) (background light))
     :foreground "Lightpink3" :background "Gray94" )
    (((class color) (background dark))
     :foreground "Gray50" :weight bold))
  "Face for button on toolbar" :group 'calfw)

(defun cfw:render-button (title command &optional state)
  "[internal] Return a decorated text for the toolbar buttons.
TITLE is a button title.  COMMAND is a interactive command
function called by clicking.  If STATE is non-nil, the face
`cfw:face-toolbar-button-on' is applied. Otherwise
`cfw:face-toolbar-button-off' is applied."
  (let ((text (concat "[" title "]"))
        (keymap (make-sparse-keymap)))
    (cfw:rt text (if state 'cfw:face-toolbar-button-on
                   'cfw:face-toolbar-button-off))
    (define-key keymap [mouse-1] command)
    (cfw:tp text 'keymap keymap)
    (cfw:tp text 'mouse-face 'highlight)
    text))

(defun cfw:render-toolbar (width current-view prev-cmd next-cmd)
  "[internal] Return a text of the toolbar.
WIDTH is width of the toolbar.
CURRENT-VIEW is a symbol of the current view type. This symbol is used to select the button faces on the toolbar.
PREV-CMD and NEXT-CMD are the moving view command, such as `cfw:navi-previous(next)-month-command' and `cfw:navi-previous(next)-week-command'."
  (let* ((prev (cfw:render-button " < " prev-cmd))
         (today (cfw:render-button "Today" 'cfw:navi-goto-today-command))
         (next (cfw:render-button " > " next-cmd))
         (month (cfw:render-button
                 "Month" 'cfw:change-view-month
                 (eq current-view 'month)))
         (tweek (cfw:render-button
                 "Two Weeks" 'cfw:change-view-two-weeks
                 (eq current-view 'two-weeks)))
         (week (cfw:render-button
                "Week" 'cfw:change-view-week
                (eq current-view 'week)))
         (day (cfw:render-button
               "Day" 'cfw:change-view-day
               (eq current-view 'day)))
         (sp  " ")
         (toolbar-text
          (cfw:render-add-right
           width (concat sp prev sp next sp today sp)
           (concat day sp week sp tweek sp month sp))))
    (cfw:render-default-content-face toolbar-text 'cfw:face-toolbar)))

(defun cfw:render-footer (width sources)
  "[internal] Return a text of the footer."
  (let* ((whole-text
          (mapconcat
           'identity
           (loop for s in sources
                 for title = (cfw:tp (substring (cfw:source-name s) 0)
                                     'cfw:source s)
                 for dot   = (cfw:tp (substring "(==)" 0) 'cfw:source s)
                 collect
                 (cfw:render-default-content-face
                  (concat
                   "[" (cfw:rt dot (cfw:render-get-face-period dot 'cfw:face-periods))
                   " " title "]")
                  (cfw:render-get-face-content title 'cfw:face-default-content)))
           "  ")))
    (cfw:render-default-content-face
     (cfw:render-left width (concat " " whole-text)) 'cfw:face-toolbar)))

(defun cfw:render-periods (date week-day periods-stack cell-width)
  "[internal] This function translates PERIOD-STACK to display content on the DATE."
  (when periods-stack
    (let ((stack (sort (copy-sequence periods-stack)
                       (lambda (a b) (< (car a) (car b))))))
      (loop for i from 0 below (car (car stack))
            do (push ; insert blank lines
                (list i (list nil nil nil))
                stack))
      (loop for (row (begin end content)) in stack
            for beginp = (equal date begin)
            for endp = (equal date end)
            for width = (- cell-width (if beginp 1 0) (if endp 1 0))
            for title = (if (and content
                                 (or (equal date begin)
                                     (eql 1 (calendar-extract-day date))
                                     (eql week-day calendar-week-start-day)))
                            (cfw:render-truncate content width t) "")
            collect
            (if content
                (cfw:rt
                 (concat
                  (if beginp "(" "")
                  (cfw:render-left width title ?-)
                  (if endp ")" ""))
                 (cfw:render-get-face-period content 'cfw:face-periods))
              "")))))

(defun cfw:render-periods-get-min (periods-each-days begin end)
  "[internal] Find the minimum empty row number of the days between
BEGIN and END from the PERIODS-EACH-DAYS."
  (loop for row-num from 0 below 10 ; assuming the number of stacked periods is less than 10
        unless
        (loop for d in (cfw:enumerate-days begin end)
              for periods-stack = (cfw:contents-get d periods-each-days)
              if (and periods-stack (assq row-num periods-stack))
              return t)
        return row-num))

(defun cfw:render-periods-place (periods-each-days row period)
  "[internal] Assign PERIOD content to the ROW-th row on the days of the period,
and append the result to periods-each-days."
  (loop for d in (cfw:enumerate-days (car period) (cadr period))
        for periods-stack = (cfw:contents-get-internal d periods-each-days)
        if periods-stack
        do (setcdr periods-stack (append (cdr periods-stack)
                                         (list (list row period))))
        else
        do (push (cons d (list (list row period))) periods-each-days))
  periods-each-days)

(defun cfw:render-periods-stacks (model)
  "[internal] Arrange the `periods' records of the model and
create period-stacks on the each days.
period-stack -> ((row-num . period) ... )"
  (let* (periods-each-days)
    (loop for period in (cfw:k 'periods model)
          for (begin end content) = period
          for row = (cfw:render-periods-get-min
                     periods-each-days begin end)
          do
          (setq periods-each-days
                (cfw:render-periods-place
                 periods-each-days row period)))
    periods-each-days))

(defun cfw:render-columns (day-columns param)
  "[internal] This function concatenates each rows on the days into a string of a physical line.
DAY-COLUMNS is a list of columns. A column is a list of following form: (DATE (DAY-TITLE . ANNOTATION-TITLE) STRING STRING...)."
  (let ((cell-width  (cfw:k 'cell-width  param))
        (cell-height (cfw:k 'cell-height param))
        (EOL (cfw:k 'eol param)) (VL (cfw:k 'vl param))
        (hline (cfw:k 'hline param)) (cline (cfw:k 'cline param)))
    (loop for day-rows in day-columns
          for date = (car day-rows)
          for (tday . ant) = (cadr day-rows)
          do
          (insert
           VL (if date
                  (cfw:tp
                   (cfw:render-default-content-face
                    (cfw:render-add-right cell-width tday ant)
                    'cfw:face-day-title)
                   'cfw:date date)
                (cfw:render-left cell-width ""))))
    (insert VL EOL)
    (loop for i from 2 upto cell-height do
          (loop for day-rows in day-columns
                for date = (car day-rows)
                for row = (nth i day-rows)
                do
                (insert
                 VL (cfw:tp
                     (cfw:render-left cell-width (and row (format "%s" row)))
                     'cfw:date date)))
          (insert VL EOL))
    (insert cline)))

(defun cfw:render-append-parts (param)
  "[internal] Append rendering parts to PARAM and return a new list."
  (let* ((EOL "\n")
         (cell-width (cfw:k 'cell-width param))
         (columns (cfw:k 'columns param))
         (num-cell-char 
          (/ cell-width (char-width cfw:fchar-horizontal-line))))
    (append
     param
     `((eol . ,EOL) (vl . ,(cfw:rt (make-string 1 cfw:fchar-vertical-line) 'cfw:face-grid))
       (hline . ,(cfw:rt 
                  (concat
                   (loop for i from 0 below columns concat
                         (concat
                          (make-string 1 (if (= i 0) cfw:fchar-top-left-corner cfw:fchar-top-junction))
                          (make-string num-cell-char cfw:fchar-horizontal-line)))
                   (make-string 1 cfw:fchar-top-right-corner) EOL)
                  'cfw:face-grid))
       (cline . ,(cfw:rt 
                  (concat
                   (loop for i from 0 below columns concat
                         (concat
                          (make-string 1 (if (= i 0) cfw:fchar-left-junction cfw:fchar-junction))
                          (make-string num-cell-char cfw:fchar-horizontal-line)))
                   (make-string 1 cfw:fchar-right-junction) EOL) 'cfw:face-grid))))))

(defun cfw:render-day-of-week-names (model param)
  "[internal] Insert week names."
  (loop for i in (cfw:k 'headers model)
        with VL = (cfw:k 'vl param) with cell-width = (cfw:k 'cell-width param)
        for name = (aref calendar-day-name-array i) do
        (insert VL (cfw:rt (cfw:render-center cell-width name)
                           (cfw:render-get-week-face i 'cfw:face-header)))))

(defun cfw:render-calendar-cells-weeks (model param title-func)
  "[internal] Insert calendar cells for week based views."
  (loop for week in (cfw:k 'weeks model) ; week rows loop
        with cell-width = (cfw:k 'cell-width param) do
        (cfw:render-columns
         (loop for date in week ; week columns loop
               for count from 0 below (length week)
               for hday = (car (cfw:contents-get date (cfw:k 'holidays model)))
               for week-day = (nth count (cfw:k 'headers  model))
               for ant = (cfw:rt (cfw:contents-get
                                  date (cfw:k 'annotations model))
                                 'cfw:face-annotation)
               for raw-periods = (cfw:contents-get
                                  date (cfw:render-periods-stacks model))
               for raw-contents = (cfw:render-sort-contents
                                   (cfw:contents-get
                                    date (cfw:k 'contents model)))
               for prs-contents = (append
                                   (cfw:render-periods
                                    date week-day raw-periods cell-width)
                                   (mapcar 'cfw:render-default-content-face raw-contents))
               for num-label = (if prs-contents
                                   (format "(%s)"
                                           (+ (length raw-contents)
                                              (length raw-periods))) "")
               for tday = (concat
                           " " ; margin
                           (funcall title-func date week-day hday)
                           (if num-label (concat " " num-label))
                           (if hday (concat " " (cfw:rt (substring hday 0)
                                                        'cfw:face-holiday))))
               collect
               (cons date (cons (cons tday ant) prs-contents)))
         param)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Views

;;; view model utilities

(defun cfw:view-model-make-weeks (begin-date end-date)
  "[internal] Return a list of weeks those have 7 days."
  (let* ((first-day-day (calendar-day-of-week begin-date)) weeks)
    (loop with i = begin-date
          with day = calendar-week-start-day
          with week = nil
          do
          ;; flush a week
          (when (and (= day calendar-week-start-day) week)
            (push (nreverse week) weeks)
            (setq week nil)
            (when (cfw:date-less-equal-p end-date i) (return)))
          ;; add a day
          (push i week)
          ;; increment
          (setq day (% (1+ day) cfw:week-days))
          (setq i (cfw:date-after i 1)))
    (nreverse weeks)))

(defun cfw:view-model-make-days (begin-date end-date)
  "[internal] Return a list of days for linear views."
  (loop with days = nil
        with i = begin-date
        do
        (push i days)
        (when (cfw:date-less-equal-p end-date i)
          (return (reverse days)))
        (setq i (cfw:date-after i 1))))

(defun cfw:view-model-make-day-names-for-week ()
  "[internal] Return a list of index of day of the week."
  (loop for i from 0 below cfw:week-days
        collect (% (+ calendar-week-start-day i) cfw:week-days)))

(defun cfw:view-model-make-day-names-for-days (begin-date end-date)
  "[internal] Return a list of index of day of the week for linear views."
  (loop with day = (calendar-day-of-week begin-date)
        with day-names = nil
        with i = begin-date
        do
        (push day day-names)
        (when (cfw:date-less-equal-p end-date i)
          (return (reverse day-names)))
        (setq day (% (1+ day) cfw:week-days))
        (setq i (cfw:date-after i 1))))

(defun cfw:view-model-make-holidays (date)
  "[internal] Return an alist of holidays around DATE."
  (let ((displayed-month (calendar-extract-month date))
        (displayed-year (calendar-extract-year date)))
    (calendar-holiday-list)))

(defun cfw:view-model-make-common-data (model begin-date end-date &optional lst)
  "[internal] Return an alist of common data for the model."
  (let* ((contents-all (cfw:contents-merge
                        begin-date end-date
                        (cfw:model-get-contents-sources model))))
    (append
    `(; common data
      (begin-date . ,begin-date) (end-date . ,end-date)
      (holidays . ,(cfw:view-model-make-holidays begin-date)) ; an alist of holidays, (DATE HOLIDAY-NAME)
      (annotations . ,(cfw:annotations-merge ; an alist of annotations, (DATE ANNOTATION)
                       begin-date end-date
                       (cfw:model-get-annotation-sources model)))
      (contents . ,(loop for i in contents-all
                         unless (eq 'periods (car i))
                         collect i)) ; an alist of contents, (DATE LIST-OF-CONTENTS)
      (periods . ,(cfw:k 'periods contents-all))) ; a list of periods, (BEGIN-DATE END-DATE SUMMARY)
    lst)))

(defun cfw:view-model-make-common-data-for-weeks (model begin-date end-date)
  "[internal] Return a model object for week based views."
  (cfw:model-create-updated-view-data
   model
   (cfw:view-model-make-common-data
    model begin-date end-date
     `((headers . ,(cfw:view-model-make-day-names-for-week)) ; a list of the index of day-of-week
       (weeks . ,(cfw:view-model-make-weeks ; a matrix of day-of-month, which corresponds to the index of `headers'
                  (cfw:week-begin-date begin-date)
                  (cfw:week-end-date   end-date)))))))

(defun cfw:view-model-make-common-data-for-days (model begin-date end-date)
  "[internal] Return a model object for linear views."
  (cfw:model-create-updated-view-data
   model
   (cfw:view-model-make-common-data
    model begin-date end-date
     `((headers . ,(cfw:view-model-make-day-names-for-days begin-date end-date)) ; a list of the index of day-of-week
       (days . ,(cfw:view-model-make-days ; a list of days, which corresponds to the index of `headers'
                 begin-date end-date))))))



;;; view-month

(defun cfw:view-month-model (model)
  "[internal] Create a logical view model of monthly calendar.
This function collects and arranges contents.  This function does
not know how to display the contents in the destinations."
  (let* ((init-date (cfw:k 'init-date model))
         (year (calendar-extract-year init-date))
         (month (calendar-extract-month init-date))
         (begin-date (cfw:date month 1 year))
         (end-date (cfw:date month (calendar-last-day-of-month month year) year)))
    ;; model
    (append
     (cfw:view-model-make-common-data-for-weeks model begin-date end-date)
     `((month . ,month) (year . ,year)))))

(defun cfw:round-cell-width (width)
  "[internal] If string-width of `cfw:fchar-horizontal-line' is not 1,
this function re-calculate and return the adjusted width."
  (cond
   ((eql (char-width cfw:fchar-horizontal-line) 1) width)
   (t (- width (% width (char-width cfw:fchar-horizontal-line))))))

(defun cfw:view-month-calc-param (dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 7, footer 1, margin 2 => 15
       (win-height (max 15 (- (cfw:dest-height dest) 15)))
       (junctions-width (* (char-width cfw:fchar-junction) 8)) ; weekdays+1
       (cell-width  (cfw:round-cell-width 
                     (max 5 (/ (- win-width junctions-width) 7)))) ; weekdays
       (cell-height (max 2 (/ win-height 6))) ; max weeks = 6
       (total-width (+ (* cell-width cfw:week-days) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,cfw:week-days))))

(defun cfw:view-month (component)
  "[internal] Render monthly calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (cfw:render-append-parts (cfw:view-month-calc-param dest)))
         (total-width (cfw:k 'total-width param))
         (EOL (cfw:k 'eol param)) (VL (cfw:k 'vl param))
         (hline (cfw:k 'hline param)) (cline (cfw:k 'cline param))
         (model (cfw:view-month-model (cfw:component-model component))))
    ;; update model
    (setf (cfw:component-model component) model)
    ;; header
    (insert
     (cfw:rt (cfw:render-title-month (cfw:k 'init-date model))
             'cfw:face-title)
     EOL (cfw:render-toolbar total-width 'month
                             'cfw:navi-previous-month-command
                             'cfw:navi-next-month-command)
     EOL hline)
    ;; day names
    (cfw:render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (let ((year (cfw:k 'year model))
          (month (cfw:k 'month model)))
      (cfw:render-calendar-cells-weeks
       model param
       (lambda (date week-day hday)
         (cfw:rt
          (format "%s" (calendar-extract-day date))
          (cond
           (hday 'cfw:face-sunday)
           ((not (cfw:month-year-contain-p month year date)) 'cfw:face-disable)
           (t (cfw:render-get-week-face week-day 'cfw:face-default-day)))))))
    ;; footer
    (insert (cfw:render-footer total-width (cfw:model-get-contents-sources model)))))



;;; view-week

(defun cfw:view-week-model (model)
  "[internal] Create a logical view model of weekly calendar.
This function collects and arranges contents.  This function does
not know how to display the contents in the destinations."
  (let* ((init-date (cfw:k 'init-date model))
         (begin-date (cfw:week-begin-date init-date))
         (end-date (cfw:week-end-date init-date)))
    (cfw:view-model-make-common-data-for-weeks model begin-date end-date)))

;; (cfw:view-week-model (cfw:model-abstract-new (cfw:date 1 1 2011) nil nil))

(defun cfw:view-week-calc-param (dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (cfw:dest-height dest) 10)))
       (junctions-width (* (char-width cfw:fchar-junction) 8))
       (cell-width  (cfw:round-cell-width 
                     (max 5 (/ (- win-width junctions-width) 7))))
       (cell-height (max 2 win-height))
       (total-width (+ (* cell-width cfw:week-days) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,cfw:week-days))))

(defun cfw:view-week (component)
  "[internal] Render weekly calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (cfw:render-append-parts (cfw:view-week-calc-param dest)))
         (total-width (cfw:k 'total-width param))
         (EOL (cfw:k 'eol param)) (VL (cfw:k 'vl param))
         (hline (cfw:k 'hline param)) (cline (cfw:k 'cline param))
         (model (cfw:view-week-model (cfw:component-model component)))
         (begin-date (cfw:k 'begin-date model))
         (end-date (cfw:k 'end-date model)))
    ;; update model
    (setf (cfw:component-model component) model)
    ;; header
    (insert
     (cfw:rt
      (cfw:render-title-period begin-date end-date)
      'cfw:face-title)
     EOL (cfw:render-toolbar total-width 'week
                             'cfw:navi-previous-week-command
                             'cfw:navi-next-week-command)
     EOL hline)
    ;; day names
    (cfw:render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (cfw:render-calendar-cells-weeks
     model param
     (lambda (date week-day hday)
       (cfw:rt (format "%s" (calendar-extract-day date))
               (if hday 'cfw:face-sunday
                 (cfw:render-get-week-face
                  week-day 'cfw:face-default-day)))))
    ;; footer
    (insert (cfw:render-footer total-width (cfw:model-get-contents-sources model)))))



;;; view-two-weeks

(defun cfw:view-two-weeks-model-adjust (model)
  "view-two-weeks-model-begin
MODEL"
  (let ((in-date (cfw:k 'init-date model)))
    (cond
     ((eq 'two-weeks (cfw:k 'type model))
      (let ((old-begin-date (cfw:k 'begin-date model))
            (old-end-date (cfw:k 'end-date model)))
        (cond
         ((cfw:date-between old-begin-date old-end-date in-date)
          in-date)
         ((cfw:date-between old-end-date (cfw:date-after old-end-date cfw:week-days) in-date)
          old-end-date)
         ((cfw:date-between (cfw:date-after old-begin-date (- cfw:week-days)) old-begin-date in-date)
          (cfw:date-after old-begin-date (- cfw:week-days)))
         (t in-date))))
     (t in-date))))

(defun cfw:view-two-weeks-model (model)
  "[internal] Create a logical view model of two-weeks calendar.
This function collects and arranges contents.  This function does
not know how to display the contents in the destinations."
  (let* ((init-date (cfw:view-two-weeks-model-adjust model))
         (begin-date (cfw:week-begin-date init-date))
         (end-date (cfw:date-after begin-date (1- (* 2 cfw:week-days)))))
    ;; model
    (append
     (cfw:view-model-make-common-data-for-weeks model begin-date end-date)
     `((type . two-weeks)))))

;; (cfw:view-two-weeks-model (cfw:model-abstract-new (cfw:date 1 1 2011) nil nil))

(defun cfw:view-two-weeks-calc-param (dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 3, footer 1, margin 2 => 11
       (win-height (max 15 (- (cfw:dest-height dest) 11)))
       (junctions-width (* (char-width cfw:fchar-junction) 8))
       (cell-width  (cfw:round-cell-width 
                     (max 5 (/ (- win-width junctions-width) 7))))
       (cell-height (max 2 (/ win-height 2)))
       (total-width (+ (* cell-width cfw:week-days) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,cfw:week-days))))

(defun cfw:view-two-weeks (component)
  "[internal] Render two-weeks calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (cfw:render-append-parts (cfw:view-two-weeks-calc-param dest)))
         (total-width (cfw:k 'total-width param))
         (EOL (cfw:k 'eol param)) (VL (cfw:k 'vl param))
         (hline (cfw:k 'hline param)) (cline (cfw:k 'cline param))
         (model (cfw:view-two-weeks-model (cfw:component-model component)))
         (begin-date (cfw:k 'begin-date model))
         (end-date (cfw:k 'end-date model)))
    ;; update model
    (setf (cfw:component-model component) model)
    ;; header
    (insert
     (cfw:rt
      (cfw:render-title-period begin-date end-date)
      'cfw:face-title)
     EOL (cfw:render-toolbar total-width 'two-weeks
                             'cfw:navi-previous-week-command
                             'cfw:navi-next-week-command)
     EOL hline)
    ;; day names
    (cfw:render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (cfw:render-calendar-cells-weeks
     model param
     (lambda (date week-day hday)
       (cfw:rt (format "%s" (calendar-extract-day date))
               (if hday 'cfw:face-sunday
                 (cfw:render-get-week-face
                  week-day 'cfw:face-default-day)))))
    ;; footer
    (insert (cfw:render-footer total-width (cfw:model-get-contents-sources model)))))



;;; view-day

(defun cfw:view-day-calc-param (dest &optional num)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((num (or num 1))
       (win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (cfw:dest-height dest) 10)))
       (junctions-width (* (char-width cfw:fchar-junction) (1+ num)))
       (cell-width  (cfw:round-cell-width 
                     (max 3 (/ (- win-width junctions-width) num))))
       (cell-height win-height)
       (total-width (+ (* cell-width num) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,num))))

(defun cfw:view-day (component)
  "[internal] Render daily calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (cfw:render-append-parts (cfw:view-day-calc-param dest)))
         (total-width (cfw:k 'total-width param))
         (EOL (cfw:k 'eol param)) (VL (cfw:k 'vl param))
         (hline (cfw:k 'hline param)) (cline (cfw:k 'cline param))
         (current-date (cfw:k 'init-date (cfw:component-model component)))
         (model
          (cfw:view-model-make-common-data-for-days
           (cfw:component-model component) current-date current-date)))
    ;; update model
    (setf (cfw:component-model component) model)
    ;; header
    (insert
     (cfw:rt
      (cfw:render-title-day current-date)
      'cfw:face-title)
     EOL (cfw:render-toolbar total-width 'day
                             'cfw:navi-previous-day-command
                             'cfw:navi-next-day-command)
     EOL hline)
    ;; day names
    (cfw:render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (cfw:render-calendar-cells-days
     model param
     (lambda (date week-day hday)
       (cfw:rt (format "%s" (calendar-extract-day date))
               (if hday 'cfw:face-sunday
                 (cfw:render-get-week-face
                  week-day 'cfw:face-default-day)))))
    ;; footer
    (insert (cfw:render-footer total-width (cfw:model-get-contents-sources model)))))

(defun cfw:render-calendar-cells-days (model param title-func)
  "[internal] Insert calendar cells for the linear views."
  (cfw:render-columns
   (loop with cell-width = (cfw:k 'cell-width param)
         with days = (cfw:k 'days model)
         for date in days ; days columns loop
         for count from 0 below (length days)
         for hday = (car (cfw:contents-get date (cfw:k 'holidays model)))
         for week-day = (nth count (cfw:k 'headers  model))
         for ant = (cfw:rt (cfw:contents-get
                            date (cfw:k 'annotations model))
                           'cfw:face-annotation)
         for raw-periods = (cfw:contents-get
                            date (cfw:render-periods-stacks model))
         for raw-contents = (cfw:render-sort-contents
                             (cfw:contents-get
                              date (cfw:k 'contents model)))
         for prs-contents = (append
                             (cfw:render-periods-days
                              date raw-periods cell-width)
                             (mapcar 'cfw:render-default-content-face raw-contents))
         for num-label = (if prs-contents
                             (format "(%s)"
                                     (+ (length raw-contents)
                                        (length raw-periods))) "")
         for tday = (concat
                     " " ; margin
                     (funcall title-func date week-day hday)
                     (if num-label (concat " " num-label))
                     (if hday (concat " " (cfw:rt (substring hday 0)
                                                  'cfw:face-holiday))))
         collect
         (cons date (cons (cons tday ant) prs-contents)))
   param))

(defun cfw:render-periods-days (date periods-stack cell-width)
  "[internal] Insert period texts."
  (when periods-stack
    (let ((stack (sort (copy-sequence periods-stack)
                       (lambda (a b) (< (car a) (car b))))))
      (loop for (row (begin end content)) in stack
            for beginp = (equal date begin)
            for endp = (equal date end)
            for width = (- cell-width 2)
            for title = (cfw:render-truncate
                         (concat
                          (cfw:strtime begin) " - "
                          (cfw:strtime end) " : "
                          content) width t)
            collect
            (if content
                (cfw:rt
                 (concat
                  (if beginp "(" " ")
                  (cfw:render-left width title ?-)
                  (if endp ")" " "))
                 (cfw:render-get-face-period content 'cfw:face-periods))
              "")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation

;; Following functions assume that the current buffer is a calendar view.

(defun cfw:cursor-to-date (&optional pos)
  "[internal] Return the date at the cursor. If the text does not
have the text-property `cfw:date', return nil."
  (get-text-property (or pos (point)) 'cfw:date))

(defun cfw:cursor-to-nearest-date ()
  "Return the date at the cursor. If the point of cursor does not
have the date, search the date around the cursor position. If the
current buffer is not calendar view (it may be bug), this
function may return nil."
  (or (cfw:cursor-to-date)
      (let* ((r (lambda () (when (not (eolp)) (forward-char))))
             (l (lambda () (when (not (bolp)) (backward-char))))
             (u (lambda () (when (not (bobp)) (line-move 1))))
             (d (lambda () (when (not (eobp)) (line-move -1)))) get)
        (setq get (lambda (cmds)
                    (save-excursion
                      (if (null cmds) (cfw:cursor-to-date)
                        (ignore-errors
                          (funcall (car cmds)) (funcall get (cdr cmds)))))))
        (or (loop for i in `((,d) (,r) (,u) (,l)
                             (,d ,r) (,d ,l) (,u ,r) (,u ,l)
                             (,d ,d) (,r ,r) (,u ,u) (,l ,l))
                  for date = (funcall get i)
                  if date return date)
            (cond
             ((> (/ (point-max) 2) (point))
              (cfw:find-first-date))
             (t (cfw:find-last-date)))))))

(defun cfw:find-first-date ()
  "[internal] Return the first date in the current buffer."
  (let ((pos (next-single-property-change (point-min) 'cfw:date)))
    (and pos (cfw:cursor-to-date pos))))

(defun cfw:find-last-date ()
  "[internal] Return the last date in the current buffer."
  (let ((pos (previous-single-property-change (point-max) 'cfw:date)))
    (and pos (cfw:cursor-to-date (1- pos)))))

(defun cfw:find-by-date (date)
  "[internal] Return a point where the text property `cfw:date'
is equal to DATE in the current calender view. If DATE is not
found in the current view, return nil."
  (let ((pos (point-min)) begin ret text-date)
    (while (setq begin (next-single-property-change pos 'cfw:date))
      (setq pos begin
            text-date (cfw:cursor-to-date begin))
      (when (and text-date (equal date text-date))
        (setq ret begin
              pos (point-max))))
    ret))

(defun cfw:find-all-by-date (date func)
  "[internal] Call the function FUNC in each regions where the
text-property `cfw:date' is equal to DATE. The function FUNC
receives two arguments, begin date and end one. This function is
mainly used at functions for putting overlays."
  (let ((pos (point-min)) begin text-date)
    (while (setq begin (next-single-property-change pos 'cfw:date))
      (setq text-date (cfw:cursor-to-date begin))
      (when (and text-date (equal date text-date))
        (let ((end (next-single-property-change
                    begin 'cfw:date nil (point-max))))
          (funcall func begin end)))
      (setq pos begin))))

(defun cfw:navi-goto-date (date)
  "Move the cursor to DATE and put selection. If DATE is not
included on the current calendar, this function changes the
calendar view."
  (let ((cp (cfw:cp-get-component)))
    (when cp
      (cfw:cp-set-selected-date cp date))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major Mode / Key bindings

(defvar cfw:calendar-mode-map
  (cfw:define-keymap
   '(
     ("<right>" . cfw:navi-next-day-command)
     ("f"       . cfw:navi-next-day-command)
     ("<left>"  . cfw:navi-previous-day-command)
     ("b"       . cfw:navi-previous-day-command)
     ("<down>"  . cfw:navi-next-week-command)
     ("n"       . cfw:navi-next-week-command)
     ("<up>"    . cfw:navi-previous-week-command)
     ("p"       . cfw:navi-previous-week-command)

     ;; Vi style
     ("l" . cfw:navi-next-day-command)
     ("h" . cfw:navi-previous-day-command)
     ("j" . cfw:navi-next-week-command)
     ("k" . cfw:navi-previous-week-command)
     ("^" . cfw:navi-goto-week-begin-command)
     ("$" . cfw:navi-goto-week-end-command)

     ("<"   . cfw:navi-previous-month-command)
     ("M-v" . cfw:navi-previous-month-command)
     (">"   . cfw:navi-next-month-command)
     ("C-v" . cfw:navi-next-month-command)
     ("<prior>" . cfw:navi-previous-month-command)
     ("<next>"  . cfw:navi-next-month-command)
     ("<home>"  . cfw:navi-goto-first-date-command)
     ("<end>"   . cfw:navi-goto-last-date-command)

     ("g" . cfw:navi-goto-date-command)
     ("t" . cfw:navi-goto-today-command)

     ("r"   . cfw:refresh-calendar-buffer)
     ("SPC" . cfw:show-details-command)

     ("D" . cfw:change-view-day)
     ("W" . cfw:change-view-week)
     ("T" . cfw:change-view-two-weeks)
     ("M" . cfw:change-view-month)

     ([mouse-1] . cfw:navi-on-click)

     ("q" . bury-buffer)
     )) "Default key map of calendar views.")

(defun cfw:calendar-mode-map (&optional custom-map)
  "[internal] Return a keymap object for the calendar buffer."
  (cond
   (custom-map
    (set-keymap-parent custom-map cfw:calendar-mode-map)
    custom-map)
   (t cfw:calendar-mode-map)))

(defvar cfw:calendar-mode-hook nil
  "This hook is called at end of setting up major mode `cfw:calendar-mode'.")

(defun cfw:calendar-mode (&optional custom-map)
  "Set up major mode `cfw:calendar-mode'.

\\{cfw:calendar-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map (cfw:calendar-mode-map custom-map))
  (setq major-mode 'cfw:calendar-mode
        mode-name "Calendar Mode")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'cfw:calendar-mode-hook))

;;; Actions

(defun cfw:change-view-month ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'month)))

(defun cfw:change-view-week ()
  "change-view-week"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'week)))

(defun cfw:change-view-two-weeks ()
  "change-view-two-weeks"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'two-weeks)))

(defun cfw:change-view-day ()
  "change-view-day"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'day)))

(defun cfw:navi-on-click ()
  "click"
  (interactive)
  (let ((cp (cfw:cp-get-component))
        (date (cfw:cursor-to-date)))
    (when (and cp date)
      (cfw:cp-set-selected-date cp date)
      (cfw:cp-fire-click-hooks cp))))

(defun cfw:refresh-calendar-buffer ()
  "Clear the calendar and render again."
  (interactive)
  (let ((cp (cfw:cp-get-component)))
    (when cp
      (loop for s in (cfw:cp-get-contents-sources cp)
            for f = (cfw:source-update s)
            if f do (funcall f))
      (loop for s in (cfw:cp-get-annotation-sources cp)
            for f = (cfw:source-update s)
            if f do (funcall f))
      (cfw:cp-update cp))))

(defun cfw:navi-goto-week-begin-command ()
  "Move the cursor to the first day of the current week."
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:navi-goto-date
     (cfw:week-begin-date
      (cfw:cp-get-selected-date (cfw:cp-get-component))))))

(defun cfw:navi-goto-week-end-command ()
  "Move the cursor to the last day of the current week."
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:navi-goto-date
     (cfw:week-end-date
      (cfw:cp-get-selected-date (cfw:cp-get-component))))))

(defun cfw:navi-goto-date-command (string-date)
  "Move the cursor to the specified date."
  (interactive "sInput Date (YYYY/MM/DD): ")
  (cfw:navi-goto-date (cfw:parsetime string-date)))

(defun cfw:navi-goto-today-command ()
  "Move the cursor to today."
  (interactive)
  (cfw:navi-goto-date (cfw:emacs-to-calendar (current-time))))

(defun cfw:navi-next-day-command (&optional num)
  "Move the cursor forward NUM days. If NUM is nil, 1 is used.
Moves backward if NUM is negative."
  (interactive)
  (when (cfw:cp-get-component)
    (unless num (setq num 1))
    (let* ((cursor-date (cfw:cp-get-selected-date (cfw:cp-get-component)))
           (new-cursor-date (cfw:date-after cursor-date num)))
      (cfw:navi-goto-date new-cursor-date))))

(defun cfw:navi-previous-day-command (&optional num)
  "Move the cursor back NUM days. If NUM is nil, 1 is used.
Moves forward if NUM is negative."
  (interactive)
  (cfw:navi-next-day-command (- (or num 1))))

(defun cfw:navi-goto-first-date-command ()
  "Move the cursor to the first day on the current calendar view."
  (interactive)
  (cfw:navi-goto-date (cfw:find-first-date)))

(defun cfw:navi-goto-last-date-command ()
  "Move the cursor to the last day on the current calendar view."
  (interactive)
  (cfw:navi-goto-date (cfw:find-last-date)))

(defun cfw:navi-next-week-command (&optional num)
  "Move the cursor forward NUM weeks. If NUM is nil, 1 is used.
Moves backward if NUM is negative."
  (interactive)
  (cfw:navi-next-day-command (* cfw:week-days (or num 1))))

(defun cfw:navi-previous-week-command (&optional num)
  "Move the cursor back NUM weeks. If NUM is nil, 1 is used.
Moves forward if NUM is negative."
  (interactive)
  (cfw:navi-next-day-command (* (- cfw:week-days) (or num 1))))

(defun cfw:navi-next-month-command (&optional num)
  "Move the cursor forward NUM months. If NUM is nil, 1 is used.
Movement is backward if NUM is negative."
  (interactive)
  (when (cfw:cp-get-component)
    (unless num (setq num 1))
    (let* ((cursor-date (cfw:cp-get-selected-date (cfw:cp-get-component)))
           (month (calendar-extract-month cursor-date))
           (day   (calendar-extract-day   cursor-date))
           (year  (calendar-extract-year  cursor-date))
           (last (progn
                   (calendar-increment-month month year num)
                   (calendar-last-day-of-month month year)))
           (day (min last day))
           (new-cursor-date (cfw:date month day year)))
      (cfw:navi-goto-date new-cursor-date))))

(defun cfw:navi-previous-month-command (&optional num)
  "Move the cursor back NUM months. If NUM is nil, 1 is used.
Movement is forward if NUM is negative."
  (interactive)
  (cfw:navi-next-month-command (- (or num 1))))

;;; Detail popup

(defun cfw:show-details-command ()
  "Show details on the selected date."
  (interactive)
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (cp  (cfw:cp-get-component))
         (model (and cp (cfw:component-model cp))))
    (when model
      (cfw:details-popup
       (cfw:details-layout cursor-date model)))))

(defvar cfw:details-buffer-name "*cfw:details*" "[internal]")
(defvar cfw:details-window-size 20 "Default detail buffer window size.")

(defun cfw:details-popup (text)
  "Popup the buffer to show details.
TEXT is a content to show."
  (let ((buf (get-buffer cfw:details-buffer-name))
        (before-win-num (length (window-list)))
        (main-buf (current-buffer)))
    (unless (and buf (eq (buffer-local-value 'major-mode buf)
                         'cfw:details-mode))
      (setq buf (get-buffer-create cfw:details-buffer-name))
      (with-current-buffer buf
        (cfw:details-mode)
        (set (make-local-variable 'cfw:before-win-num) before-win-num)))
    (with-current-buffer buf
      (let (buffer-read-only)
        (set (make-local-variable 'cfw:main-buf) main-buf)
        (erase-buffer)
        (insert text)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun cfw:details-layout (date model)
  "Layout details and return the text.
DATE is a date to show. MODEL is model object."
  (let* ((EOL "\n")
         (HLINE (cfw:rt (concat (make-string (window-width) ?-) EOL) 'cfw:face-grid))
         (holiday (cfw:model-get-holiday-by-date date model))
         (annotation (cfw:model-get-annotation-by-date date model))
         (periods (cfw:model-get-periods-by-date date model))
         (contents (cfw:model-get-contents-by-date date model)))
  (concat
   (cfw:rt (concat "Schedule on " (cfw:strtime date) " (") 'cfw:face-header)
   (cfw:rt (calendar-day-name date)
           (cfw:render-get-week-face (calendar-day-of-week date) 'cfw:face-header))
   (cfw:rt (concat ")" EOL) 'cfw:face-header)
   (when (or holiday annotation)
     (concat
      (and holiday (cfw:rt holiday 'cfw:face-holiday))
      (and holiday annotation " / ")
      (and annotation (cfw:rt annotation 'cfw:face-annotation))
      EOL))
   HLINE
   (loop for (begin end summary) in periods
         for prefix = (cfw:rt
                       (concat (cfw:strtime begin) " - " (cfw:strtime end) " : ")
                       (cfw:render-get-face-period summary 'cfw:face-periods))
         concat
         (concat prefix " " summary EOL))
   (loop for i in contents concat
         (concat "- " (cfw:rt i (cfw:render-get-face-content i 'cfw:face-default-content))
                 EOL)))))

(defvar cfw:details-mode-map
  (cfw:define-keymap
   '(("q"   . cfw:details-kill-buffer-command)
     ("SPC" . cfw:details-kill-buffer-command)
     ("n"   . cfw:details-navi-next-command)
     ("f"   . cfw:details-navi-next-command)
     ("<right>"  . cfw:details-navi-next-command)
     ("p"   . cfw:details-navi-prev-command)
     ("b"   . cfw:details-navi-prev-command)
     ("<left>"   . cfw:details-navi-prev-command)
     ))
  "Default key map for the details buffer.")

(defvar cfw:details-mode-hook nil "")

(defun cfw:details-mode ()
  "Set up major mode `cfw:details-mode'.

\\{cfw:details-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map cfw:details-mode-map)
  (setq major-mode 'cfw:details-mode
        mode-name "Calendar Details Mode")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'cfw:details-mode-hook))

(defun cfw:details-kill-buffer-command ()
  "Kill buffer and delete window."
  (interactive)
  (let ((win-num (length (window-list)))
        (next-win (get-buffer-window cfw:main-buf)))
    (when (and (not (one-window-p))
               (> win-num cfw:before-win-num))
      (delete-window))
    (kill-buffer cfw:details-buffer-name)
    (when next-win (select-window next-win))))

(defun cfw:details-navi-next-command (&optional num)
  (interactive)
  (when cfw:main-buf
    (with-current-buffer cfw:main-buf
      (cfw:navi-next-day-command num)
      (cfw:show-details-command))))

(defun cfw:details-navi-prev-command (&optional num)
  (interactive)
  (when cfw:main-buf
    (with-current-buffer cfw:main-buf
      (cfw:navi-previous-day-command num)
      (cfw:show-details-command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High level API

;; buffer

(defun* cfw:open-calendar-buffer
    (&key date buffer custom-map contents-sources annotation-sources view)
  "Open a calendar buffer simply.
DATE is initial focus date. If it is nil, today is selected
initially.  This function uses the function
`cfw:create-calendar-component-buffer' internally."
  (interactive)
  (let ((cp (cfw:create-calendar-component-buffer
             :date date :contents-sources contents-sources
             :annotation-sources annotation-sources :view view)))
    (switch-to-buffer (cfw:cp-get-buffer cp))))

(defun* cfw:create-calendar-component-buffer
    (&key date buffer custom-map contents-sources annotation-sources view)
  "Return a calendar buffer with some customize parameters.

This function binds the component object at the
buffer local variable `cfw:component'.

The size of calendar is calculated from the window that shows
BUFFER or the selected window.
DATE is initial focus date. If it is nil, today is selected initially.
BUFFER is the buffer to be rendered. If BUFFER is nil, this function creates a new buffer named `cfw:calendar-buffer-name'.
CUSTOM-MAP is the additional keymap that is added to default keymap `cfw:calendar-mode-map'."
  (let* ((dest  (cfw:dest-init-buffer buffer nil nil custom-map))
         (model (cfw:model-abstract-new date contents-sources annotation-sources))
         (cp (cfw:cp-new dest model view date)))
    (with-current-buffer (cfw:dest-buffer dest)
      (set (make-local-variable 'cfw:component) cp))
    cp))

;; region

(defun* cfw:create-calendar-component-region
    (&key date width height keymap contents-sources annotation-sources view)
  "Insert markers of the rendering destination at current point and display the calendar view.

This function returns a component object and stores it at the text property `cfw:component'.

DATE is initial focus date. If it is nil, today is selected initially.
WIDTH and HEIGHT are reference size of the calendar view. If those are nil, the size is calculated from the selected window.
KEYMAP is the keymap that is put to the text property `keymap'. If KEYMAP is nil, `cfw:calendar-mode-map' is used."
  (let (mark-begin mark-end)
    (setq mark-begin (point-marker))
    (insert " ")
    (setq mark-end (point-marker))
    (save-excursion
      (let* ((dest (cfw:dest-init-region (current-buffer) mark-begin mark-end width height))
             (model (cfw:model-abstract-new date contents-sources annotation-sources))
             (cp (cfw:cp-new dest model view date))
             (after-update-func
              (lexical-let ((keymap keymap) (cp cp))
                (lambda ()
                  (cfw:dest-with-region (cfw:component-dest cp)
                    (let (buffer-read-only)
                      (put-text-property (point-min) (1- (point-max))
                                         'cfw:component cp)
                      (cfw:fill-keymap-property
                       (point-min) (1- (point-max)) (or keymap cfw:calendar-mode-map))))))))
        (setf (cfw:dest-after-update-func dest) after-update-func)
        (funcall after-update-func)
        cp))))

(defun cfw:fill-keymap-property (begin end keymap)
  "[internal] Put the given text property to the region between BEGIN and END.
If the text already has some keymap property, the text is skipped."
  (save-excursion
    (goto-char begin)
    (loop with pos = begin with nxt = nil
          until (or (null pos) (<= end pos))
          when (get-text-property pos 'keymap) do
          (setq pos (next-single-property-change pos 'keymap))
          else do
          (setq nxt (next-single-property-change pos 'keymap))
          (when (null nxt) (setq nxt end))
          (put-text-property pos (min nxt end) 'keymap keymap))))

;; inline

(defun* cfw:get-calendar-text
    (width height &key date keymap contents-sources annotation-sources view)
  "Return a text that is drew the calendar view.

In this case, the rendering destination object is disposable.

WIDTH and HEIGHT are reference size of the calendar view.  If the
given size is larger than the minimum size (about 45x20), the
calendar is displayed within the given size. If the given size is
smaller, the minimum size is used.

DATE is initial focus date. If it is nil, today is selected initially."
  (let* ((dest (cfw:dest-init-inline width height))
         (model (cfw:model-abstract-new date contents-sources annotation-sources))
         (cp (cfw:cp-new dest model view date))
         text)
    (setq text
          (with-current-buffer (cfw:cp-get-buffer cp)
            (buffer-substring (point-min) (point-max))))
    (kill-buffer (cfw:cp-get-buffer cp))
    text))



;;; debug

(defun cfw:open-debug-calendar ()
  (let* ((source1
          (make-cfw:source
           :name "test1"
           :color "Lightpink3"
           :period-bgcolor "Lightpink1"
           :period-fgcolor "White"
           :opt-face '(:weight bold)
           :opt-period-face '(:slant italic)
           :data
           (lambda (b e)
             '(((1  1 2011) "TEST1")
               ((1 10 2011) "TEST2" "TEST3")
               (periods
                ((1 8 2011) (1 9 2011) "PERIOD1")
                ((1 11 2011) (1 12 2011) "Period2")
                ((1 12 2011) (1 14 2011) "long long title3"))
               ))
           :update
           (lambda () (message "SOURCE: test1 update!"))))
         (source2
          (make-cfw:source
           :name "test2"
           :data
           (lambda (b e)
             '(((1  2 2011) "PTEST1")
               ((1 10 2011) "PTEST2" "PTEST3")
               (periods
                ((1 14 2011) (1 15 2011) "Stack")
                ((1 29 2011) (1 31 2011) "PERIOD W"))
               ))))
         (asource1
          (make-cfw:source
           :name "Moon"
           :data
           (lambda (b e)
             '(((1  4 2011) . "New Moon")
               ((1 12 2011) . "Young Moon")
               ((1 20 2011) . "Full Moon")
               ((1 26 2011) . "Waning Moon")
               ))))
         (asource2
          (make-cfw:source
           :name "Moon"
           :data
           (lambda (b e)
             '(((1  5 2011) . "AN1")
               ((1 13 2011) . "AN2")
               ((1 20 2011) . "AN3")
               ((1 28 2011) . "AN4")
               ))))
         (cp (cfw:create-calendar-component-buffer
              :view 'month
              :contents-sources (list source1 source2)
              :annotation-sources (list asource1 asource2))))
    (cfw:cp-add-update-hook cp (lambda () (message "CFW: UPDATE HOOK")))
    (cfw:cp-add-click-hook cp (lambda () (message "CFW: CLICK HOOK %S" (cfw:cursor-to-nearest-date))))
    (cfw:cp-add-selection-change-hook cp (lambda () (message "CFW: SELECT %S" (cfw:cursor-to-nearest-date))))
    (switch-to-buffer (cfw:cp-get-buffer cp))
    ))

(provide 'calfw)
;;; calfw.el ends here

;; (progn (eval-current-buffer) (cfw:open-debug-calendar))
;; (progn (eval-current-buffer) (cfw:open-calendar-buffer))
