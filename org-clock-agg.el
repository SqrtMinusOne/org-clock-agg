;;; org-clock-agg.el --- Tree-like reports for org-clock records -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (compat "29.1.4.1") (org-ql "0.8-pre"))
;; Homepage: https://github.com/SqrtMinusOne/org-clock-agg
;; Published-At: 2023-12-17

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Aggregate org-clock records and display the results in an
;; interactive buffer.  org-clock records are grouped by predicates
;; such as file name, their outline path in the file, etc.  Each
;; record is placed in a tree structure; each node of the tree shows
;; the total time spent in that node and its children.  The top-level
;; node shows the total time spent in all records found by the query.
;;
;; `org-clock-agg' is the main entrypoint.  It can be run interactively
;; or from elisp code.  See the docstring for details.
;;
;; See also the README at
;; <https://github.com/SqrtMinusOne/org-clock-agg> for more details.

;;; Code:
(require 'cl-lib)
(require 'font-lock)
(require 'outline)
(require 'org)
(require 'seq)
(require 'widget)

(require 'compat)
(require 'org-ql)

(eval-when-compile
  (require 'org-ql-view))

;; XXX byte-compiler on 29.4 started to want this, don't know why
(defvar widget-push-button-prefix)
(defvar widget-push-button-suffix)

(defgroup org-clock-agg nil
  "Aggregate org-clock statistics."
  :group 'org-clock)

(defcustom org-clock-agg-duration-format "%h:%.2m"
  "Format string for durations in `org-clock-agg' views.

Refer to `format-seconds' for the available format specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-files-preset nil
  "Presets for the \"files\" parameter in `org-clock-agg' views.

The variable is an alist with preset names as keys and lists of
file names as values."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'org-clock-agg)

(defcustom org-clock-agg-day-format "%Y-%m-%d, %a"
  "Format string for days in `org-clock-agg' views.

Refer to `format-time-string' for the available format
specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-week-format "%Y-%W"
  "Format string for weeks in `org-clock-agg' views.

Refer to `format-time-string' for the list of available format
specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-month-format "%Y-%m"
  "Format string for months in `org-clock-agg' views.

Refer to `format-time-string' for the list of available format
specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-properties nil
  "Org properties to include in `org-clock-agg' views.

The value of this variable should be a list of strings.

Set this interactively or manually reset `org-ql-cache' and
`org-ql-node-value-cache' after modification."

  :type '(repeat string)
  :group 'org-clock-agg
  :set (lambda (&rest _)
         (setq org-ql-cache (make-hash-table :weakness 'key))
         (setq org-ql-node-value-cache (make-hash-table :weakness 'key))))

(defcustom org-clock-agg-node-title-width-delta 40
  "How many characters to truncate from the node title.

Refer to `org-clock-agg-node-format' for instructions on configuring
this."
  :type 'integer
  :group 'org-clock-agg)

(defcustom org-clock-agg-node-format "%-%(+ title-width)t %20c %8z"
  "Format string for the node title in `org-clock-agg' views.

The following format specifiers are available:
- %t - node title with the level prefix, truncated to `title-width'
   characters (see below)
- %c - name of the grouping function that produced the node (the
   `:readable-name' parameter)
- %z - time spent in the node according to
   `org-clock-agg-duration-format'
- %S - time share of the node against the parent node
- %s - time share of the node against the top-level node

Refer to `format-spec' for available modifiers.

This format string also evaluates Elisp expressions in the %(...)
blocks.  During evaluation, the following variables are bound:
- `title-width': `window-width' minus
  `org-clock-agg-node-title-width-delta'.

This way, in the default configuration, the node title is truncated to
fit into the window, utilizing `org-clock-agg-node-title-width-delta'
as the remaining characters for the rest of the string."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-elem-format "- [%s]--[%e] => %d : %t"
  "Format string for elements in `org-clock-agg' views.

The following format specifiers are available:
- %s - start of the time range
- %e - end of the time range
- %d - duration of the time range
- %t - title of the record.

The formats of %s and %e are controlled by `org-time-stamp-formats'."
  :type 'string
  :group 'org-clock-agg)

(defconst org-clock-agg--extra-params-default
  '(("Show records:" . (checkbox :extras-key :show-elems))
    ("Add \"Ungrouped\"" . (checkbox :extras-key :add-ungrouped)))
  "Default set of extra parameters for `org-clock-agg' views.")

(defcustom org-clock-agg-extra-params nil
  "Extra parameters for `org-clock-agg' views."
  :type '(alist :key-type string :value-type sexp)
  :group 'org-clock-agg)

(defface org-clock-agg-group-face
  '((t :inherit font-lock-comment-face))
  "Face for group names in `org-clock-agg' tree views."
  :group 'org-clock-agg)

(defface org-clock-agg-share-face
  '((t :inherit font-lock-comment-face))
  "Face for node time share values in `org-clock-agg' tree views."
  :group 'org-clock-agg)

(defface org-clock-agg-duration-face
  '((t :inherit font-lock-constant-face))
  "Face for durations in `org-clock-agg' tree views."
  :group 'org-clock-agg)

(defface org-clock-agg-param-face
  '((t :inherit font-lock-variable-name-face))
  "Face for parameters in `org-clock-agg' tree views."
  :group 'org-clock-agg)

(defface org-clock-agg-elem-face nil
  "Face for elements in `org-clock-agg' tree views.

It's probably supposed to be nil because it overrides the default
element formatting."
  :group 'org-clock-agg)

;; This function appears in Emacs 29 and isn't avaliable in `compat'
;; for some reason
(defun org-clock-agg--alist-to-plist (alist)
  "Convert ALIST to a plist."
  (let ((res '()))
    (dolist (x alist)
      (push (car x) res)
      (push (cdr x) res))
    (nreverse res)))

;;; Querying
(defun org-clock-agg--parse-clocks (headline)
  "Extract org-clock clocks from HEADLINE.

Return a list of alists with the following keys:
- `:start' - start time in seconds since the epoch
- `:end' - end time in seconds since the epoch
- `:duration' - duration in seconds."
  (save-restriction
    ;; I used to insert a substring into a separate buffer to run
    ;; `org-element-parse-buffer', but somehow this broke on the most
    ;; recent `org-mode'.
    (narrow-to-region
     (org-element-property :contents-begin headline)
     (org-element-property :contents-end headline))
    (let (res)
      (org-element-map (org-element-parse-buffer) 'clock
        (lambda (clock)
          (let ((start (time-convert
                        (org-timestamp-to-time (org-element-property :value clock))
                        'integer))
                (end (time-convert
                      (org-timestamp-to-time (org-element-property :value clock) t)
                      'integer)))
            (push
             `((:start . ,start)
               (:end . ,end)
               (:duration . ,(- end start)))
             res)))
        ;; The last argument stops parsing after the first headline.
        ;; So only clocks in the first headline are parsed.
        nil nil 'headline)
      res)))

(defun org-clock-agg--properties-at-point ()
  "Return a list of selected properties at point.

`org-clock-agg-properties' sets the list of properties to select.  The
properties are inherited from the parent headlines and from the global
properties set at the beginning of the file."
  (let ((global-props
         (org-ql--value-at
          1 (lambda ()
              (cl-loop for res in (org-collect-keywords org-clock-agg-properties)
                       collect (cons (nth 0 res) (nth 1 res))))))
        (local-props
         (org-ql--value-at
          (point)
          (lambda ()
            (cl-loop for key in org-clock-agg-properties
                     for val = (org-entry-get nil key t)
                     when val collect `(,key . ,val))))))
    (seq-uniq
     (append local-props global-props)
     (lambda (a b)
       (equal (car a) (car b))))))

(defun org-clock-agg--parse-headline ()
  "Parse headline at point.

Return a list of alists with the following keys:
- `:start' - start time in seconds since the epoch
- `:end' - end time in seconds since the epoch
- `:duration' - duration in seconds
- `:headline' - instance of org-element for the headline
- `:tags' - list of tags
- `:file' - file name
- `:outline-path' - list of outline path, i.e. all headlines from the
root to the current headline
- `:properties' - list of properties, `org-clock-agg-properties' sets the
list of properties to select
- `:category' - category of the current headline."
  (let* ((headline (org-element-headline-parser))
         (tags-val (org-ql--tags-at (point)))
         (tags (seq-filter
                #'stringp ;; to filter out `org-ql-nil'
                (append (unless (eq (car tags-val) 'org-ql-nil)
                          (car tags-val))
                        (unless (eq (cdr tags-val) 'org-ql-nil)
                          (cdr tags-val)))))
         (file (buffer-file-name))
         (outline-path (mapcar
                        #'substring-no-properties
                        (org-ql--outline-path)))
         (properties (when org-clock-agg-properties
                       (org-clock-agg--properties-at-point)))
         (category (org-get-category)))
    (org-ql--add-markers headline)
    (cl-loop for clock in (org-clock-agg--parse-clocks headline)
             collect`(,@clock
                      (:headline . ,headline)
                      (:tags . ,tags)
                      (:file . ,file)
                      (:outline-path . ,outline-path)
                      (:properties . ,properties)
                      (:category . ,category)))))

(defun org-clock-agg--normalize-time-predicate (val kind)
  "Normalize VAL to a time predicate.

VAL can be either:
- A number interpreted as a number of days from the current one.
- A string parseable by `parse-time-string', with or without the time
  part.

KIND is either \"from\" or \"to\".  If it's the latter, the time part
is set to 23:59:59 when possible; otherwise, it's set to 00:00:00.

The result is the number of seconds since the epoch."
  (when-let (int-val
             (and (stringp val) (ignore-errors (number-to-string val))))
    (setq val int-val))
  (cond ((numberp val)
         ;; Hmm, so that's why alpapapa loves ts, dash and whatnot...
         (+
          (time-convert
           (encode-time
            (append
             (if (eq kind 'to) '(59 59 23) '(0 0 0))
             (seq-drop (decode-time) 3)))
           'integer)
          (* val 24 60 60)))
        ((stringp val)
         (let ((res (parse-time-string val)))
           (setf (decoded-time-second res)
                 (or (decoded-time-second res) (if (eq kind 'to) 59 0))
                 (decoded-time-minute res)
                 (or (decoded-time-minute res) (if (eq kind 'to) 59 0))
                 (decoded-time-hour res)
                 (or (decoded-time-hour res) (if (eq kind 'to) 23 0)))
           (time-convert
            (encode-time res)
            'integer)))
        (t (user-error "Invalid time predicate: %s" val))))

(defun org-clock-agg--filter-elems (from to elems)
  "Filter ELEMS by FROM and TO.

Refer to `org-clock-agg--normalize-time-predicate' for the possible
values of FROM and TO.

ELEMS is a list as described in `org-clock-agg--parse-headline'."
  (let ((from-date (org-clock-agg--normalize-time-predicate from 'from))
        (to-date (org-clock-agg--normalize-time-predicate to 'to)))
    (cl-loop for elem in elems
             for start = (or (alist-get :start elem) 0)
             for end = (or (alist-get :end elem) (expt 2 32))
             when (and (>= start from-date)
                       (<= end to-date))
             collect elem)))

(defun org-clock-agg--query (from to files)
  "Query org files in FILES for clocked entries from FROM to TO.

Refer to `org-clock-agg--normalize-time-predicate' for the possible
values of FROM and TO.

Return a list as described in `org-clock-agg--parse-headline'."
  (if (org-clock-agg--drill-down-p)
      org-clock-agg--elems
    (thread-last
      (cl-loop for res in (org-ql-query
                            :select #'org-clock-agg--parse-headline
                            :from files
                            :where `(clocked :from ,from :to ,to))
               append res)
      (org-clock-agg--filter-elems from to))))

;;; Aggregation
(defvar org-clock-agg-groupby-functions nil
  "Group by functions for `org-clock-agg'.

This is an alist with function names as keys and alists containing the
following keys as values:
- `:function' - the grouping function itself
- `:hidden' - whether to hide the function in the UI
- `:readable-name' - name to display in the UI
- `:default-sort' - default sorting function to use for this group (a
  key of `org-clock-agg-sort-functions')

See `org-clock-agg-defgroupby' on how to define new grouping
functions.")

(defvar org-clock-agg-sort-functions nil
  "Sort functions for `org-clock-agg'.

This is an alist with function names as keys and alists with the
following keys as values:
- `:function' - the sorting function itself
- `:readable-name' - name to display in the UI.

See `org-clock-agg-defsort' on how to define new sorting
functions.")

;; XXX This looks like reinventing the wheel... IDK.
(defmacro org-clock-agg--extract-params (body &rest params)
  "Extract parameters from BODY.

BODY is a list of expressions.  PARAMS is a list of symbols starting
with \":\".

E.g., if BODY is (:foo 1 :bar 2 something something), the usage is as
follows:

\(let \(foo bar)
  \(org-clock-agg--extract-params body :foo :bar)
  ;; do something with foo and bar
)"
  `(let ((body-wo-docstring (if (stringp (car-safe ,body)) (cdr body) ,body))
         (docstring (when (stringp (car-safe ,body)) (car-safe ,body))))
     (while-let ((symbol (and
                          (member (car-safe body-wo-docstring) ',params)
                          (car-safe body-wo-docstring))))
       ,@(mapcar
          (lambda (param)
            `(when (eq symbol ,param)
               (setq ,(intern (substring (symbol-name param) 1)) (cadr body-wo-docstring))))
          params)
       (setq body-wo-docstring (cddr body-wo-docstring)))
     (if docstring
         (setq ,body (cons docstring body-wo-docstring))
       (setq ,body body-wo-docstring))))

(cl-defmacro org-clock-agg-defgroupby (name &body body)
  "Define a grouping function for `org-clock-agg'.

NAME is the name of the function.  BODY binds the following variables:
- `elem' - an alist as described in `org-clock-agg--parse-headline'
- `extra-params' - an alist with extra parameters.  See
  `org-clock-agg' on that.

The function must return a list of strings, which are the group names.

BODY can also include the following keyword arguments:
- `:readable-name' - function name for the UI.  If not given, NAME is
  used.
- `:hidden' - if non-nil, the function is not shown in the UI.
- `:default-sort' - if non-nil, the function is used as the default
  sort function."
  (declare (indent defun)
           (doc-string 2))
  (let ((func-name (intern (concat "org-clock-agg--groupby-" (symbol-name name))))
        readable-name hidden default-sort)
    ;; Parse keyword arguments in BODY
    (org-clock-agg--extract-params body :readable-name :hidden
                                   :default-sort)
    (unless readable-name
      (setq readable-name (symbol-name name)))
    `(progn
       (defun ,func-name (elem extra-params)
         ;; XXX To silence the byte-compiler
         (ignore elem extra-params)
         ,@body)
       (setf (alist-get ',name org-clock-agg-groupby-functions)
             '((:function . ,func-name)
               (:hidden . ,hidden)
               (:readable-name . ,readable-name)
               (:default-sort . ,default-sort))))))

(cl-defmacro org-clock-agg-defsort (name &body body)
  "Define a sorting function for `org-clock-agg'.

NAME is the name of the function.  BODY binds a `nodes' variable,
which is a list of tree nodes as described in function
`org-clock-agg--groupby'.

BODY can also contain the following keyword arguments:
- `:readable-name' - function name for the UI.  If not given, NAME is
  used."
  (declare (indent defun)
           (doc-string 2))
  (let ((func-name (intern (concat "org-clock-agg--sort-" (symbol-name name))))
        readable-name)
    (org-clock-agg--extract-params body :readable-name)
    (unless readable-name
      (setq readable-name (symbol-name name)))
    `(progn
       (defun ,func-name (nodes)
         ,@body)
       (setf (alist-get ',name org-clock-agg-sort-functions)
             '((:function . ,func-name)
               (:readable-name . ,readable-name))))))

(org-clock-agg-defgroupby category
  "Group org-clock entries by category."
  :readable-name "Category"
  :default-sort total
  (list (alist-get :category elem)))

(org-clock-agg-defgroupby org-file
  "Group org-clock entries by file in `org-directory'."
  :readable-name "Org file"
  :default-sort total
  (list
   (file-relative-name (alist-get :file elem)
                       (directory-file-name org-directory))))

(org-clock-agg-defgroupby outline-path
  "Group org-clock entries by outline path."
  :readable-name "Outline path"
  :default-sort total
  (alist-get :outline-path elem))

(org-clock-agg-defgroupby tags
  "Group org-clock entries by tags."
  :readable-name "Tags"
  :default-sort total
  (seq-sort
   #'string-lessp
   (alist-get :tags elem)))

(org-clock-agg-defgroupby headline
  "Group org-clock entries by headline."
  :readable-name "Headline"
  :default-sort total
  (list (substring-no-properties
         (org-element-property :raw-value (alist-get :headline elem)))))

(org-clock-agg-defgroupby day
  :readable-name "Day"
  :default-sort start-time
  (list (thread-last elem
                     (alist-get :start)
                     (seconds-to-time)
                     (format-time-string org-clock-agg-day-format))))

(org-clock-agg-defgroupby week
  :readable-name "Week"
  :default-sort start-time
  (list (thread-last elem
                     (alist-get :start)
                     (seconds-to-time)
                     (format-time-string org-clock-agg-week-format))))

(org-clock-agg-defgroupby month
  :readable-name "Month"
  :default-sort start-time
  (list (thread-last elem
                     (alist-get :start)
                     (seconds-to-time)
                     (format-time-string org-clock-agg-month-format))))

(org-clock-agg-defgroupby todo
  :readable-name "TODO keyword"
  :default-sort total
  (list (substring-no-properties
         (org-element-property :todo-keyword (alist-get :headline elem)))))

(org-clock-agg-defgroupby is-done
  :readable-name "Is done"
  :default-sort total
  (list (if (eq (org-element-property :todo-type (alist-get :headline elem)) 'done)
            "Done"
          "Not done")))

(org-clock-agg-defgroupby day-of-week
  :readable-name "Day of week"
  :default-sort name
  (list (thread-last elem
                     (alist-get :start)
                     (seconds-to-time)
                     (format-time-string "%u - %A"))))

(defun org-clock-agg--make-property-name-readable (name)
  "Make an org property NAME more readable."
  (thread-last
    name
    (replace-regexp-in-string (rx (or "_" "-")) " ")
    (capitalize)))

(org-clock-agg-defgroupby selected-properties
  :readable-name "Selected props"
  :default-sort total
  (cl-loop for (key . value) in (alist-get :properties elem)
           if value
           collect (format
                    "%s: %s"
                    (org-clock-agg--make-property-name-readable key)
                    value)))

(org-clock-agg-defgroupby root-group
  "Return \"Root\".  Used for the root group."
  :readable-name "Root"
  :default-sort total
  :hidden t
  (list "Results"))

(org-clock-agg-defsort name
  "Sort by name."
  :readable-name "Name"
  (seq-sort-by (lambda (elem) (alist-get :name elem)) #'string-lessp nodes))

(org-clock-agg-defsort total
  "Sort by total time spent."
  :readable-name "Total time"
  (seq-sort-by (lambda (elem) (alist-get :total elem)) #'> nodes))

(org-clock-agg-defsort start-time
  "Sort by start time."
  :readable-name "Start time"
  (seq-sort-by
   (lambda (elem)
     (thread-last elem
                  (list)
                  (org-clock-agg--ungroup)
                  (mapcar (lambda (row-elem) (alist-get :start row-elem)))
                  (seq-min)))
   #'> nodes))

(org-clock-agg-defsort end-time
  "Sort by end time."
  :readable-name "End time"
  (seq-sort-by
   (lambda (elem)
     (thread-last elem
                  (list)
                  (org-clock-agg--ungroup)
                  (mapcar (lambda (row-elem) (alist-get :end row-elem)))
                  (seq-max)))
   #'> nodes))

(defun org-clock-agg--groupby-apply (alist groups elem)
  "Recursively perform grouping for `org-clock-agg'.

ALIST is the alist used to store the results.  GROUPS is a list of
groups for ELEM.  GROUPS is a list with the following structure for
one group:
- group name
- parameters of the grouping function (as in the variable
  `org-clock-agg-groupby-functions')
- name of the sorting function (keys of the variable
  `org-clock-agg-sort-functions')
- sort order (t to reverse).

Refer to the function `org-clock-agg--groupby' for a description of
the return value."
  (let* ((group-params (car groups))
         (key (nth 0 group-params))
         (groupby (nth 1 group-params))
         (sort (nth 2 group-params))
         (sort-order (nth 3 group-params))
         (rest (cdr groups))
         (duration (alist-get :duration elem))
         (prev-val (alist-get key alist nil nil #'equal)))
    (when key
      (setf (alist-get key alist nil nil #'equal)
            `((:total . ,(+ duration (or (alist-get :total prev-val) 0)))
              (:groupby . ,groupby)
              (:children . ,(org-clock-agg--groupby-apply
                             (alist-get :children prev-val) rest elem))
              (:sort-symbol . ,sort)
              (:sort-order . ,sort-order)
              (:parent-share . ,0)
              (:total-share . ,0)
              (:elems . ,(if rest
                             (alist-get :elems prev-val)
                           (cons elem (alist-get :elems prev-val))))))))
  alist)

(defun org-clock-agg--add-ungrouped (tree)
  "Add \"Ungrouped\" nodes to TREE.

This function adds an \"Ungrouped\" node to every node in TREE that
contains both `:elems' (ungrouped elements) and `:children'.  This
can happen when only a portion of the elements of the node was grouped
by a grouping function.

The addition of the \"Ungrouped\" node with all the ungrouped elements
ensures that the total time spent in the node equals the sum of the
total time spent in its children.

TREE is a tree as returned by `org-clock-agg--groupby'."
  (dolist (node tree)
    (let ((children (alist-get :children (cdr node)))
          (elems (alist-get :elems (cdr node))))
      (org-clock-agg--add-ungrouped children)
      (when (and children elems)
        (let ((total (seq-reduce (lambda (acc val)
                                   (+ acc (alist-get :duration val)))
                                 elems 0)))

          (setf (alist-get :children (cdr node))
                (cons
                 `("Ungrouped"
                   (:total . ,total)
                   (:groupby . ((:readable-name . "Ungrouped")))
                   (:children)
                   (:sort-symbol . total)
                   (:sort-order)
                   (:parent-share . 0)
                   (:total-share . 0)
                   (:elems . ,elems))
                 (alist-get :children (cdr node)))))
        (setf (alist-get :elems (cdr node)) nil))))
  tree)

(defun org-clock-agg--groupby-postaggregate (tree &optional total-time parent-time)
  "Perform final aggregation calculations on TREE.

Sets the following fields on each tree node:
- `:parent-share'
- `:total-share'

TOTAL-TIME and PARENT-TIME are recursive parameters."
  (unless (and total-time parent-time)
    (setq total-time (alist-get :total (cdar tree)))
    (setq parent-time total-time))
  (dolist (node tree)
    (let ((total (float (alist-get :total (cdr node)))))
      (setf (alist-get :parent-share (cdr node))
            (if (< 0 parent-time)
                (/ total (float parent-time))
              0)
            (alist-get :total-share (cdr node))
            (if (< 0 total-time)
                (/ total (float total-time))
              0))
      (org-clock-agg--groupby-postaggregate
       (alist-get :children (cdr node))
       total-time
       total))))

(defun org-clock-agg--groupby (elems groupby-list sort-list sort-order-list extra-params)
  "Group ELEMS for `org-clock-agg' into a tree.

ELEMS is a list as described in `org-clock-agg--parse-headline'.
GROUPBY-LIST is a list of keys of the variable
`org-clock-agg-groupby-functions'.  SORT-LIST is a list of keys of
the variable `org-clock-agg-sort-functions'.  SORT-ORDER-LIST is a
list of booleans indicating whether to reverse the sort order for the
corresponding key in SORT-LIST.

The root group is always added to the beginning of GROUPBY-LIST.

EXTRA-PARAMS is an alist of extra parameters for the grouping
functions.  If `:add-ungrouped' is non-nil, \"Ungrouped\" nodes are
added to the tree.  See `org-clock-agg' for details.

The return value is a tree of alists with the following keys:
- `:total' - total seconds spent in the group
- `:parent-share' - `:total' / time spent in the parent node
- `:total-share' - `:total' / time spent in the top-level node
- `:groupby' - grouping function (as in the variable
  `org-clock-agg-groupby-functions')
- `:children' - list of children tree nodes
- `:sort-symbol' - key of the variable `org-clock-agg-sort-functions'
  used for sorting
- `:sort-order' - if non-nil, the sort order is reversed
- `:elems' - list of elements in the group, in the same form as
  ELEMS."
  (let (tree)
    (dolist (elem elems)
      (let* ((group-symbols (cons 'root-group groupby-list))
             (sort-symbols (cons 'total sort-list))
             (sort-orders (cons nil sort-order-list))
             (groups
              (cl-loop for group-symbol in group-symbols
                       for sort-symbol in sort-symbols
                       for sort-order in sort-orders
                       for groupby = (alist-get group-symbol org-clock-agg-groupby-functions)
                       for group-values = (funcall (alist-get :function groupby) elem extra-params)
                       append
                       (mapcar
                        (lambda (group-value)
                          (list group-value groupby sort-symbol sort-order))
                        group-values))))
        (setq tree (org-clock-agg--groupby-apply tree groups elem))))
    (when (alist-get :add-ungrouped extra-params)
      (setq tree (org-clock-agg--add-ungrouped tree)))
    (org-clock-agg--groupby-postaggregate tree)
    tree))

(defun org-clock-agg--ungroup (tree)
  "Reverse grouping for TREE.

TREE is a tree of alists as described in `org-clock-agg--groupby'.
The return value is a list of elements as described in
`org-clock-agg--parse-headline'."
  (cl-loop for node in tree
           append (alist-get :elems node)
           append (org-clock-agg--ungroup (alist-get :children node))))

(defun org-clock-agg--groupby-sort (tree)
  "Sort the grouped TREE.

TREE is a tree of alists as described in `org-clock-agg--groupby'."
  (let* ((sorted-nodes-by-group
          (thread-last
            tree
            (mapcar (lambda (node) (cons (cons :name (car node)) (cdr node))))
            (seq-group-by
             (lambda (node)
               (list (alist-get :symbol (alist-get :groupby node))
                     (alist-get :sort-symbol node)
                     (alist-get :sort-order node))))
            (mapcar
             (lambda (grouped)
               (let (;; (group-symbol (nth 0 (car grouped)))
                     (sort-symbol (nth 1 (car grouped)))
                     (sort-order (nth 2 (car grouped))))
                 (setf (cdr grouped)
                       (funcall (thread-last org-clock-agg-sort-functions
                                             (alist-get sort-symbol)
                                             (alist-get :function))
                                (cdr grouped)))
                 (when sort-order
                   (setf (cdr grouped) (reverse (cdr grouped))))
                 grouped)))
            (seq-sort-by
             (lambda (grouped)
               (thread-last org-clock-agg-groupby-functions
                            (alist-get (car (car grouped)))
                            (alist-get :readable-name)))
             #'string-lessp)))
         (tree (seq-reduce (lambda (acc grouped)
                             (append (cdr grouped) acc))
                           sorted-nodes-by-group nil)))
    (dolist (node tree)
      (let ((children (alist-get :children node))
            (elems (alist-get :elems node)))
        (when children
          (setf (alist-get :children node)
                (org-clock-agg--groupby-sort children)))
        (when elems
          (setf (alist-get :elems node)
                (seq-sort-by (lambda (elem) (alist-get :start elem))
                             #'>
                             (alist-get :elems node))))))
    (mapcar (lambda (node)
              (cons (alist-get :name node)
                    node))
            tree)))

;; View & manage results
(defvar-local org-clock-agg--params nil
  "Parameters for the current `org-clock-agg' buffer.")

(defvar-local org-clock-agg--elems nil
  "Elements for the current `org-clock-agg' buffer.")

(defvar-local org-clock-agg--tree nil
  "Tree for the current `org-clock-agg' buffer.")

(defun org-clock-agg-quit ()
  "Quit the current `org-clock-agg' buffer."
  (interactive)
  (quit-window t))

(defvar org-clock-agg-tree-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'org-clock-agg-quit)
    (define-key keymap (kbd "r") #'org-clock-agg-refresh)
    (define-key keymap (kbd "e") #'org-clock-agg-view-elems-at-point)
    (define-key keymap (kbd "d") #'org-clock-agg-drill-down-at-point)
    (define-key keymap (kbd "<tab>") #'outline-toggle-children)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'org-clock-agg-quit
        "gr" #'org-clock-agg-refresh
        "e" #'org-clock-agg-view-elems-at-point
        "d" #'org-clock-agg-drill-down-at-point
        (kbd "<tab>") #'outline-toggle-children))
    keymap))

(define-derived-mode org-clock-agg-tree-mode fundamental-mode "Org Clock Agg Tree"
  "Major mode for viewing `org-clock-agg' results."
  (outline-minor-mode 1))

(defun org-clock-agg--render-controls-files ()
  "Render the file picker for the `org-clock-agg' buffer."
  (apply
   #'widget-create 'menu-choice
   :tag "Files"
   :value (alist-get :files org-clock-agg--params)
   :notify (lambda (widget &rest _)
             (setf (alist-get :files org-clock-agg--params)
                   (widget-value widget)))
   '(item :tag "Org Agenda" :value org-agenda)
   (append
    (mapcar
     (lambda (item)
       `(item :tag ,(car item) :value ,(cdr item)))
     org-clock-agg-files-preset)
    '((editable-list :tag "File"
                     :entry-format "%i %d %v"
                     :menu-tag "Custom list"
                     :value nil
                     (editable-field :tag "File" :value ""))))))

(defun org-clock-agg--render-controls-date ()
  "Render the date picker for the `org-clock-agg' buffer."
  (widget-create 'editable-field
                 :size 20
                 :format (concat (propertize "Date from: " 'face 'widget-button)
                                 "%v   ")
                 :value (let ((val (alist-get :from org-clock-agg--params)))
                          (if (numberp val)
                              (number-to-string val)
                            val))
                 :notify (lambda (widget &rest _)
                           (let ((val (widget-value widget)))
                             (when (string-match-p (rx bos (? "-") (+ digit) eos) val)
                               (setq val (string-to-number val)))
                             (setf (alist-get :from org-clock-agg--params) val))))
  (widget-create 'editable-field
                 :size 20
                 :format (concat (propertize "To: " 'face 'widget-button)
                                 "%v   ")
                 :value (let ((val (alist-get :to org-clock-agg--params)))
                          (if (numberp val)
                              (number-to-string val)
                            val))
                 :notify (lambda (widget &rest _)
                           (let ((val (widget-value widget)))
                             (when (string-match-p (rx bos (? "-") (+ digit) eos) val)
                               (setq val (string-to-number val)))
                             (setf (alist-get :to org-clock-agg--params) val)))))

(defun org-clock-agg--render-controls-groupby ()
  "Render grouping controls for the `org-clock-agg' buffer."
  (insert (propertize "Group by: " 'face 'widget-button) "\n")
  (widget-create 'editable-list
                 :tag "Group by"
                 :entry-format "%i %d %v"
                 :value
                 (cl-loop for group-value in (alist-get :groupby org-clock-agg--params)
                          for sort-value in (alist-get :sort org-clock-agg--params)
                          for sort-order-value in
                          (alist-get :sort-order org-clock-agg--params)
                          collect (list group-value sort-value sort-order-value))
                 :notify
                 (lambda (widget _changed-widget &optional _event)
                   (let ((group-value (mapcar #'car (widget-value widget)))
                         (sort-value (mapcar #'cadr (widget-value widget)))
                         (sort-order-value (mapcar #'caddr (widget-value widget))))
                     (setf (alist-get :groupby org-clock-agg--params) group-value)
                     (setf (alist-get :sort org-clock-agg--params) sort-value)
                     (setf (alist-get :sort-order org-clock-agg--params)
                           sort-order-value)))
                 `(group
                   :value (outline-path total)
                   (menu-choice
                    :tag "Group"
                    :notify (lambda (widget _child &optional event)
                              (if-let* ((value (widget-value widget))
                                        (default-sort
                                         (alist-get
                                          :default-sort
                                          (alist-get value org-clock-agg-groupby-functions)))
                                        (parent (widget-get widget :parent)))
                                  (widget-value-set parent (list value default-sort)))
                              (widget-default-action widget event))
                    ,@(thread-last
                        org-clock-agg-groupby-functions
                        (seq-filter (lambda (groupby)
                                      (not (alist-get :hidden (cdr groupby)))))
                        (mapcar
                         (lambda (groupby)
                           (let ((name (car groupby))
                                 (readable-name (alist-get :readable-name (cdr groupby))))
                             `(item :tag ,readable-name
                                    :value ,name
                                    :menu-tag ,readable-name))))))
                   (menu-choice
                    :tag "Order"
                    ,@(mapcar
                       (lambda (sort)
                         (let ((name (car sort))
                               (readable-name (alist-get :readable-name (cdr sort))))
                           `(item :tag ,readable-name
                                  :value ,name
                                  :menu-tag ,readable-name)))
                       org-clock-agg-sort-functions))
                   (toggle :on "Reverse order" :off "Normal order"))))

(defun org-clock-agg--extras-notify (widget &rest _)
  "Notify funciton for extra-params widgets.

WIDGET is the instance of the widget that was changed."
  (let ((extras-key (widget-get widget :extras-key)))
    (setf (alist-get extras-key
                     (alist-get :extra-params org-clock-agg--params))
          (widget-value widget))))

(defun org-clock-agg--render-extra-params ()
  "Render extra-params for the `org-clock-agg' buffer."
  (pcase-dolist (`(,name . ,params) (append org-clock-agg--extra-params-default
                                            org-clock-agg-extra-params))
    (insert (propertize name 'face 'widget-button) " ")
    (apply #'widget-create `(,@params :notify org-clock-agg--extras-notify))
    (insert "\n")))

(defun org-clock-agg--drill-down-p ()
  "Whether the current buffer is drill-down for the previous query."
  (eq (alist-get :files org-clock-agg--params) 'drill))

(defun org-clock-agg--render-controls ()
  "Render controls for the `org-clock-agg' buffer."
  (remove-overlays)
  (insert (propertize "* Parameters" 'face 'org-level-1) "\n")
  (if (org-clock-agg--drill-down-p)
      (insert (propertize "Files" 'face 'widget-inactive)
              ": Drill down previous query\n\n")
    (org-clock-agg--render-controls-files)
    (insert "\n")
    (org-clock-agg--render-controls-date)
    (insert "\n\n"))
  (org-clock-agg--render-controls-groupby)
  (insert "\n")
  (org-clock-agg--render-extra-params)
  (insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (org-clock-agg-refresh))
                 "Refresh")
  (insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (org-clock-agg-view-elems))
                 "View records")
  (insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (org-clock-agg-csv))
                 "Export records to CSV")
  (insert " ")
  (unless (org-clock-agg--drill-down-p)
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (org-clock-agg-generate-report))
                   "Create function"))
  (insert "\n\n")
  (widget-setup))

(defun org-clock-agg--trim-string (string max-len)
  "Trim STRING to MAX-LEN characters.

If STRING is longer than MAX-LEN, trim it to MAX-LEN - 3 and
append \"...\"."
  (let ((len (length string)))
    (if (> len max-len)
        (concat (substring string 0 (- max-len 3)) "...")
      string)))

(defun org-clock-agg--goto-elem (elem)
  "Go to the element at ELEM.

ELEM is an alist as described in `org-clock-agg--parse-headline'."
  (let ((marker (org-element-property :org-marker (alist-get :headline elem))))
    (org-goto-marker-or-bmk marker)))

(defun org-clock-agg--process-format-spec (spec &optional lexical)
  "Expand the %(...) in SPEC string.

Each %(...) is evaluated as an elisp expression and the result
is inserted in the string.

LEXICAL is the lexical environment in which the expressions are
evaluated."
  (save-match-data
    (while (string-match "%\\(([^)]+)\\)" spec)
      (setq spec (replace-match
                  (format "%s" (eval (read (match-string 1 spec)) lexical))
                  t t spec)))
    spec))

(defun org-clock-agg-render-tree-node-elems (node)
  "Render elements for the tree NODE.

NODE is one node of a tree, which is described in the function
`org-clock-agg--groupby'."
  (when-let ((elems (alist-get :elems (cdr node)))
             (widget-push-button-prefix "")
             (widget-push-button-suffix ""))
    (dolist (elem elems)
      (let* ((spec
              `((?s .
                    ,(propertize
                      (thread-last elem
                                   (alist-get :start)
                                   (seconds-to-time)
                                   (format-time-string (cdr org-time-stamp-formats)))
                      'face 'org-date))
                (?e . ,(propertize
                        (thread-last elem
                                     (alist-get :end)
                                     (seconds-to-time)
                                     (format-time-string (cdr org-time-stamp-formats)))
                        'face 'org-date))
                (?d . ,(org-duration-from-minutes
                        (/ (alist-get :duration elem) 60)))
                (?t . ,(concat
                        (when-let ((todo-keyword
                                    (org-element-property
                                     :todo-keyword (alist-get :headline elem))))
                          (propertize
                           (concat (substring-no-properties todo-keyword) " ") 'face
                           (if (eq (org-element-property
                                    :todo-type (alist-get :headline elem))
                                   'done)
                               'org-done 'org-todo)))
                        (org-element-property :raw-value
                                              (alist-get :headline elem))))))
             (elem-name
              (format-spec
               (org-clock-agg--process-format-spec org-clock-agg-elem-format)
               spec)))
        (widget-create 'push-button
                       :elem elem
                       :notify (lambda (widget &rest _)
                                 (let ((elem (widget-get widget :elem)))
                                   (org-clock-agg--goto-elem elem)))
                       :button-face 'org-clock-agg-elem-face
                       elem-name))
      (insert "\n"))))

(defun org-clock-agg--render-tree-node (node show-elems &optional level)
  "Render the tree NODE.

NODE is one node of a tree, which is described in the function
`org-clock-agg--groupby'.  If SHOW-ELEMS is non-nil, render the
elements as well.  LEVEL is the level of the node."
  (unless level
    (setq level 1))
  (let* ((level-face (nth (mod (1- level) 8)  org-level-faces))
         (level-string (make-string level ?*))
         (title-width (- (window-width) org-clock-agg-node-title-width-delta))
         (spec `((?t . ,(propertize (org-clock-agg--trim-string
                                     (concat level-string " " (car node))
                                     title-width)
                                    'face level-face))
                 (?c . ,(propertize
                         (alist-get :readable-name (alist-get :groupby (cdr node)))
                         'face 'org-clock-agg-group-face))
                 (?s . ,(propertize
                         (thread-last (cdr node)
                                      (alist-get :total-share)
                                      (* 100)
                                      (truncate)
                                      (number-to-string))
                         'face 'org-clock-agg-share-face))
                 (?S . ,(propertize
                         (thread-last (cdr node)
                                      (alist-get :parent-share)
                                      (* 100)
                                      (truncate)
                                      (number-to-string))
                         'face 'org-clock-agg-share-face))
                 (?z . ,(propertize
                         (format-seconds
                          org-clock-agg-duration-format
                          (alist-get :total (cdr node)))
                         'face 'org-clock-agg-duration-face)))))
    (insert (propertize
             (format-spec
              (org-clock-agg--process-format-spec
               org-clock-agg-node-format
               `((title-width . ,title-width)))
              spec)
             'node node)
            "\n")
    (when show-elems
      (org-clock-agg-render-tree-node-elems node)))
  (mapc (lambda (child)
          (org-clock-agg--render-tree-node child show-elems (1+ level)))
        (alist-get :children (cdr node))))

(defun org-clock-agg--view-elems (tree)
  "View elements of an `org-clock-agg' TREE with `org-ql'."
  ;; `org-ql' doesn't requrire this by default, I assume for
  ;; optimization purposes.  I won't interfere.
  (require 'org-ql-view)
  (let* ((elems (org-clock-agg--ungroup tree))
         (strings (mapcar (lambda (elem)
                            (org-ql-view--format-element
                             (alist-get :headline elem)))
                          elems)))
    (org-ql-view--display
      :buffer "*org-clock-agg-elems*"
      :header (format "Elements: %s" (if (length= tree 1) (caar tree) "tree"))
      :strings strings)))

(defun org-clock-agg-view-elems-at-point ()
  "View elements of an `org-clock-agg' node at point."
  (interactive)
  (let ((node-at-point (get-text-property (point) 'node)))
    (unless node-at-point
      (user-error "No node at point!"))
    (org-clock-agg--view-elems (list node-at-point))))

(defun org-clock-agg-view-elems ()
  "View the found elements in the current `org-clock-agg' buffer."
  (interactive)
  (org-clock-agg--view-elems org-clock-agg--tree))

(defun org-clock-agg-drill-down-at-point ()
  "Open the report buffer solely for the element at point."
  (interactive)
  (let ((node-at-point (get-text-property (point) 'node)))
    (unless node-at-point
      (user-error "No node at point!"))
    (let ((buffer (generate-new-buffer (format "*org-clock-agg-drill-down-%s*"
                                               (car node-at-point))))
          (params (copy-tree org-clock-agg--params)))
      (setf (alist-get :files params) 'drill)
      (switch-to-buffer-other-window buffer)
      (with-current-buffer buffer
        (org-clock-agg-tree-mode)
        (setq-local org-clock-agg--elems (org-clock-agg--ungroup
                                          (list node-at-point)))
        (setq-local org-clock-agg--params params)
        (let ((inhibit-read-only t))
          (org-clock-agg--render-controls)
          ;; XXX No idea why, but setting these variables with let
          ;; doesn't work when the package is loaded.
          (setq-local widget-push-button-prefix "")
          (setq-local widget-push-button-suffix "")
          (org-clock-agg-refresh))
        (goto-char (point-min))))))

(defun org-clock-agg--parse-files (files)
  "Return a list of files to use in the `org-clock-agg' buffer.

FILES is a possible return value of the file picker, which is
created by `org-clock-agg--render-controls-files'."
  (cond ((eq files 'org-agenda)
         (org-agenda-files))
        ((member files (mapcar #'car org-clock-agg-files-preset))
         (alist-get files org-clock-agg-files-preset nil nil #'equal))
        (t files)))

(cl-defun org-clock-agg-exec (from to files groupby sort sort-order extra-params)
  "Aggregate org-clock data and return the result as tree.

See `org-clock-agg' for the meaning of FROM, TO, FILES, GROUPBY, SORT,
SORT-ORDER, and EXTRA-PARAMS.  See `org-clock-agg--groupby' for the
return value description."
  (let* ((files (org-clock-agg--parse-files files))
         (elems (org-clock-agg--query from to files))
         (tree (org-clock-agg--groupby elems groupby sort sort-order extra-params))
         (tree (org-clock-agg--groupby-sort tree)))
    (cons elems tree)))

(defun org-clock-agg-refresh ()
  "Refresh the `org-clock-agg' buffer."
  (interactive)
  (cl-destructuring-bind (&key from to files groupby sort sort-order extra-params)
      (org-clock-agg--alist-to-plist org-clock-agg--params)
    (pcase-let ((`(,elems . ,tree)
                 (org-clock-agg-exec from to files groupby sort sort-order extra-params)))
      (setq-local org-clock-agg--elems elems)
      (setq-local org-clock-agg--tree tree)
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (search-forward (format "* Results") nil 'noerror)
          (beginning-of-line)
          (delete-region (point) (point-max))
          (dolist (node tree)
            (org-clock-agg--render-tree-node
             node
             (alist-get :show-elems extra-params))))))))

(defun org-clock-agg-generate-report ()
  "Generate a report function from the `org-clock-agg' state."
  (interactive)
  (unless (derived-mode-p 'org-clock-agg-tree-mode)
    (user-error "Not in `org-clock-agg-tree-mode'"))
  (let ((buffer (generate-new-buffer "*org-clock-agg-gen*")))
    (cl-destructuring-bind (&key from to files groupby sort sort-order extra-params)
        (org-clock-agg--alist-to-plist org-clock-agg--params)
      (with-current-buffer buffer
        (emacs-lisp-mode)
        (insert
         ";; Change the function name if necessary\n"
         (pp-to-string
          `(defun org-clock-agg-custom-report ()
             (interactive)
             (apply #'org-clock-agg
                    '(,from ,to ,files ,groupby ,sort ,sort-order ,extra-params)))))))
    (switch-to-buffer buffer)))

(defun org-clock-agg--csv-elems-to-alist (elems)
  "Convert ELEMS to an alist.

ELEMS is a list as described in `org-clock-agg--parse-headline'."
  (cl-loop for elem in elems
           collect
           `((start . ,(thread-last elem
                                    (alist-get :start)
                                    (seconds-to-time)
                                    (format-time-string (cdr org-time-stamp-formats))))
             (end . ,(thread-last elem
                                  (alist-get :end)
                                  (seconds-to-time)
                                  (format-time-string (cdr org-time-stamp-formats))))
             (duration . ,(alist-get :duration elem))
             (headline . ,(car (org-clock-agg--groupby-headline elem nil)))
             (todo-keyword . ,(car (org-clock-agg--groupby-todo elem nil)))
             (is-done . ,(car (org-clock-agg--groupby-is-done elem nil)))
             (day-of-week . ,(car (org-clock-agg--groupby-day-of-week elem nil)))
             (category . ,(car (org-clock-agg--groupby-category elem nil)))
             (org-file . ,(car (org-clock-agg--groupby-org-file elem nil)))
             (outline-path . ,(string-join
                               (org-clock-agg--groupby-outline-path elem nil) "/"))
             (tags . ,(string-join
                       (org-clock-agg--groupby-tags elem nil) "/")))))

(defun org-clock-agg--csv-alist-to-string (data)
  "Convert DATA to csv string.

DATA has to be an alist, where each item has the same set of
attributes."
  (concat
   (mapconcat
    (lambda (datum)
      (symbol-name (car datum)))
    (car data)
    ",")
   "\n"
   (cl-loop with keys = (mapcar #'car (car data))
            for datum in data
            concat (mapconcat
                    (lambda (key)
                      (let ((item (alist-get key datum)))
                        (cond ((numberp item) (format "%s" item))
                              ((and (stringp item)
                                    (string-match-p (rx (| " " "," "\"")) item))
                               (format "\"%s\"" (string-replace
                                                 "\""
                                                 "\"\""
                                                 item)))
                              ((stringp item) (format "%s" item))
                              (t ""))))
                    keys ",")
            concat "\n")))

(defun org-clock-agg-csv ()
  "Export the found elements in the `org-clock-agg' buffer as CSV."
  (interactive)
  (unless org-clock-agg--elems
    (user-error "Nothing found in the current buffer!"))
  (let* ((data (org-clock-agg--csv-elems-to-alist
                org-clock-agg--elems))
         (csv-string (org-clock-agg--csv-alist-to-string data))
         (file-name (read-file-name "Save CSV: " nil "report.csv")))
    (with-temp-file file-name
      (insert csv-string))))

(defun org-clock-agg (from to files groupby sort sort-order extra-params)
  "Aggregate org-clock data.

The function creates an interactive buffer for configuring the
aggregation and displaying the results.  When called
non-interactively, initial parameters can be passed as arguments.

Use `org-clock-agg-exec' to retrieve the results without the
interactive buffer.

FROM and TO define the time range.  Both are either a relative number
of days, or a date string parseable by `parse-time-string', with or
without the time part.  See `org-clock-agg--normalize-time-predicate'
for details.

FILES is either `org-agenda', a key from `org-clock-agg-files-preset'
\(in which case the value of that variable is used), or a list of
file paths.

GROUPBY is a list of keys from `org-clock-agg-groupby-functions'.
Each function returns a list of groups for each entry; the result
forms a tree.  SORT is a list of keys from
`org-clock-agg-sort-functions', and its length must match GROUPBY.
The Nth entry in SORT defines the sorting logic for the results of the
Nth GROUPBY function.

SORT-ORDER must be of the same length as SORT.  If the Nth entry is
non-nil, the sorting is reversed.

EXTRA-PARAMS is an alist of \"extra parameters\".  Possible keys are
defined by `org-clock-agg--extra-params-default' and
`org-clock-agg-extra-params'.  Built-in parameters include:
- `:show-elems' - whether to display raw elements for each group in
  the buffer (an \"element\" represents one org-clock record).
- `:add-ungrouped' - whether to add the \"Ungrouped\" group to the
  results.

`org-clock-agg-extra-params' can define new parameters, intended for
custom aggregation functions to control their behavior at runtime.

Refer to the mentioned variables and the interactive buffer for
available group and sort functions; use `org-clock-agg-defgroupby' and
`org-clock-agg-defsort' to define new ones."
  (interactive (list -7 0 'org-agenda nil nil nil nil))
  (let* ((buffer (generate-new-buffer "*org-clock-agg*")))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (org-clock-agg-tree-mode)
      (setq-local org-clock-agg--params
                  `((:from . ,from)
                    (:to . ,to)
                    (:files . ,files)
                    (:groupby . ,groupby)
                    (:sort . ,sort)
                    (:sort-order . ,sort-order)
                    (:extra-params . ,extra-params)))
      (let ((inhibit-read-only t))
        (org-clock-agg--render-controls)
        ;; XXX No idea why, but setting these variables with let
        ;; doesn't work when the package is loaded.
        (setq-local widget-push-button-prefix "")
        (setq-local widget-push-button-suffix "")
        (org-clock-agg-refresh))
      (goto-char (point-min)))))

(provide 'org-clock-agg)
;;; org-clock-agg.el ends here
