;;; org-clock-agg.el --- Tree-like reports for org-clock records -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (compat "29.1.4.1") (org-ql "0.8-pre"))
;; Homepage: https://github.com/SqrtMinusOne/org-clock-agg

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

;; Aggregate org-clock records and show the results in an interactive
;; buffer.  The records are grouped by predicates such as file name,
;; their outline path in the file, etc.  Each record is placed in a
;; tree strcture; each node of the tree shows the total time spent in
;; that node and its children.  The top-level node shows the total
;; time spent in all records found by the query.
;;
;; `org-clock-agg' is the main entrypoint.  It can be run interactively
;; or from elisp code.  See the docstring for details.
;;
;; See also the REAME at
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

(defgroup org-clock-agg nil
  "Aggregate org-clock statistics."
  :group 'org-clock)

(defcustom org-clock-agg-duration-format "%h:%.2m"
  "Format string for durations in `org-clock-agg' views.

See `format-seconds' for the list of available format specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-files-preset nil
  "Presets for the \"files\" parameter in org-clock-agg views."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'org-clock-agg)

(defcustom org-clock-agg-day-format "%Y-%m-%d, %a"
  "Format string for days in `org-clock-agg' views.

See `format-time-string' for the list of available format specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-week-format "%Y-%W"
  "Format string for weeks in `org-clock-agg' views.

See `format-time-string' for the list of available format specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-month-format "%Y-%m"
  "Format string for months in `org-clock-agg' views.

See `format-time-string' for the list of available format specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-properties nil
  "Org properties to include in `org-clock-agg' views.

Either set this interactively or reset the `org-ql-cache' variable
manually after setting."
  :type '(repeat string)
  :group 'org-clock-agg
  :set (lambda (&rest _)
         (setq org-ql-cache (make-hash-table :weakness 'key))
         (setq org-ql-node-value-cache (make-hash-table :weakness 'key))))

(defconst org-clock-agg--extra-params-default
  '(("Show elements:" . (checkbox :extras-key :show-elems))
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

;; XXX org-ql caches results of queries, so make sure to run this
;; after updating `org-clock-agg--parse-headline'
;; (setq org-ql-cache (make-hash-table :weakness 'key))

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
  (let ((contents (buffer-substring-no-properties
                   ;; contents-begin starts after the headline
                   (org-element-property :contents-begin headline)
                   (org-element-property :contents-end headline))))
    (with-temp-buffer
      (insert contents)
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
        res))))

(defun org-clock-agg--properties-at-point ()
  "Return a list of selected properties at point.

  `org-clock-agg-properties' sets the list of properties to select.  The
  properties are inherited from the parent headlines and from the global
  properties set in the beginning of the file."
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
  - a number, in which case it's interpreted as a number of days from
  the current one
  - a string, parseable by `parse-time-string', with or without the time
  part.

  KIND is either 'from or 'to.  If it's the latter, the time part is the
  to 23:59:59 when possible, otherwise it's 00:00:00.

  The result is a number of seconds since the epoch."

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

  FROM and TO should either be a number (e.g. -7 is the last week) or a
  string parseable by `parse-time-string'.

  ELEMS is a list as descbribed in `org-clock-agg--parse-headline'."

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

  Return a list as descbribed in `org-clock-agg--parse-headline'."
  (thread-last
    (cl-loop for res in (org-ql-query
                          :select #'org-clock-agg--parse-headline
                          :from files
                          :where `(clocked :from ,from :to ,to))
             append res)
    (org-clock-agg--filter-elems from to)))

;;; Aggregation
(defvar org-clock-agg-groupby-functions nil
  "Group by functions for `org-clock-agg'.

  This is an alist with function names as keys and alists with the
  following keys as values:
  - `:function' - grouping function itself
  - `:hidden' - whether to hide the function in the UI
  - `:readable-name' - name to display in the UI
  - `:default-sort' - default sorting function to use for this group.

  See `org-clock-agg-defgroupby' on how to define new grouping
  functions.")

(defvar org-clock-agg-sort-functions nil
  "Sort functions for `org-clock-agg'.

  This is an alist with function names as keys and alists with the
  following keys as values:
  - `:function' - sorting function itself
  - `:readable-name' - name to display in the UI.

  See `org-clock-agg-defsort' on how to define new sorting
  functions.")

;; XXX This looks like reinventing the wheel... IDK.
(defmacro org-clock-agg--extract-params (body &rest params)
  "Extract parameters from BODY.

  BODY is a list of expressions.  PARAMS is a list of symbols starting
  with \":\".

  E.g. if BODY is (:foo 1 :bar 2 something something), the usage is as follows:

  \(let \(foo bar)
\(org-clock-agg--extract-params body :foo :bar)
;; do something with foo and bar
)"
  `(let ((body-wo-docstring (if (stringp (car-safe body)) (cdr body) body))
         (docstring (when (stringp (car-safe body)) (car-safe body))))
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
         (setq body (cons docstring body-wo-docstring))
       (setq body body-wo-docstring))))

(cl-defmacro org-clock-agg-defgroupby (name &body body)
  "Define a grouping function for `org-clock-agg'.

NAME is the name of the function.  BODY has the following variables bound:
- `elem' - an alist as described in `org-clock-agg--parse-headline'
- `extra-params' - and alist with extra parameters.  See
  `org-clock-agg' on that.
The function must return a list of strings, which are the group
names.

BODY can also contain the following keyword arguments:
- `:readable-name' - function name for the UI.  If not given, the name
of the function is used.
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
         ,@body)
       (setf (alist-get ',name org-clock-agg-groupby-functions)
             '((:function . ,func-name)
               (:hidden . ,hidden)
               (:readable-name . ,readable-name)
               (:default-sort . ,default-sort))))))

(cl-defmacro org-clock-agg-defsort (name &body body)
  "Define a sorting function for `org-clock-agg'.

NAME is the name of the function.  BODY has a variable `nodes' bound,
which is a list of tree nodes as described in
function `org-clock-agg--groupby'.

BODY can also contain the following keyword arguments:
- `:readable-name' - function name for the UI.  If not given, the name
of the function is used."
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
  (list (org-element-property :raw-value (alist-get :headline elem))))

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
  "Recursively perform the grouping for `org-clock-agg'.

ALIST is the alist in which to store the results.  GROUPS is a list of
groups for ELEM.  GROUPS is a list with the following values:
- group name
- parameters of the grouping function (as in the variable
                                          `org-clock-agg-groupby-functions')
- name of the sorting function (keys of the variable
                                     `org-clock-agg-sort-functions')
- sort order (t to reverse).

See the function `org-clock-agg--groupby' for the description of the
return value."
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
              (:elems . ,(if rest
                             (alist-get :elems prev-val)
                           (cons elem (alist-get :elems prev-val))))))))
  alist)

(defun org-clock-agg--add-ungrouped (tree)
  "Add \"Ungrouped\" nodes to TREE.

Such node is added to every node in TREE that has both `:elems',
i.e. ungrouped elements, and `:children'.  This can happen when only
part of elements of the node was grouped by a grouping function.

Adding the \"Ungrouped\" node with all the ungrouped elements ensures that
the total time spent in node equals the sum of the total time spent in
its children.

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
                   (:elems . ,elems))
                 (alist-get :children (cdr node)))))
        (setf (alist-get :elems (cdr node)) nil))))
  tree)

(defun org-clock-agg--groupby (elems groupby-list sort-list sort-order-list extra-params)
  "Group ELEMS for `org-clock-agg' into a tree.

ELEMS is a list as described in `org-clock-agg--parse-headline'.
GROUPBY-LIST is a list of keys of the variable
`org-clock-agg-groupby-functions'.  SORT-LIST is a list of keys of the variable
`org-clock-agg-sort-functions'.  SORT-ORDER-LIST is a list of booleans
indicating whether to reverse the sort order for the corresponding key
in SORT-LIST.

The root group is always added to the beginning of GROUPBY-LIST.

EXTRA-PARAMS is an alist of extra parameters for the grouping
functions.  If `:add-ungrouped' is non-nil, add \"Ungrouped\" nodes to
the tree.  See `org-clock-agg' for more.

The return value is a tree of alists with the following keys:
- `:total' - total seconds spent in group
- `:groupby' - grouping function (as in the variable
                                     `org-clock-agg-groupby-functions')
- `:children' - list of children tree nodes
- `:sort-symbol' - key of the variable `org-clock-agg-sort-functions' used for
sorting
- `:sort-order' - if non-nil, reverse the sort order
- `:elems' - list of elements in the group, same form as ELEMS."
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
               (let ((group-symbol (nth 0 (car grouped)))
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
    (define-key keymap (kbd "<tab>") #'outline-toggle-children)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'org-clock-agg-quit
        "gr" #'org-clock-agg-refresh
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
   :notify (lambda (widget &rest ignore)
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
                 :format (concat (propertize "Date from: " 'face 'widget-button) "%v   ")
                 :value (let ((val (alist-get :from org-clock-agg--params)))
                          (if (numberp val)
                              (number-to-string val)
                            val))
                 :notify (lambda (widget &rest ignore)
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
                 :notify (lambda (widget &rest ignore)
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
                 :value (cl-loop for group-value in (alist-get :groupby org-clock-agg--params)
                                 for sort-value in (alist-get :sort org-clock-agg--params)
                                 for sort-order-value in (alist-get :sort-order org-clock-agg--params)
                                 collect (list group-value sort-value sort-order-value))
                 :notify
                 (lambda (widget changed-widget &optional event)
                   (let ((group-value (mapcar #'car (widget-value widget)))
                         (sort-value (mapcar #'cadr (widget-value widget)))
                         (sort-order-value (mapcar #'caddr (widget-value widget))))
                     (setf (alist-get :groupby org-clock-agg--params) group-value)
                     (setf (alist-get :sort org-clock-agg--params) sort-value)
                     (setf (alist-get :sort-order org-clock-agg--params) sort-order-value)))
                 `(group
                   :value (outline-path total)
                   (menu-choice
                    :tag "Group"
                    :notify (lambda (widget _child &optional event)
                              (if-let* ((value (widget-value widget))
                                        (default-sort (alist-get
                                                       :default-sort
                                                       (alist-get value org-clock-agg-groupby-functions)))
                                        (parent (widget-get widget :parent)))
                                  (widget-value-set parent (list value default-sort)))
                              (widget-default-action widget event))
                    ,@(thread-last
                        org-clock-agg-groupby-functions
                        (seq-filter (lambda (groupby)
                                      (not (alist-get :hidden (cdr groupby)))))
                        (mapcar (lambda (groupby)
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

(defun org-clock-agg--render-controls ()
  "Render controls for the `org-clock-agg' buffer."
  (remove-overlays)
  (insert (propertize "* Parameters" 'face 'org-level-1) "\n")
  (org-clock-agg--render-controls-files)
  (insert "\n")
  (org-clock-agg--render-controls-date)
  (insert "\n\n")
  (org-clock-agg--render-controls-groupby)
  (insert "\n")
  (org-clock-agg--render-extra-params)
  (insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (org-clock-agg-refresh))
                 "Refresh")
  (insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (org-clock-agg-generate-report))
                 "Create function")
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

(defun org-clock-agg-render-tree-node-elems (node)
  "Render elements for the tree NODE.

NODE is one node of a tree, which is described in the function
`org-clock-agg--groupby'."
  (when-let ((elems (alist-get :elems (cdr node)))
             (widget-push-button-prefix "")
             (widget-push-button-suffix ""))
    (dolist (elem elems)
      (let ((elem-name
             (format
              "- [%s]--[%s] => %s : %s"
              (propertize
               (thread-last elem
                            (alist-get :start)
                            (seconds-to-time)
                            (format-time-string (cdr org-time-stamp-formats)))
               'face 'org-date)
              (propertize
               (thread-last elem
                            (alist-get :end)
                            (seconds-to-time)
                            (format-time-string (cdr org-time-stamp-formats)))
               'face 'org-date)
              (org-duration-from-minutes
               (/ (alist-get :duration elem) 60))
              (concat
               (when-let ((todo-keyword (substring-no-properties
                                         (org-element-property
                                          :todo-keyword
                                          (alist-get :headline elem)))))
                 (propertize
                  (concat todo-keyword " ") 'face
                  (if (eq (org-element-property :todo-type (alist-get :headline elem)) 'done)
                      'org-done 'org-todo)))
               (org-element-property :raw-value (alist-get :headline elem))))))
        (widget-create 'push-button
                       :elem elem
                       :notify (lambda (widget &rest ignore)
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
  (let ((level-face (nth (mod (1- level) 8)  org-level-faces))
        (level-string (make-string level ?*))
        (title-width (- (window-width) 40)))
    (insert
     (format (format "%%-%ds %%20s %%8s" title-width)
             (propertize (org-clock-agg--trim-string
                          (concat level-string " " (car node))
                          title-width)
                         'face level-face)
             (propertize
              (alist-get :readable-name (alist-get :groupby (cdr node)))
              'face 'org-clock-agg-group-face)
             (propertize
              (format-seconds
               org-clock-agg-duration-format
               (alist-get :total (cdr node)))
              'face 'org-clock-agg-duration-face))
     "\n")
    (when show-elems
      (org-clock-agg-render-tree-node-elems node)))
  (mapc (lambda (child)
          (org-clock-agg--render-tree-node child show-elems (1+ level)))
        (alist-get :children (cdr node))))

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

(defun org-clock-agg (from to files groupby sort sort-order extra-params)
  "Aggregate org-clock data.

The function creates an interactive buffer to configure the
aggregation and display the results.  If functions is called
non-interactively, intials parameters can be passed as arguments.

Use `org-clock-agg-exec' if you want to retrive the results
without the interactive buffer.

FROM and TO define the time range.  Both are `org-ql' time predicates,
that is a number of days (e.g. -7 for the last week) or a date
parseable by `parse-time-string'.

FILES is either 'org-agenda, a key of `org-clock-agg-files-preset' (in
which case the value of that variable is used) or a list of files.

GROUPBY is a list of keys of `org-clock-agg-groupby-functions'.  Each
function returns a list of groups for each entry; the result is a
tree.  SORT is a list of keys of `org-clock-agg-sort-functions' that
has to be the same length as GROUPBY.  Nth entry is the SORT list
defines the sort logic for the results of the Nth GROUPBY function.

SORT-ORDER has to be the same length as SORT.  If Nth entry is non-nil,
the sorting is reversed.

EXTRA-PARAMS is an alist of \"extra parameters\".  Possible keys are
defined by `org-clock-agg--extra-params-default' and
`org-clock-agg-extra-params'.  The built-in parameters are:
- `:show-elems' - whether to show raw elements for each group in the
  buffer.  An \"element\" is one org-clock record.
- `:add-ungrouped' - whether to add the \"Ungrouped\" group to the
  results.

`org-clock-agg-extra-params' can be used to define new parameters.
This is meant to be used by custom aggregation functions to control
their behavior in runtime.

See the mentioned variables for and the interactive buffer for the
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
        (org-clock-agg-refresh))
      (goto-char (point-min)))))

(provide 'org-clock-agg)
;;; org-clock-agg.el ends here
