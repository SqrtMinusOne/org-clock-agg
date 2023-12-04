;;; org-clock-agg.el --- Aggregate org-clock statistics  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.1") (org-ql "0.8-pre"))
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

;; TODO

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
  "Format string for durations in org-clock-agg views.

See `format-seconds' for the list of available format specifiers."
  :type 'string
  :group 'org-clock-agg)

(defcustom org-clock-agg-files-preset nil
  "Presets for the \"files\" parameter in org-clock-agg views."
  :type '(alist :key-type string :value-type (repeat string)))

(defface org-clock-agg-group-face
  '((t :inherit font-lock-comment-face))
  "Face for group names in org-clock-agg tree views."
  :group 'org-clock-agg)

(defface org-clock-agg-duration-face
  '((t :inherit font-lock-constant-face))
  "Face for durations in org-clock-agg tree views."
  :group 'org-clock-agg)

(defface org-clock-agg-param-face
  '((t :inherit font-lock-variable-name-face))
  "Face for parameters in org-clock-agg tree views."
  :group 'org-clock-agg)

;; Reset org-ql cache
(setq org-ql-cache (make-hash-table :weakness 'key))
(setq org-clock-agg-groupby nil)
(setq org-clock-agg-sort nil)

;;; Querying
(defun org-clock-agg--parse-clocks (headline)
  (let ((contents (buffer-substring-no-properties
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
          nil nil 'headline)
        res))))

(defun org-clock-agg--parse-headline ()
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
         (category (org-get-category)))
    (org-ql--add-markers headline)
    (cl-loop for clock in (org-clock-agg--parse-clocks headline)
             collect`(,@clock
                      (:headline . ,headline)
                      (:tags . ,tags)
                      (:file . ,file)
                      (:outline-path . ,outline-path)
                      (:category . ,category)))))

(defun org-clock-agg--query (from to files)
  (cl-loop for res in (org-ql-query
                        :select #'org-clock-agg--parse-headline
                        :from files
                        :where `(clocked :from ,from :to ,to))
           append res))

;;; Aggregation
(defvar org-clock-agg-groupby nil
  "Group by functions.")

(defvar org-clock-agg-sort nil
  "Sort functions.")

;; XXX This looks like reinventing the wheel... IDK.
(defmacro org-clock-agg--extract-params (body &rest params)
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
       (defun ,func-name (elem)
         ,@body)
       (push (cons ',name '((:symbol . ,name)
                            (:function . ,func-name)
                            (:hidden . ,hidden)
                            (:readable-name . ,readable-name)
                            (:default-sort . ,default-sort)))
             org-clock-agg-groupby))))

(cl-defmacro org-clock-agg-defsort (name &body body)
  (declare (indent defun)
           (doc-string 2))
  (let ((func-name (intern (concat "org-clock-agg--sort-" (symbol-name name))))
        readable-name)
    (org-clock-agg--extract-params body :readable-name)
    (unless readable-name
      (setq readable-name (symbol-name name)))
    `(progn
       (defun ,func-name (elems)
         ,@body)
       (push (cons ',name '((:function . ,func-name)
                            (:readable-name . ,readable-name)))
             org-clock-agg-sort))))

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

(org-clock-agg-defgroupby root-group
  "Return \"Root\".  Used for the root group."
  :readable-name "Root"
  :default-sort total
  :hidden t
  (list "Results"))

(org-clock-agg-defsort name
  "Sort by name."
  :readable-name "Name"
  (seq-sort-by (lambda (elem) (alist-get :name elem)) #'string-lessp elems))

(org-clock-agg-defsort total
  "Sort by total time spent."
  :readable-name "Total time"
  (seq-sort-by (lambda (elem) (alist-get :total elem)) #'> elems))

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
   #'> elem))

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
   #'> elem))

(defun org-clock-agg--groupby-apply (alist groups elem)
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

(defun org-clock-agg--groupby (elems groupby-list sort-list sort-order-list)
  (let (res)
    (dolist (elem elems)
      (let* ((group-symbols (cons 'root-group groupby-list))
             (sort-symbols (cons 'total sort-list))
             (sort-orders (cons nil sort-order-list))
             (groups
              (cl-loop for group-symbol in group-symbols
                       for sort-symbol in sort-symbols
                       for sort-order in sort-orders
                       for groupby = (alist-get group-symbol org-clock-agg-groupby)
                       for group-values = (funcall (alist-get :function groupby) elem)
                       append
                       (mapcar
                        (lambda (group-value)
                          (list group-value groupby sort-symbol sort-order))
                        group-values))))
        (setq res (org-clock-agg--groupby-apply res groups elem))))
    res))

(defun org-clock-agg--ungroup (tree)
  (cl-loop for tree-elem in tree
           append (alist-get :elems tree-elem)
           append (org-clock-agg--ungroup (alist-get :children elem))))

(defun org-clock-agg--groupby-sort (tree)
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
                       (funcall (thread-last org-clock-agg-sort
                                             (alist-get sort-symbol)
                                             (alist-get :function))
                                (cdr grouped)))
                 (when sort-order
                   (setf (cdr grouped) (reverse (cdr grouped))))
                 grouped)))
            (seq-sort-by
             (lambda (grouped)
               (thread-last org-clock-agg-groupby
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
  "Parameters for the current org-clock-agg buffer.")

(defvar-local org-clock-agg--elems nil
  "Elements for the current org-clock-agg buffer.")

(defvar-local org-clock-agg--tree nil
  "Tree for the current org-clock-agg buffer.")

(defun org-clock-agg-quit ()
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
  "Major mode for viewing org-clock-agg results."
  (outline-minor-mode 1))

(defun org-clock-agg--render-controls-files ()
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

(defun org-clock-agg--validate-date (widget)
  (let ((date (widget-value widget)))
    (unless (or (not (stringp date))
                (and
                 (not (string-match-p (rx bos (? "-") (+ digit) eos) date))
                 (not (and (decoded-time-year val)
                           (decoded-time-month val)
                           (decoded-time-day val)))))
      (widget-put widget :error "Enter number or date in format YYYY-MM-DD")
      widget)))

(defun org-clock-agg--render-controls-date ()
  (widget-create 'editable-field
                 :size 20
                 :format (concat (propertize "Date from: " 'face 'widget-button) "%v   ")
                 :value (let ((val (alist-get :from org-clock-agg--params)))
                          (if (numberp val)
                              (number-to-string val)
                            val))
                 :validate #'org-clock-agg--validate-date
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
                 :validate #'org-clock-agg--validate-date
                 :notify (lambda (widget &rest ignore)
                           (let ((val (widget-value widget)))
                             (when (string-match-p (rx bos (? "-") (+ digit) eos) val)
                               (setq val (string-to-number val)))
                             (setf (alist-get :to org-clock-agg--params) val)))))

(defun org-clock-agg--render-controls-groupby ()
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
                                                       (alist-get value org-clock-agg-groupby)))
                                        (parent (widget-get widget :parent)))
                                  (widget-value-set parent (list value default-sort)))
                              (widget-default-action widget event))
                    ,@(thread-last
                        org-clock-agg-groupby
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
                       org-clock-agg-sort))
                   (toggle :on "Reverse order" :off "Normal order"))))

(defun org-clock-agg--render-controls ()
  (remove-overlays)
  (insert (propertize "* Parameters" 'face 'org-level-1) "\n")
  (org-clock-agg--render-controls-files)
  (insert "\n")
  (org-clock-agg--render-controls-date)
  (insert "\n\n")
  (org-clock-agg--render-controls-groupby)
  (insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (org-clock-agg-refresh))
                 "Refresh")
  (insert "\n\n")
  (widget-setup))

(defun org-clock-agg--trim-string (string max-len)
  (let ((len (length string)))
    (if (> len max-len)
        (concat (substring string 0 (- max-len 3)) "...")
      string)))

(defun org-clock-agg--render-tree-elem (elem &optional level)
  (unless level
    (setq level 1))
  (let ((level-face (nth (mod (1- level) 8)  org-level-faces))
        (level-string (make-string level ?*))
        (title-width (- (window-width) 40)))
    (insert
     (format (format "%%-%ds %%20s %%8s" title-width)
             (propertize (org-clock-agg--trim-string
                          (concat level-string " " (car elem))
                          title-width)
                         'face level-face)
             (propertize
              (alist-get :readable-name (alist-get :groupby (cdr elem)))
              'face 'org-clock-agg-group-face)
             (propertize
              (format-seconds
               org-clock-agg-duration-format
               (alist-get :total (cdr elem)))
              'face 'org-clock-agg-duration-face))
     "\n"))
  (mapc (lambda (child)
          (org-clock-agg--render-tree-elem child (1+ level)))
        (alist-get :children (cdr elem))))

(defun org-clock-agg--parse-files (files)
  (cond ((eq files 'org-agenda)
         (org-agenda-files))
        ((member files (mapcar #'car org-clock-agg-files-preset))
         (alist-get files org-clock-agg-files-preset nil nil #'equal))
        (t files)))

(defun org-clock-agg-refresh ()
  (interactive)
  (cl-destructuring-bind (&key from to files groupby sort sort-order)
      (cl--alist-to-plist org-clock-agg--params)
    (let* ((files (org-clock-agg--parse-files files))
           (elems (org-clock-agg--query from to files))
           (tree (org-clock-agg--groupby elems groupby sort sort-order))
           (tree (org-clock-agg--groupby-sort tree)))
      (setq-local org-clock-agg--elems elems)
      (setq-local org-clock-agg--tree tree)
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (search-forward (format "* Results") nil 'noerror)
          (beginning-of-line)
          (delete-region (point) (point-max))
          (mapc #'org-clock-agg--render-tree-elem tree))))))

(defun org-clock-agg (from to files groupby sort sort-order)
  (interactive (list -7 0 'org-agenda nil nil nil))
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
                    (:sort-order . ,sort-order)))
      (let ((inhibit-read-only t))
        (org-clock-agg--render-controls)
        (org-clock-agg-refresh))
      (goto-char (point-min)))))

(provide 'org-clock-agg)
;;; org-clock-agg.el ends here
