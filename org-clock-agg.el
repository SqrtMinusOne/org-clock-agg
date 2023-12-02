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

(defmacro org-clock-agg--extract-params (body &rest params)
  `(while-let ((symbol (and
                        (member (car-safe body) ',params)
                        (car-safe body))))
     ,@(mapcar
        (lambda (param)
          `(when (eq symbol ,param)
             (setq ,(intern (substring (symbol-name param) 1)) (cadr body))))
        params)
     (setq body (cddr body))))

(cl-defmacro org-clock-agg-defgroupby (name doc &body body)
  (declare (indent defun)
           (doc-string 2))
  (let ((func-name (intern (concat "org-clock-agg--groupby-" (symbol-name name))))
        readable-name
        hidden)
    ;; Parse keyword arguments in BODY
    (org-clock-agg--extract-params body :readable-name :hidden)
    (unless readable-name
      (setq readable-name (symbol-name name)))
    `(progn
       (defun ,func-name (elem)
         ,doc
         ,@body)
       (push (cons ',name '((:function . ,func-name)
                            (:hidden . ,hidden)
                            (:readable-name . ,readable-name)))
             org-clock-agg-groupby))))

(org-clock-agg-defgroupby category
  "Group org-clock entries by category."
  :readable-name "Category"
  (list (alist-get :category elem)))

(org-clock-agg-defgroupby org-file
  "Group org-clock entries by file in `org-directory'."
  :readable-name "Org file"
  (list
   (file-relative-name (alist-get :file elem)
                       (directory-file-name org-directory))))

(org-clock-agg-defgroupby outline-path
  "Group org-clock entries by outline path."
  :readable-name "Outline path"
  (alist-get :outline-path elem))

(org-clock-agg-defgroupby root-group
  "Return \"Root\".  Used for the root group."
  :readable-name "Root"
  :hidden t
  (list "Results"))

(defun org-clock-agg--groupby-apply (alist groups elem)
  (let* ((key (caar groups))
         (groupby (cdar groups))
         (rest (cdr groups))
         (duration (alist-get :duration elem))
         (prev-val (alist-get key alist nil nil #'equal)))
    (when key
      (setf (alist-get key alist nil nil #'equal)
            `((:total . ,(+ duration (or (alist-get :total prev-val) 0)))
              (:groupby . ,groupby)
              (:children . ,(org-clock-agg--groupby-apply
                             (alist-get :children prev-val) rest elem))
              (:elems . ,(if rest
                             (alist-get :elems prev-val)
                           (cons elem (alist-get :elems prev-val))))))))
  alist)

(defun org-clock-agg--groupby (elems groupby-list)
  (let (res)
    (dolist (elem elems)
      (let* ((group-symbols (cons 'root-group groupby-list))
             (groups
              (cl-loop for group-symbol in group-symbols
                       for groupby = (alist-get group-symbol org-clock-agg-groupby)
                       for group-values = (funcall (alist-get :function groupby) elem)
                       append
                       (mapcar
                        (lambda (group-value)
                          (cons group-value groupby))
                        group-values))))
        (setq res (org-clock-agg--groupby-apply res groups elem))))
    res))

(defun org-clock-agg--groupby-sort (tree sort)
  (setq tree (seq-sort-by (lambda (elem)
                            (alist-get :total elem))
                          #'> tree))
  (dolist (elem tree)
    (let ((children (alist-get :children elem)))
      (when children
        (setf (alist-get :children elem)
              (org-clock-agg--groupby-sort children sort)))))
  tree)

;; View & manage results
(defvar-local org-clock-agg--params nil
  "Parameters for the current org-clock-agg buffer.")

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

(defun org-clock-agg--render-controls ()
  (remove-overlays)
  (insert (propertize "* Parameters" 'face 'org-level-1) "\n")
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
                     (editable-field :tag "File" :value "")))))
  (insert "\n")

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
                             (setf (alist-get :to org-clock-agg--params) val))))
  (insert "\n\n")

  (insert (propertize "Group by: " 'face 'widget-button) "\n")
  (widget-create 'editable-list
                 :tag "Group by"
                 :entry-format "%i %d %v"
                 :value (alist-get :groupby org-clock-agg--params)
                 :notify
                 (lambda (widget &rest ignore)
                   (setf (alist-get :groupby org-clock-agg--params)
                         (widget-value widget)))
                 `(menu-choice
                   :tag "Group"
                   ,@(thread-last
                       org-clock-agg-groupby
                       (seq-filter (lambda (groupby)
                                     (not (alist-get :hidden (cdr groupby)))))
                       (mapcar (lambda (groupby)
                                 (let ((name (car groupby))
                                       (readable-name (alist-get :readable-name (cdr groupby))))
                                   `(item :tag ,readable-name
                                          :value ,name
                                          :menu-tag ,readable-name)))))))
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
  (cl-destructuring-bind (&key from to files groupby sort)
      (cl--alist-to-plist org-clock-agg--params)
    (let* ((files (org-clock-agg--parse-files files))
           (elems (org-clock-agg--query from to files))
           (tree (org-clock-agg--groupby elems groupby))
           (tree (org-clock-agg--groupby-sort tree sort)))
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (search-forward (format "* Results") nil 'noerror)
          (beginning-of-line)
          (delete-region (point) (point-max))
          (mapc #'org-clock-agg--render-tree-elem tree))))))

(defun org-clock-agg (from to files groupby sort)
  (interactive (list -7 0 'org-agenda nil nil))
  (let* ((buffer (generate-new-buffer "*org-clock-agg*")))
    (switch-to-buffer-other-window buffer)

    (with-current-buffer buffer
      (org-clock-agg-tree-mode)
      (setq-local org-clock-agg--params
                  `((:from . ,from)
                    (:to . ,to)
                    (:files . ,files)
                    (:groupby . ,groupby)
                    (:sort . ,sort)))
      (let ((inhibit-read-only t))
        (org-clock-agg--render-controls)
        (org-clock-agg-refresh))
      (goto-char (point-min)))))

(provide 'org-clock-agg)
;;; org-clock-agg.el ends here
