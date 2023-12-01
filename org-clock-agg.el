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

(defface org-clock-agg-group-face
  '((t :inherit font-lock-comment-face))
  "Face for group names in org-clock-agg tree views."
  :group 'org-clock-agg)

(defface org-clock-agg-duration-face
  '((t :inherit font-lock-constant-face))
  "Face for durations in org-clock-agg tree views."
  :group 'org-clock-agg)

(defcustom org-clock-agg-duration-format "%h:%.2m"
  "Format string for durations in org-clock-agg views.

See `format-seconds' for the list of available format specifiers."
  :type 'string
  :group 'org-clock-agg)

;; Reset org-ql cache
(setq org-ql-cache (make-hash-table :weakness 'key))

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

(cl-defmacro org-clock-agg-defgroupby (name doc &body body)
  (declare (indent defun)
           (doc-string 2))
  (let ((func-name (intern (concat "org-clock-agg--groupby-" (symbol-name name))))
        readable-name
        hidden)
    ;; Parse keyword arguments in BODY
    (while-let ((symbol (and
                         (member (car-safe body) '(:hidden :readable-name))
                         (car-safe body))))
      (when (eq :hidden symbol) (setq hidden (cadr body)))
      (when (eq :readable-name symbol) (setq readable-name (cadr body)))
      (setq body (cddr body)))
    `(progn
       (defun ,func-name (elem)
         ,doc
         ,@body)
       (unless readable-name
         (setq readable-name (symbol-name name)))
       (push (cons ',name '((:function . ,func-name)
                            (:hidden . ,hidden)
                            (:readable-name . ,readable-name)))
             org-clock-agg-groupby))))

(org-clock-agg-defgroupby category
  "Group org-clock entries by category."
  (list (alist-get :category elem)))

(org-clock-agg-defgroupby org-file
  "Group org-clock entries by file in `org-directory'."
  (list
   (file-relative-name (alist-get :file elem)
                       (directory-file-name org-directory))))

(org-clock-agg-defgroupby outline-path
  "Group org-clock entries by outline path."
  (alist-get :outline-path elem))

(org-clock-agg-defgroupby root--group
  "Return \"Root\".  Used for the root group."
  :hidden t
  (list "Root"))

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
      (let* ((group-symbols (cons 'root--group groupby-list))
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

(defun org-clock-agg--groupby-sort (tree)
  (setq tree (seq-sort-by (lambda (elem)
                            (alist-get :total elem))
                          #'> tree))
  (dolist (elem tree)
    (let ((children (alist-get :children elem)))
      (when children
        (setf (alist-get :children elem)
              (org-clock-agg--groupby-sort children)))))
  tree)

;; View results
(defun org-clock-agg-quit ()
  (interactive)
  (quit-window t))

(defvar org-clock-agg-tree-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'org-clock-agg-quit)
    (define-key keymap (kbd "<tab>") #'outline-toggle-children)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'org-clock-agg-quit
        "<tab>" #'outline-toggle-children))
    keymap))

(define-derived-mode org-clock-agg-tree-mode special-mode "Org Clock Agg Tree"
  "Major mode for viewing org-clock-agg results."
  (outline-minor-mode 1))

(defun org-clock-agg--render-tree-elem (elem &optional level)
  (unless level
    (setq level 1))
  (let ((level-face (nth (mod (1- level) 8)  org-level-faces))
        (level-string (make-string level ?*)))
    (insert
     (format "%-50s %20s %8s"
             (propertize (concat level-string " " (car elem))
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

(defun org-clock-agg--render-tree (tree)
  (let ((buffer (generate-new-buffer "*org-clock-agg*")))
    (with-current-buffer buffer
      (org-clock-agg-tree-mode)
      (let ((inhibit-read-only t))
        (mapc #'org-clock-agg--render-tree-elem tree))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(provide 'org-clock-agg)
;;; org-clock-agg.el ends here
