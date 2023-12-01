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
(require 'compat)
(require 'org)
(require 'org-ql)

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
    ;; Just to make the output more tolerable
    ;; (setf
    ;;  (plist-get (cadr (alist-get :headline headline)) :todo-keyword)
    ;;  (substring-no-properties
    ;;   (plist-get (cadr (alist-get :headline headline)) :todo-keyword)))
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
      (when (eq :hidden symbol)
        (setq hidden (cadr body)))
      (when (eq :readable-name symbol)
        (setq readable-name (cadr body)))
      (setq body (cddr body)))
    `(progn
       (defun ,func-name (elem)
         ,doc
         ,@body)
       (push (cons ',name '((:function . ,func-name)
                            (:hidden . ,hidden)
                            (:readable-name . ,(or readable-name
                                                   (symbol-name name)))))
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

(provide 'org-clock-agg)
;;; org-clock-agg.el ends here
