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

(defun org-clock-agg--parse-clocks (headline)
  (let ((contents (buffer-substring-no-properties
                   (org-element-property :contents-begin headline)
                   (org-element-property :contents-end headline))))
    (with-temp-buffer
      (insert contents)
      (let (res)
        (org-element-map (org-element-parse-buffer) 'clock
          (lambda (clock)
            (let ((start )
                  (end (time-convert
                        (org-timestamp-to-time (org-element-property :value clock) t)
                        'integer))))
            (push
             `((:start . ,(time-convert
                           (org-timestamp-to-time (org-element-property :value clock))
                           'integer))
               (:end . ,(time-convert
                         (org-timestamp-to-time (org-element-property :value clock) t)
                         'integer)))
             res))
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
    `((:headline . ,headline)
      (:tags . ,tags)
      (:file . ,file)
      (:outline-path . ,outline-path)
      (:category . ,category)
      (:clocks . ,(org-clock-agg--parse-clocks headline)))))

(defun org-clock-agg--query (from to files)
  (org-ql-query
    :select #'org-clock-agg--parse-headline
    :from files
    :where `(clocked :from ,from :to ,to)))


(defun org-clock-agg ()
  (interactive)
  ())

(provide 'org-clock-agg)
;;; org-clock-agg.el ends here
