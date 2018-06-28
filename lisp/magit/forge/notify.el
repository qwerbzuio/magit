;;; magit/forge/notify.el --- forge notify support  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'magit/forge)

;;; Class

(defclass magit-forge-notification (magit-forge-object)
  ((closql-class-prefix       :initform "magit-")
   (closql-table              :initform notification)
   (closql-primary-key        :initform id)
   (id                        :initarg :id)
   (project                   :initarg :project)
   (forge                     :initarg :forge)
   (desc                      :initarg :desc)
   (reason                    :initarg :reason)
   (unread-p                  :initarg :unread-p)
   (last-read                 :initarg :last-read)
   (updated                   :initarg :updated)
   (title                     :initarg :title)
   (type                      :initarg :type)
   (topic                     :initarg :topic)))

;;; Pull

(cl-defmethod magit-forge--pull-notifications
  ((class (subclass magit-github-project)) githost)
  (emacsql-with-transaction (magit-db)
    (magit-sql [:drop-table-if-exists notification])
    (magit-sql [:create-table notification $S1]
               (cdr (assq 'notification magit--db-table-schemata)))
    (if-let (spec (assoc githost magit-forge-alist))
        (pcase-let ((`(,_ ,apihost ,forge ,_) spec))
          (dolist (n (magit-forge--fetch-notifications class apihost))
            (let-alist n
              (let* ((type (intern (downcase .subject.type)))
                     (type (if (eq type 'pullrequest) 'pullreq type))
                     (owner .repository.owner.login)
                     (name .repository.name)
                     (project-id (magit-forge--object-id
                                  class forge apihost owner name))
                     (project-args (list githost owner name))
                     (project (magit-forge-get-project project-args))
                     (topic-nr (and (string-match "[0-9]*\\'" .subject.url)
                                    (string-to-number
                                     (match-string 0 .subject.url))))
                     (notification-id (format "%s" .id)))
                ;; (unless project
                ;;   (setq project (magit-forge-get-project project-args nil t))
                ;;   (magit-forge--pull-issues project)
                ;;   (magit-forge--pull-pullreqs project))
                (when-let (topic
                           (and project
                                (or (magit-forge-get-issue project topic-nr)
                                    (magit-forge-get-pullreq project topic-nr))))
                  (oset topic unread-p t));.unread))
                (closql-insert
                 (magit-db)
                 (magit-forge-notification
                  :id           notification-id
                  :project      (and project project-id)
                  :forge        forge
                  :desc         (format "%s:%s/%s" forge owner name)
                  :reason       (intern (downcase .reason))
                  :unread-p     .unread
                  :last-read    .last_read_at
                  :updated      .updated_at
                  :title        .subject.title
                  :type         type
                  :topic        topic-nr))))))
      (error "No entry for %S in magit-forge-alist" githost))))

(cl-defmethod magit-forge--fetch-notifications
  ((_class (subclass magit-github-project)) apihost)
  (magit--ghub-get nil "/notifications" '((all . "true")) :host apihost))

;;; Core

(cl-defmethod magit-forge-get-project ((notify magit-forge-notification)
                                       &optional demand)
  "Return the object for the project that NOTIFY belongs to."
  (when-let (id (or (oref notify project)
                    (and demand
                         (oref notify project-id))))
    (closql-get (magit-db) id 'magit-forge-project)))

;;; Commands

;;;###autoload
(defun magit-notification-browse () (interactive)) ; TODO

(defalias 'magit-notification-visit 'magit-notification-browse)

;;; Utilities

(defun magit-forge--list-notifications-all ()
  (closql-query (magit-db) nil nil 'magit-forge-notification))

(defun magit-forge--list-notifications-unread ()
  (mapcar (lambda (row)
            (closql--remake-instance 'magit-forge-notification (magit-db) row))
          (magit-sql [:select * :from notification
                      :where (notnull unread-p)])))

;;; Sections

(defun magit-notification-at-point ()
  (magit-section-when notification))

(defvar magit-notification-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'magit-notification-browse)
    (define-key map [remap magit-visit-thing]  'magit-notification-visit)
    map))

(defun magit-insert-notifications ()
  (when-let (ns (magit-forge--list-notifications-all)) ; FIXME
    (magit-insert-section (dashboard) ; FIXME
      (magit-insert-heading "Notifications:")
      (pcase-dolist (`(,_ . ,ns) (--group-by (oref it project) ns))
        (let ((prj (magit-forge-get-project (car ns))))
          (magit-insert-section (project prj)
            (magit-insert-heading
              (propertize (format "%s/%s:" (oref prj owner) (oref prj name))
                          'face 'bold))
            (dolist (notify ns)
              (with-slots (type topic title) notify
                (cond
                 ((when-let ((issue
                              (and (eq type 'issue)
                                   (magit-forge-get-issue prj topic))))
                    (magit-insert-issue issue) t))
                 ((when-let ((pullreq
                              (and (eq type 'pullreq)
                                   (magit-forge-get-pullreq prj topic))))
                    (magit-insert-pullreq pullreq) t))
                 (t (magit-insert-notification notify)))))))))))

(defun magit-insert-notification (notify &optional format)
  (with-slots (type number topic title) notify
    (if (memq type '(issue pullreq))
        (magit-insert-section (notification notify)
          (insert (format (or format "%s %s\n")
                          (propertize (format "#%s" topic) 'face 'magit-dimmed)
                          (propertize title 'face 'magit-dimmed))))
      (magit-insert-section (notification notify)
        (insert (propertize (format "(%s) %s\n" type title)
                            'face 'magit-dimmed))))))

;;; Dashboard

(defvar magit-forge-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-forge-dashboard-mode'.")

(define-derived-mode magit-forge-dashboard-mode magit-mode "Magit Dashboard"
  ""
  :group 'magit-refs
  (hack-dir-local-variables-non-file-buffer))

(defun magit-forge-dashboard-refresh-buffer ()
  (magit-insert-notifications))

;;;###autoload
(defun magit-forge-dashboard ()
  (interactive)
  (magit-mode-setup #'magit-forge-dashboard-mode))

;;; _
(provide 'magit/forge/notify)
;;; magit/forge/notify.el ends here
