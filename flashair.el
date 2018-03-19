;;; flashair.el --- flashair interface -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: fun

;; flashair.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; flashair.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'url)

(defvar flashair-address "192.168.1.6o6"
  "The IP address (or name) of the Flashair card.")

(defvar flashair-directory "~/pics/gh4/"
  "The directory pictures will be downloaded to.")

(defun flashair-process-live-p (process)
  (message "Status: %s" (process-status process))
  (memq (process-status process) '(open connect)))

(defun flashair-delete-process (process)
  (message "Deleting process")
  (set-process-sentinel process nil)
  (delete-process process))

(defvar flashair-download-process nil)
(defvar flashair-main-process nil)

(defun flashair ()
  "Start monitoring flashair cards for images."
  (interactive)
  (when flashair-main-process
    (cancel-timer flashair-main-process))
  (setq flashair-main-process
	(run-at-time 1 10 'flashair-keepalive)))

(defun flashair-keepalive ()
  (when (or (not flashair-download-process)
	    (and (not (flashair-process-live-p (car flashair-download-process)))
		 (not (memq (cdr flashair-download-process) timer-list))))
    (flashair-probe)))

(defun flashair-retrieve (url timeout callback)
  (setq url-http-open-connections (make-hash-table :test 'equal :size 17))
  (let* ((url-http-attempt-keepalives nil)
	 (timer nil)
	 (process
	  (get-buffer-process
	   (url-retrieve
	    url
	    (lambda (status)
	      (when timer
		(cancel-timer timer))
	      (funcall callback status))
	    nil t))))
    (setq timer
	  (run-at-time
	   timeout nil
	   (lambda ()
	     (when (flashair-process-live-p process)
	       (flashair-delete-process process)
	       (message "No response")
	       (flashair-probe)))))
    (setq flashair-download-process (cons process timer))))

(defun flashair-probe ()
  (flashair-retrieve (format "http://%s/DCIM/" flashair-address)
		  1 'flashair-check-directory))

(defun flashair-check-directory (status)
  (let ((buffer (current-buffer)))
    (message "Checking... %s" status)
    (unless (eq (car status) :error)
      (goto-char (point-min))
      (let ((directories nil))
	(while (re-search-forward "fname.:\"\\([^\"]+\\)\"" nil t)
	  (push (match-string 1) directories))
	(flashair-download-directories directories)))
    (kill-buffer buffer)))

(defun flashair-download-directories (directories &optional previous)
  (message "Downloading %s" directories)
  (let ((url (format "http://%s/DCIM/%s/" flashair-address (pop directories))))
    (flashair-retrieve
     url 10
     (lambda (status)
       (let ((buffer (current-buffer)))
	 (message "Directory status %s" status)
	 (unless (eq (car status) :error)
	   (let ((files nil)
		 (to-download previous))
	     (while (re-search-forward "fname.:\"\\([^\"]+\\)\"" nil t)
	       (let ((file (match-string 1))
		     (case-fold-search t))
		 (when (string-match "\\.jpg\\'" file)
		   (push file files))))
	     (dolist (file files)
	       (if (file-exists-p
		    (expand-file-name file flashair-directory))
		   ;; If we've seen any of the files in this directory
		   ;; before, then there's no need to check previous
		   ;; directories for non-transferred files.
		   (setq directories nil)
		 (push (cons url file) to-download)))
	     (cond
	      (directories
	       (flashair-download-directories directories to-download))
	      (to-download
	       (flashair-download-images to-download)))))
	 (kill-buffer buffer))))))

(defun flashair-download-images (images)
  (message "Downloading %s (of %s)..." (cdar images) (length images))
  (let ((image (pop images)))
    (flashair-retrieve
     (concat (car image) (cdr image)) 20
     (lambda (status)
       (let ((buffer (current-buffer)))
	 (message "File status %s" status)
	 (unless (eq (car status) :error)
	   (goto-char (point-min))
	   (when (re-search-forward "Content-Length: \\([0-9]+\\)" nil t)
	     (let ((content-length (string-to-number (match-string 1))))
	       (message "Downloaded %s" content-length)
	       (when (and (search-forward "\n\n" nil t)
			  (= content-length
			     (- (point-max) (point))))
		 ;; We have downloaded the correct length.
		 (write-region (point) (point-max)
			       (expand-file-name (cdr image)
						 flashair-directory))
		 (if images
		     (flashair-download-images images)
		   (flashair-probe))))))
	 (kill-buffer buffer))))))

(provide 'flashair)

;;; flashair.el ends here
