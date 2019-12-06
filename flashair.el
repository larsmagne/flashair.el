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

(defvar flashair-address nil
  "The IP address (or name) of the Flashair card.")

(defvar flashair-directory nil
  "The directory pictures will be downloaded to.")

(defvar flashair-processing-command nil
  "If non-nil, this should be a shell script that takes two parameters.
The first is the input image file name and the second is the
output name.  This is meant for doing automatic transforms on the
images that are downloaded.")

(defun flashair-process-live-p (process)
  (memq (process-status process) '(open connect)))

(defun flashair-delete-process (process)
  (set-process-sentinel process nil)
  (delete-process process))

(defvar flashair-download-process nil)
(defvar flashair-main-process nil)
(defvar flashair-buffer nil)

(defun flashair ()
  "Start monitoring flashair cards for images."
  (interactive)
  (flashair-cancel)
  (setq flashair-main-process
	(run-at-time 1 1 'flashair-keepalive)))

(defun flashair-cancel ()
  "Cancel Flashair monitoring."
  (interactive)
  (when flashair-download-process
    (delete-process (car flashair-download-process))
    (when (cdr flashair-download-process)
      (cancel-timer (cdr flashair-download-process)))
    (setq flashair-download-process nil))
  (when flashair-main-process
    (cancel-timer flashair-main-process)
    (setq flashair-main-process nil)))

(defun flashair-buffer ()
  "Monitor a Flashair for images and insert any images into the current buffer."
  (interactive)
  (setq flashair-buffer (current-buffer))
  (flashair))

(defun flashair-keepalive ()
  (unless (buffer-live-p flashair-buffer)
    (flashair-cancel))
  (flashair-probe))

(defun flashair-retrieve (url timeout callback)
  (with-current-buffer (get-buffer-create "*flashair*")
    (goto-char (point-max))
    (insert (format "%s %s\n" (format-time-string "%FT%T")
		    url)))
  ;; Lingering connections to a device that isn't there just gets in
  ;; the way.
  (maphash
   (lambda (key value)
     (when (equal (car key) flashair-address)
       (dolist (proc value)
	 (set-process-sentinel proc #'ignore)
	 (delete-process proc))
       (setf (gethash key url-http-open-connections) nil)))
   url-http-open-connections)
  (let* ((url-http-attempt-keepalives nil)
	 (timer nil)
	 buffer process)
    (setq buffer
	  (ignore-errors
	    (url-retrieve
	     url
	     (lambda (status)
	       (when timer
		 (cancel-timer timer))
	       (funcall callback status)
	       (when (buffer-live-p buffer)
		 (kill-buffer buffer)))
	     nil t)))
    (when buffer
      (setq process (get-buffer-process buffer))
      (setq timer
	    (run-at-time
	     timeout nil
	     (lambda ()
	       (when (flashair-process-live-p process)
		 (flashair-delete-process process)
		 (kill-buffer buffer)
		 (flashair-probe)))))
      (setq flashair-download-process (cons process timer)))))

(defun flashair-probe ()
  (when (or (not flashair-download-process)
	    (and (not (flashair-process-live-p (car flashair-download-process)))
		 (not (memq (cdr flashair-download-process) timer-list))))
    (flashair-probe-1)))

(defun flashair-probe-1 ()
  (let ((process (start-process "ping" nil "ping"
				"-c" "1" flashair-address)))
    (message (format-time-string "%FT%T Pinging"))
    (setq flashair-download-process (cons process nil))
    (set-process-sentinel
     process
     (lambda (process change)
       (when (string-match "finished" change)
	 (set-process-sentinel process nil)
	 (message (format-time-string "%FT%T Retrieving"))
	 (flashair-retrieve (format "http://%s/DCIM/" flashair-address)
			    10 'flashair-check-directory))))))

(defun flashair-check-directory (status)
  (let ((buffer (current-buffer)))
    (unless (eq (car status) :error)
      (goto-char (point-min))
      (let ((directories nil))
	(while (re-search-forward "fname.:\"\\([^\"]+\\)\"" nil t)
	  (push (match-string 1) directories))
	(flashair-download-directories directories)))
    (kill-buffer buffer)))

(defun flashair-download-directories (directories &optional previous)
  (let ((url (format "http://%s/DCIM/%s/" flashair-address (pop directories))))
    (flashair-retrieve
     url 10
     (lambda (status)
       (let ((buffer (current-buffer)))
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
	 (unless (eq (car status) :error)
	   (goto-char (point-min))
	   (when (re-search-forward "Content-Length: \\([0-9]+\\)" nil t)
	     (let ((content-length (string-to-number (match-string 1))))
	       (message "Downloaded %s (%d bytes)" (cdr image) content-length)
	       (when (and (search-forward "\n\n" nil t)
			  (= content-length
			     (- (point-max) (point))))
		 (let ((file (expand-file-name (cdr image)
					       flashair-directory)))
		   ;; We have downloaded the correct length.
		   (write-region (point) (point-max) file
				 nil 'nomsg)
		   (when (buffer-live-p flashair-buffer)
		     (flashair-insert file)))
		 (if images
		     (flashair-download-images images)
		   (flashair-probe))))))
	 (kill-buffer buffer))))))

(defun flashair-insert (file)
  (let ((new (format "/tmp/n-%s" (file-name-nondirectory file))))
    (if flashair-processing-command
	(call-process flashair-processing-command nil nil nil file new)
      (setq new file))
    (with-current-buffer flashair-buffer
      (let ((edges (window-inside-pixel-edges
		    (get-buffer-window (current-buffer) t))))
	(save-excursion
	  (goto-char (point-max))
	  (insert-image
	   (create-image
	    new (flashair--image-type) nil
	    :max-width
	    (truncate
	     (* 0.7 (- (nth 2 edges) (nth 0 edges))))
	    :max-height
	    (truncate
	     (* 0.5 (- (nth 3 edges) (nth 1 edges)))))
	   (format "<img src=%S>" new))
	  (insert "\n\n\n\n"))))))

(defun flashair--image-type ()
  (if (or (and (fboundp 'image-transforms-p)
	       (image-transforms-p))
	  (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))

(provide 'flashair)

;;; flashair.el ends here
