;;; emacstv.el --- Index Emacs-related videos   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(defvar emacstv-index-org (expand-file-name "videos.org" (file-name-directory (or load-file-name (buffer-file-name))))
	"*Where the data is stored.")

(defun emacstv-export-json ()
	(interactive)
	(with-current-buffer (find-file-noselect emacstv-index-org)
		(let ((data (org-map-entries
								 (lambda ()
									 (let* ((end (save-excursion (org-end-of-subtree)))
													(beg (progn (org-end-of-meta-data t) (point)))
													(desc (if (< beg end) (string-trim (buffer-substring beg end)) "")))
										 (append (org-entry-properties)
														 `(("DESCRIPTION" . ,desc)))))
								 "LEVEL=1")))
			(with-temp-file (expand-file-name "videos.json" (file-name-directory emacstv-index-org))
				(insert (json-encode data))))))

(defun emacstv-add-from-youtube (url)
	(interactive "MYouTube URL: ")
	(let* ((json-object-type 'alist)
				 (json-array-type 'list)
				 data)
		(with-current-buffer (url-retrieve-synchronously url)
			(set-buffer-multibyte t)
			(when (re-search-forward "ytInitialPlayerResponse *= *" nil t)
				(setq data (json-read))))
		(let-alist (alist-get 'videoDetails data)
			(if (org-find-property "YOUTUBE_URL" url)
					(goto-char (org-find-property "YOUTUBE_URL" url))
				(unless .title
					(error "Could not get video details for %s" url))
				(goto-char (point-max))
				(unless (bolp) (insert "\n"))
				(insert "* " .title "\n")
				(org-entry-put (point) "YOUTUBE_URL" url)
				(insert (org-ascii--indent-string .shortDescription 2) "\n\n"))
			(org-entry-put (point) "DATE" (let-alist data .microformat.playerMicroformatRenderer.publishDate))
			(org-entry-put (point) "SPEAKERS" .author)
			(org-entry-put (point) "DURATION" (format-seconds "%02h:%z%02m:%02s" (string-to-number .lengthSeconds))))))

(defun emacstv-add-from-org (&optional beg end)
	"Add the link at point.
If a region is active, add all the YouTube links in that region."
	(interactive (list (when (region-active-p) (region-beginning))
										 (when (region-active-p) (region-end))))
	(when (derived-mode-p 'org-mode)
		(if (and beg end)
				(progn
					(goto-char beg)
					(while (re-search-forward org-any-link-re end t)
						(when (org-element-lineage (org-element-context) 'link t)
							(let ((link (org-element-property :raw-link (org-element-context))))
								(when (string-match "youtu\\.?be" link)
									(with-current-buffer (find-file-noselect emacstv-index-org)
										(condition-case nil
												(emacstv-add-from-youtube link)
											(error nil))
										(display-buffer-in-side-window (current-buffer) nil)))))))
			;; add the current link
			(let ((link (org-element-property :raw-link (org-element-context))))
				(when (string-match "youtu\\.?be" link)
					(with-current-buffer (find-file-noselect emacstv-index-org)
						(emacstv-add-from-youtube link)
						(display-buffer-in-side-window (current-buffer) nil)))))))

(provide 'emacstv)
;;; emacstv.el ends here
