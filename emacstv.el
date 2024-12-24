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

(defun emacstv-find-by-youtube-url (url)
	"Move point to the entry for URL.
Returns nil if not found."
	(let* ((id-re (regexp-quote (cond
															 ((string-match "youtu.be/\\(.*\\)" url) (match-string 1 url))
															 ((string-match "www.youtube.com/watch\\?\\(.*\\)" url)
																(assoc-default "v" (url-parse-args (match-string 1 url)) #'string=))
															 (t (error "Unknown URL pattern")))))
				 (pos
					(catch 'found
						(org-map-entries
						 (lambda ()
							 (when (string-match id-re (org-entry-get (point) "YOUTUBE_URL"))
								 (throw 'found (point))))
						 "YOUTUBE_URL={.}")
						nil)))
		(when pos
			(goto-char pos))))

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

(defun emacstv-export-rss ()
  (interactive)
  (with-current-buffer (find-file-noselect emacstv-index-org)
    ;; TODO: Consider sorting video.org so we don't have to elsewhere.
    (let ((data (sort (org-map-entries
                       (lambda ()
                         (let* ((end (save-excursion
                                       (org-end-of-subtree)))
                                (start (progn
                                         (org-end-of-meta-data t)
                                         (point)))
                                (body (if (< start end)
                                          (string-trim (buffer-substring start end))
                                        "")))
                           (append (org-entry-properties)
                                   `(("BODY" . ,body)))))
                       "LEVEL=1")
                      (lambda (a b)
                        (time-less-p
                         (date-to-time (or (map-elt b "DATE") ""))
                         (date-to-time (or (map-elt a "DATE") "")))))))
      (with-temp-file (expand-file-name "videos.rss"
                                        (file-name-directory emacstv-index-org))
        (insert (format "
<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">
  <channel>
    <title>Emacs TV</title>
    <link>https://emacs.tv</link>
    <description>Emacs videos</description>"))
        (dolist (entry data)
          (let ((title (or (map-elt entry "ITEM") ""))
                (url (or (map-elt entry "URL")
                         (map-elt entry "MEDIA_URL")
                         (map-elt entry "TOOBNIX_URL")
                         (map-elt entry "YOUTUBE_URL")
                         (map-elt entry "TRANSCRIPT_URL")
                         ""))
                (urls (seq-filter
                       (lambda (item) (string-suffix-p "_URL" (car item)))
                       entry))
                (timestamp (format-time-string "%a, %d %b %Y %H:%M:%S %z"
                                               (date-to-time (or (map-elt entry "DATE") ""))))
                (body (or (map-elt entry "BODY") "")))
            (insert (format "
    <item>
      <title>%s</title>
      <link>%s</link>
      <description><![CDATA[<div id=\"content\">%s</div>]]></description>
      <pubDate>%s</pubDate>
    </item>"
                            title
                            url
                            (if-let ((links (concat
                                             (mapconcat
                                              (lambda (url)
                                                (format "<a href=\"%s\">%s</a>"
                                                        (cdr url)
                                                        (downcase (substring (car url) 0 -4))))
                                              urls
                                              "&nbsp;·&nbsp;"))))
                                (concat links "<br><br>"
                                        body)
                              body)
                            (xml-escape-string timestamp)))))
        (insert "
  </channel>
</rss>")))))

(defun emacstv-add-from-youtube (url)
	"Add an entry for URL."
	(interactive (list (read-string "YouTube URL: "
																	(when (string-match "^https://\\(www\\.\\)?youtu"
																											(or (car kill-ring) ""))
																		(car kill-ring)))))
	(let* ((json-object-type 'alist)
				 (json-array-type 'list)
				 data)
		(with-current-buffer (url-retrieve-synchronously url)
			(set-buffer-multibyte t)
			(when (re-search-forward "ytInitialPlayerResponse *= *" nil t)
				(setq data (json-read))))
		(let-alist (alist-get 'videoDetails data)
			(unless (emacstv-find-by-youtube-url url)
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
