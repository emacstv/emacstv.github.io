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

(defun emacstv-youtube-id (url)
	"Return the video ID for URL."
	(cond
	 ((string-match "youtu.be/\\(.*\\)" url) (match-string 1 url))
	 ((string-match "youtube.com/live/\\(.*?\\)\\(\\?.+\\|$\\)" url) (match-string 1 url))
	 ((string-match "www.youtube.com/watch\\?\\(.*\\)" url)
		(car (assoc-default "v" (url-parse-query-string (match-string 1 url)) #'string=)))
	 (t nil)))

(defun emacstv-vimeo-id (url)
	"Return the video ID for URL."
	(cond
	 ((string-match "vimeo\\.com/\\([0-9]+\\)" url) (match-string 1 url))
	 (t nil)))

(defun emacstv-find-by-generic-url (field id-func url)
	(when-let*
			((id (funcall id-func url))
			 (id-re (regexp-quote id))
			 (pos
				(catch 'found
					(org-map-entries
					 (lambda ()
						 (when (string-match id-re (org-entry-get (point) field))
							 (throw 'found (point))))
					 (concat field "={.}"))
					nil)))
		(goto-char pos)))

(defun emacstv-find-by-media-url (url)
	"Move point to the entry for URL.
Returns nil if not found."
	(emacstv-find-by-generic-url "MEDIA_URL" #'identity url))

(defun emacstv-find-by-youtube-url (url)
	"Move point to the entry for URL.
Returns nil if not found."
	(emacstv-find-by-generic-url "YOUTUBE_URL" #'emacstv-youtube-id url))

(defun emacstv-find-by-vimeo-url (url)
	"Move point to the entry for URL.
Returns nil if not found."
	(emacstv-find-by-generic-url "VIMEO_URL" #'emacstv-vimeo-id url))

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
    (let ((data (org-map-entries
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
                 "LEVEL=1")))
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
                                               (date-to-time (or (map-elt entry "DATE") ""))
                                               "UTC"))
                (body (or (map-elt entry "BODY") "")))
            (insert (format "
    <item>
      <title>%s</title>
      <link>%s</link>
      <description><![CDATA[<div id=\"content\">%s</div>]]></description>
      <pubDate>%s</pubDate>
    </item>"
                            (xml-escape-string title)
                            (xml-escape-string url)
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

(defun emacstv-build ()
	(interactive)
	(emacstv-sort-by-newest-first)
	(emacstv-export-rss)
	(emacstv-export-json)
	(emacstv-count-entries))

(defun emacstv-find-by-url (url)
	"Go to the entry that for URL."
	;; TODO: This could be more efficient someday.
	(or
	 (emacstv-find-by-youtube-url url)
	 (emacstv-find-by-vimeo-url url)
	 (emacstv-find-by-media-url url)))

(defun emacstv-add-video-object (video)
	"VIDEO should be an alist."
	(with-current-buffer (find-file-noselect emacstv-index-org)
		(unless (emacstv-find-by-url (emacstv-video-url video))
			(unless (or (assoc-default "ITEM" video #'string=)
									(assoc-default 'ITEM video))
				(error "Could not get video details for %s" video))
			(goto-char (point-max))
			(unless (bolp) (insert "\n"))
			(insert "* " (or (assoc-default "ITEM" video #'string=)
											 (assoc-default 'ITEM video)) "\n")
			(when (or (assoc-default "DESCRIPTION" video #'string=)
								(assoc-default 'DESCRIPTION video))
				(insert (org-ascii--indent-string
								 (or (assoc-default "DESCRIPTION" video #'string=)
										 (assoc-default 'DESCRIPTION video)) 2)
								"\n\n")))
			;; set the properties if specified
			(dolist (prop '("DATE" "DURATION" "YOUTUBE_URL" "TOOBNIX_URL" "VIMEO_URL" "SPEAKERS" "URL"))
				(when (and (or (assoc-default prop video #'string=)
											 (assoc-default (intern prop) video))
									 (not (org-entry-get (point) prop)))
					(org-entry-put
					 (point) prop
					 (or (assoc-default prop video #'string=)
							 (assoc-default (intern prop) video)))))))

(defun emacstv-add-from-youtube (url)
	"Add an entry for URL."
	(interactive (list (read-string "YouTube URL: "
																	(let ((clipboard (current-kill 0 t)))
																		(when (string-match "^https://\\(www\\.\\)?youtu"
																												clipboard)
																			clipboard)))))
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
			(org-entry-put (point) "DURATION" (emacstv-format-seconds (string-to-number .lengthSeconds))))))

;; useful for extracting from YouTube:
;; from channel page:
;; [...document.querySelectorAll('a.ytd-rich-grid-media#video-title-link')].map((o) => `- [[${o.href}][${o.getAttribute('title')}]]\n`).join('')
;; from playlist:
;; [...document.querySelectorAll('a.ytd-playlist-panel-video-renderer#wc-endpoint')].map((o) => `- [[${o.href}][${o.querySelector('#video-title').getAttribute('title')}]]\n`).join('')
;; other playlist view:
;; [...document.querySelectorAll('a.ytd-playlist-video-renderer#video-title')].map((o) => `- [[${o.href}][${o.getAttribute('title')}]]\n`).join('')
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
												(unless (emacstv-find-by-youtube-url link)
													(emacstv-add-from-youtube link))
											(error nil))
										(display-buffer (current-buffer) nil)))))))
			;; add the current link
			(let ((link (org-element-property :raw-link (org-element-context))))
				(when (string-match "youtu\\.?be" link)
					(with-current-buffer (find-file-noselect emacstv-index-org)
						(unless (emacstv-find-by-youtube-url link)
							(emacstv-add-from-youtube link)
							(display-buffer (current-buffer)))))))))

(defun emacstv-insert-org-list-from-spookfox ()
	(interactive)
	(insert (spookfox-js-injection-eval-in-active-tab "[...document.querySelectorAll('a.ytd-playlist-video-renderer#video-title')].map((o) => `- [[${o.href}][${o.getAttribute('title')}]]\n`).join('') || [...document.querySelectorAll('#primary a.ytd-playlist-panel-video-renderer#wc-endpoint, #items a.ytd-playlist-panel-video-renderer#wc-endpoint')].map((o) => `- [[${o.href}][${o.querySelector('#video-title').getAttribute('title')}]]\n`).join('') || [...document.querySelectorAll('a.ytd-rich-grid-media#video-title-link')].map((o) => `- [[${o.href}][${o.getAttribute('title')}]]\n`).join('')" t)))


(defun emacstv-sort-by-newest-first ()
	(interactive)
	(with-current-buffer (find-file-noselect emacstv-index-org)
		(goto-char (point-min))
		(org-sort-entries nil ?R nil nil "DATE")))

(defun emacstv-timestamp-to-seconds (time-string)
  "Find HH:MM:SS.MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern."
  (save-match-data
    (when (and time-string
							 (string-match "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?"
														 time-string))
      (let ((hours (string-to-number (or (match-string 2 time-string) "0")))
            (mins  (string-to-number (or (match-string 3 time-string) "0")))
            (secs  (string-to-number (or (match-string 4 time-string) "0")))
            (msecs (string-to-number (string-pad
                                      (or (match-string 5 time-string) "0")
                                      3 ?0))))
        (/ (+ (* (truncate hours) 3600000)
							(* (truncate mins) 60000)
							(* (truncate secs) 1000)
							(truncate msecs))
					 1000.0)))))

(defun emacstv-count-entries ()
	(interactive)
	(with-current-buffer (find-file-noselect emacstv-index-org)
		(let ((count 0)
					(seconds 0))
			(org-map-entries
			 (lambda ()
				 (setq seconds (+ seconds (or (emacstv-timestamp-to-seconds
																			 (org-entry-get (point) "DURATION"))
																			0)))
				 (cl-incf count))
			 "LEVEL=1")
			(message "%d videos %s"
							 count
							 (emacstv-format-seconds (floor seconds))))))

(defvar emacstv-load-videos-from 'org "org or json")

(defun emacstv-videos ()
	"Return a list of videos."
	;; Hmm... Parsing the JSON is probably faster,
	;; but parsing the Org file gets more up-to-date
	;; info in case things have been changed.
	(pcase emacstv-load-videos-from
		('org
		 (with-current-buffer (find-file-noselect emacstv-index-org)
			 (org-map-entries
				(lambda ()
					(let* ((end (save-excursion (org-end-of-subtree)))
								 (beg (progn (org-end-of-meta-data t) (point)))
								 (desc (if (< beg end) (string-trim (buffer-substring beg end)) "")))
						(append (org-entry-properties)
										`(("DESCRIPTION" . ,desc)))))
				"LEVEL=1")))
		('json
		 (let ((json-object-type 'alist)
					 (json-list-type 'list))
			 (json-read-file (concat (file-name-sans-extension emacstv-index-org) ".json"))))
		(_ (error "Set `emacstv-load-videos-from' to org or json"))))

(defun emacstv-format-for-completion (o)
	"Format O for completion."
	(concat
	 (or (assoc-default "ITEM" o 'string=)
			 (assoc-default 'ITEM o))
	 (if (not (string=
						 (or
							(assoc-default "SPEAKERS" o 'string=)
							(assoc-default 'SPEAKERS o)) ""))
			 (format " - %s"
							 (or
								(assoc-default "SPEAKERS" o 'string=)
								(assoc-default 'SPEAKERS o)))
		 "")
	 (if (or (assoc-default "DURATION" o 'string=)
					 (assoc-default 'DURATION o))
			 (format " (%s)"
							 (or (assoc-default "DURATION" o 'string=)
									 (assoc-default 'DURATION o)))
		 "")))

;; (memoize-restore #'emacstv-videos)
(when (and (functionp 'memoize)
					 (not (get #'emacstv-videos :memoize-original-function)))
	(memoize #'emacstv-videos "60"))

(defun emacstv-video-completion-collection (string predicate action)
	"Return a collection for completion."
	;; TODO: There's probably a more efficient way to do this
	(if (eq action 'metadata)
			'(metadata
				(cycle-sort-function . identity)
				(display-sort-function . identity))
		(complete-with-action
		 action
		 (mapcar (lambda (o) (cons (emacstv-format-for-completion o) o))
						 (emacstv-videos))
		 string predicate)))

(defun emacstv-complete-video (&optional prompt)
	(let ((collection
				 (mapcar (lambda (o) (cons (emacstv-format-for-completion o) o))
								 (emacstv-videos))))
		(assoc-default
		 (completing-read (or prompt "Video: ")
											#'emacstv-video-completion-collection nil t)
		 collection #'string=)))

(defun emacstv-video-url (video)
	"Return the URL for playing VIDEO."
	(or (assoc-default "MEDIA_URL" video #'string=)
			(assoc-default "TOOBNIX_URL" video #'string=)
			(assoc-default "VIMEO_URL" video #'string=)
			(assoc-default "YOUTUBE_URL" video #'string=)
			(assoc-default 'MEDIA_URL video #'string=)
			(assoc-default 'TOOBNIX_URL video #'string=)
			(assoc-default 'VIMEO_URL video #'string=)
			(assoc-default 'YOUTUBE_URL video #'string=)))

(defun emacstv-format-seconds (seconds)
	"Format SECONDS as hh:mm:ss. Omit hh or mm if not needed."
	;; work around because %z can't be in format-seconds multiple times
	(let ((seconds-in-day (* 60 60 24)))
		(replace-regexp-in-string
		 "^0" ""
		 (concat
			(if (< seconds seconds-in-day)
					""
				(prog1 (format "%d:" (floor (/ seconds seconds-in-day)))
					(setq seconds (mod seconds seconds-in-day))))
			(concat (format-seconds "%.2h:%z%.2m:%.2s" (floor seconds)))))))

;;;###autoload
(defun emacstv-play (video)
	(interactive (list (emacstv-complete-video)))
	(require 'mpv)
	(let ((url (if (stringp video) video (emacstv-video-url video))))
		(setq emacstv-playlist (list url))
		(if (mpv-live-p)
				(progn
					(mpv-run-command "loadfile" url))
			(mpv-play-url url))))

(defun emacstv-random-video ()
	(let ((videos (emacstv-videos)))
		(elt videos (random (length videos)))))

;;;###autoload
(defun emacstv-play-random ()
	"Play a random Emacs video."
	(interactive)
	(require 'mpv)
	(emacstv-play (emacstv-random-video)))

(defun emacstv-shuffle-list (list)
  "Shuffle LIST using Fisher-Yates algorithm."
  (let ((shuffled (copy-sequence list)))
    (dotimes (i (1- (length shuffled)))
      (let* ((j (+ i (random (- (length shuffled) i))))
             (temp (nth i shuffled)))
        (setf (nth i shuffled) (nth j shuffled))
        (setf (nth j shuffled) temp)))
    shuffled))

(defvar emacstv-playlist nil)

(defun emacstv-clear-playlist ()
	(interactive)
	(mpv-run-command "playlist-clear")
	(setq emacstv-playlist nil))

(defun emacstv-queue-random ()
	"Queue up lots of Emacs videos."
	(interactive)
	(require 'mpv)
	(emacstv-clear-playlist)
	(setq emacstv-playlist (emacstv-shuffle-list (mapcar #'emacstv-video-url (emacstv-videos))))
	(if (mpv-live-p)
			(dolist (url emacstv-playlist)
				(mpv-playlist-append-url url))
		(mpv-play-url (car emacstv-playlist))
		(dolist (url (cdr emacstv-playlist))
			(mpv-playlist-append-url url))))


;;;###autoload
(define-minor-mode emacstv-background-mode
	"Play random Emacs videos in the background."
	:global t
	(if emacstv-background-mode
			(progn
				(require 'mpv)
				(emacstv-queue-random))
		;; no worries if it has already quit
		(condition-case nil
				(mpv-quit nil)
			(error nil))))

;;;###autoload
(defun emacstv-play-at-point ()
	"Play a video from the agenda or org file."
	(interactive)
	(when (derived-mode-p 'org-agenda-mode)
		(org-agenda-switch-to))
	(when (derived-mode-p 'org-mode)
		(let ((url (or (org-entry-get (point) "MEDIA_URL")
									 (org-entry-get (point) "TOOBNIX_URL")
									 (org-entry-get (point) "YOUTUBE_URL")
									 (org-entry-get (point) "VIMEO_URL"))))
			(if url
					(emacstv-play url)
				(error "Could not find URL for %s" (org-entry-get (point) "ITEM"))))))

(defun emacstv-add-to-playlist-at-point ()
	"Queue a video from the agenda or org file."
	(interactive)
	(when (derived-mode-p 'org-agenda-mode)
		(org-agenda-switch-to))
	(when (derived-mode-p 'org-mode)
		(let ((url (or (org-entry-get (point) "MEDIA_URL")
									 (org-entry-get (point) "TOOBNIX_URL")
									 (org-entry-get (point) "YOUTUBE_URL")
									 (org-entry-get (point) "VIMEO_URL"))))
			(if url
					(if (mpv-live-p)
							(progn
								(setq emacstv-playlist (append emacstv-playlist (list url)))
								(mpv-playlist-append-url url))
						(emacstv-play-at-point))
				(error "Could not find URL")))))

;;;###autoload
(defun emacstv-agenda-search ()
	(interactive)
	(require 'org-agenda)
	(let ((org-agenda-files (list emacstv-index-org)))
		(org-search-view)
		(when (featurep 'hl-line)
			(hl-line-mode 1))))

;; ex. search: (and (heading "python") (not (tags "python")))
;;;###autoload
(defun emacstv-org-ql-search (query)
	(interactive (list (progn
											 (read-string "Query: "
																		(when (and (boundp 'org-ql-view-query) org-ql-view-query)
																			(format "%S" org-ql-view-query))))))
	(require 'org-ql)
	(org-ql-search (list emacstv-index-org) query)
	(when (featurep 'hl-line)
		(hl-line-mode 1)))

;;;###autoload
(defun emacstv-find-org ()
	(interactive)
	(find-file emacstv-index-org))

;;;###autoload
(defun emacstv-jump-to-current-video ()
	"Jump to the entry for the current video."
	(interactive)
	(emacstv-find-org)
	(require 'mpv)
	(unless (mpv-live-p)
		(error "No active MPV process."))
	(let ((url (mpv-get-property "stream-open-filename"))
				(playlist-index (and emacstv-playlist
														 (mpv-get-property "playlist-playing-pos"))))
		(or (emacstv-find-by-url url)
				(emacstv-find-by-url (elt emacstv-playlist playlist-index))
				(and url (error "Could not find %s" url))
				(error "Not sure what the current URL is"))))

(defun emacstv-org-ql-search-matching-untagged (query)
	"Search for headings matching QUERY that don't have that as a tag."
	(interactive (list (progn
											 (read-string "Query: "
																		(when (and (boundp 'org-ql-view-query) org-ql-view-query)
																			(format "%S" org-ql-view-query))))))
	(require 'org-ql)
	(org-ql-search (list emacstv-index-org)
		`(and (heading ,query) (not (tags ,query))))
	(when (featurep 'hl-line)
		(hl-line-mode 1)))

(defun emacstv-org-ql-search-untagged ()
	"Search for untagged items."
	(interactive)
	(require 'org-ql)
	(org-ql-search (list emacstv-index-org)
		`(and (not (tags-regexp "."))))
	(when (featurep 'hl-line)
		(hl-line-mode 1)))


(provide 'emacstv)
;;; emacstv.el ends here
