;;; torrents-csv.el --- Emacs frontend to torrents-csv  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Divya Ranjan Pattanaik <divya@subvertising.org>

;; Author: Divya Ranjan Pattanaik <divya@subvertising.org>
;; Keywords: torrents, csv
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://codeberg.org/divyaranjan/torrents-csv.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;; In addition to conditions of the GNU General Public License, this program may
;; not be redistributed without the following acknowledgement:

;;; Commentary:

;; This is a frontend to torrents-csv project, which helps in being able to search
;; and get magnets of torrents within Emacs.

(require 'url)
(require 'button)
(require 'url-http)
(require 'tabulated-list)
(require 'json)

(defgroup torrents-csv nil
  "Emacs frontend for torrent-csv to fetch torrents."
  :group 'external
  :prefix "torrents-csv-")

(defcustom torrents-csv-domain  "https://torrents-csv.com/"
  "The domain of torrents-csv to use. If a local server is being used
then the appropriate localhost address and port is to be defined."
  :type 'string
  :group 'torrents-csv)

(defcustom torrents-csv-results-count "100"
  "The number of results that are displayed in a query."
  :type 'string
  :group 'torrents-csv)

(defvar torrents-csv-base-api-url (concat torrents-csv-domain "service/search?"))

(defun info->magnet (name info)
  "Convert a NAME and INFO into proper torrent magnet by concatenating them."
  (concat "magnet:?xt=urn:btih:" info "&dn=" name))

(defun copy-magnet (magnet)
  "Copy the magnet of the current list item"
  (kill-new magnet)
  (message "🧲 has been copied to clipboard!"))

(defun copy-magnet-from-row ()
  "Copy the magnet link from the current row."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
	 (magnet (aref entry 5)))
      (kill-new magnet)
      (message "🧲 has been copied to clipboard!")))

(defun round-to-decimals-safe (number decimals)
  "Round NUMBER to DECIMALS decimal places with input validation."
  (if (< decimals 0)
      (error "DECIMALS must be non-negative")
    (let ((factor (float (expt 10 decimals))))
      (/ (round (* number factor)) factor))))

(defun bytes-to-mb-or-gb (bytes)
  "Convert BYTES into Megabytes or Gigabytes."
  (if (> bytes 1000000000)
      (concat (int-to-string
	       (round-to-decimals-safe
		(/ (float bytes) (float 1000000000)) 2)) "G")
    (concat (int-to-string
	     (round-to-decimals-safe
	      (/ (float bytes) (float 1000000)) 2)) "M")))


;;; Torrents List Mode and Keymaps

(defvar-keymap torrents-list-mode-map
  "m" #'copy-magnet-from-row)

(define-derived-mode torrents-list-mode tabulated-list-mode "Torrents List"
  "Major mode for viewing and operating on torrents in a tabulated list."

  ;; Column names
  (setq tabulated-list-format
	[("Magnet" 8 t)
	 ("Name" 155 t)
         ("Size" 10 t)
         ("Seeders" 8 t)
         ("Leechers" 8 t)])

  ;; Set padding and default sorting
  (setq tabulated-list-padding 10)
  (setq tabulated-list-sort-key (cons "Seeders" nil)) ; Default sort by "Seeders"

  ;; Refresh and display the table
  (tabulated-list-print)

  ;; Initialize the header row
  (tabulated-list-init-header)
  (delete-other-windows))

(defun torrents-display-tabulated (data)
  "Display fetched torrent data as a table using `torrents-list-mode'."
  (let ((buffer (get-buffer-create "*Torrents Results*")))
    (with-current-buffer buffer
      ;; Prepare tabulated list entries
      (setq tabulated-list-entries
            (mapcar (lambda (torrent)
                      (let* ((info (alist-get 'infohash torrent))
                             (name (alist-get 'name torrent))
                             (size (bytes-to-mb-or-gb (alist-get 'size_bytes torrent)))
                             (seeders (alist-get 'seeders torrent))
                             (leechers (alist-get 'leechers torrent))
                             (magnet (info->magnet name info)))
                        (list nil
                              (vector
			       (make-text-button "🧲"
						 nil
						 'action (lambda (_) (copy-magnet magnet))
						 'help-echo "Click to copy magnet")
                               name
                               size
                               (number-to-string seeders)
                               (number-to-string leechers)
			       magnet))))
                    (alist-get 'torrents data))))
    (pop-to-buffer buffer))
  (torrents-list-mode))

(defun torrents-json-parse-buffer ()
  "Parse the buffer with JSON data"
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'alist))
    (json-read)))

(defun search-torrents ()
  "Command for searching torrents and then displaying them in a tabulated list."
  (interactive)
  (let ((input (read-string "What do you want to pirate? ")))
    (if (string-empty-p input)
	(error "Query cannot be empty!")
      (setq torrent-query input))
    (let* ((api-url (concat torrents-csv-base-api-url
			    "q=" input "&size="
			    torrents-csv-results-count))
	   (url-user-agent (concat "torrents-csv - GNU Emacs"
				   emacs-version
				   " <https://www.gnu.org/software/emacs>")))
      (url-retrieve api-url
		    (lambda (status)
		      (if (plist-get status :error)
			  (message "Failed to fetch torrents")
			(let ((data (torrents-json-parse-buffer)))
			  (torrents-display-tabulated data))))))))
