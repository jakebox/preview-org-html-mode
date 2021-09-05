;;; preview-org-html-mode.el --- Automatically preview org-exported HTML files within Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Jake B <jakebox.github.io>

;; Author: Jake B <jakebox.github.io>
;; Url: https://github.com/jakebox/preview-org-html-mode
;; Keywords: Org, convenience, outlines
;; Version: 0.3
;; Package-Requires: ((emacs "27.1") (org "8.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This minor mode provides a side-by-side preview of your org-exported HTML
;; files using the either the eww or xwidget browsers. The update frequency of
;; the preview can be configured to suit your preference.
;;

;; Quick start:
;; Put this file under your load path.
;; Enable the minor mode in an Org buffer:
;;   M-x preview-org-html-mode
;; Configure options with M-x customize-group preview-org-html

;;; Code:

;;;; Requirements
(require 'org)
(require 'xwidget)
(require 'eww)


(defgroup preview-org-html nil
  "Automatically preview org-exported HTML files within Emacs."
  :group 'org-mode
  :link '(url-link :tag "Homepage" "https://github.com/jakebox/preview-org-html-mode/"))

(defcustom preview-org-html-refresh-configuration 'save
  "Specifies how often the HTML preview will be refreshed.
  
If 'manual, update manually by running `preview-org-html-refresh'.
If 'save, update on save (default).
If 'export, update on manual export \(using `org-html-export-to-html').
If 'timer, update preview on timer ('preview-org-html-timer-interval').
If 'instant, update ASAP (may cause slowdowns)."
  :type '(choice
		  (symbol :tag "Update preview manually."   'manual)
		  (symbol :tag "Update preview on save."    'save)
		  (symbol :tag "Update preview on export."  'export)
		  (symbol :tag "Update preview on a timer." 'timer)
		  (symbol :tag "Update preview instantly."  'instant))
  :options '(save export timer instant)
  :group 'preview-org-html)

(defcustom preview-org-html-timer-interval 2
  "Integer seconds to wait between exports when in 'timer mode."
  :type 'integer
  :group 'preview-org-html)

(defcustom preview-org-html-viewer 'eww
  "Which Emacs browser 'preview-org-html-mode' will use.
If 'eww, use eww browser (default).
If 'xwidget, use xwidget browser."
  :type 'symbol
  :group 'preview-org-html)

(defcustom preview-org-html-subtree-only nil
  "If non-nil, scope the preview to the current subtree."
  :type 'boolean
  :group 'preview-org-html)

(defvar preview-org-html--browser-buffer nil)
(defvar preview-org-html--previewed-buffer-name nil)
(defvar preview-org-html--refresh-timer nil)
(defvar-local preview-org-html--html-file nil)


;; https://emacs.stackexchange.com/questions/7116/pop-a-window-into-a-frame
(defun preview-org-html-pop-window-to-frame ()
  "Pop a window to a frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(defun preview-org-html-refresh ()
  "Exports the org file to HTML and refreshes the preview."
  ;; Refresh the preview.
  (interactive)
  ;; WIP If in manual mode it doesn't matter what buffer is active, just export and refresh
  (cond
   ((eq preview-org-html-refresh-configuration 'manual) ;; if in manual mode
		 (pop-to-buffer preview-org-html--previewed-buffer-name nil t)
		 (preview-org-html--org-export-html)
		 (preview-org-html--reload-preview))
		((unless (or (eq (eq (get-buffer preview-org-html--previewed-buffer-name) ;; TODO JAKE WHAT IS THIS
                             ;; In timer and instant modes the visible buffer matters
							 (window-buffer (selected-window))) nil)
					 (or (let ((state preview-org-html-refresh-configuration))
						   (eq state 'timer) (eq state 'instant))))
		   (preview-org-html--org-export-html)
		   (preview-org-html--reload-preview)))))

(defun preview-org-html--org-export-html ()
  "Silently export org to HTML."
  (let ((standard-output 'ignore))
	(org-export-to-file 'html (substring preview-org-html--html-file 7)
	  nil preview-org-html-subtree-only nil nil nil nil)))

(defun preview-org-html--reload-preview ()
  "Reload preview."
  (save-selected-window
	(pop-to-buffer preview-org-html--browser-buffer)
	(cond ((eq preview-org-html-viewer 'xwidget) (xwidget-webkit-reload))
		  ((eq preview-org-html-viewer 'eww)
		   (with-selected-window (selected-window)
			 ;; This stuff is to keep eww window scrolled at same point
			 (let ((eww-point (point))
				   (eww-window-start (window-start)))
			   (eww-reload)
			   (goto-char eww-point)
			   (set-window-start nil eww-window-start)))))))

(defun preview-org-html--kill-preview-buffer ()
  "Kill the xwidget preview buffer and pop back to the previewed org buffer."
  ;; Only do these things if the preview is around
  (when (bound-and-true-p preview-org-html--browser-buffer)
    ;; If preview is visible we first delete the window, otherwise
	;; just kill the preview buffer
	(if (get-buffer-window preview-org-html--browser-buffer 'visible)
		(delete-window (get-buffer-window preview-org-html--browser-buffer)))
	(let ((kill-buffer-query-functions nil))
	  (kill-buffer preview-org-html--browser-buffer))
	(pop-to-buffer preview-org-html--previewed-buffer-name)))

(defun preview-org-html--run-with-timer ()
  "Configure timer to refresh preview for `timer' mode."
  (setq preview-org-html--refresh-timer
		(run-at-time 1 preview-org-html-timer-interval #'preview-org-html-refresh)))

(defun preview-org-html--config ()
  "Configure buffer for preview: add exit hooks; configure refresh hooks."
  (setq preview-org-html--previewed-buffer-name (buffer-name))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Configure exit hooks
    (add-hook hook #'preview-org-html--stop-preview nil t))
  (let ((conf preview-org-html-refresh-configuration))
	(cond
	 ((eq conf 'manual))
	 ((eq conf 'save) ;; On save
	  (add-hook 'after-save-hook #'preview-org-html-refresh nil t))
	 ((eq conf 'timer) ;; every X seconds
	  (preview-org-html--run-with-timer))
	 ((eq conf 'export) ;; On export using org-html-export-html command manually
	  (advice-add 'org-html-export-to-html :after #'preview-org-html--reload-preview))
	 ((eq conf 'instant) ;; WIP Instantly (on self insert refresh)
	  (add-hook 'post-self-insert-hook #'preview-org-html-refresh nil t)))))

(defun preview-org-html--unconfig ()
  "Unconfigure 'preview-org-html-mode' (remove hooks and advice)."
  (let ((conf preview-org-html-refresh-configuration))
	(cond ((eq conf 'instant) ;; WIP
		   (remove-hook 'post-self-insert-hook #'preview-org-html-refresh t))
		  ((eq conf 'save)
		   (remove-hook 'after-save-hook #'preview-org-html-refresh t))
		  ((eq conf 'timer)
		   (cancel-timer preview-org-html--refresh-timer))
		  ((eq conf 'export)
		   (advice-remove 'org-html-export-to-html #'preview-org-html--reload-preview))))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Remove hooks
    (remove-hook hook #'preview-org-html--stop-preview t))
  ;; Reset variables
  (dolist (var '(preview-org-html--browser-buffer preview-org-html--previewed-buffer-name))
	(set var nil)))

(defun preview-org-html--open-browser ()
  "Open a browser to preview the exported HTML file."
  ;; Store the exported HTML filename
  (setq-local preview-org-html--html-file (concat (file-name-sans-extension
												   (concat "file://" buffer-file-name)) ".html"))
  (unless (file-exists-p preview-org-html--html-file)
	(preview-org-html--org-export-html)) ;; Unless the file already exists, export it
  ;; Procedure to open the side-by-side preview
  (split-window-right)
  (other-window 1)
  (let ((file preview-org-html--html-file))
	(cond ((eq preview-org-html-viewer 'xwidget) (xwidget-webkit-browse-url file))
		  ((eq preview-org-html-viewer 'eww) (eww-browse-url file))))
  (setq preview-org-html--browser-buffer (get-buffer (buffer-name)))
  (previous-window-any-frame))

(defun preview-org-html--start-preview ()
  "Begin the preview-org-html preview."
  (when buffer-file-name
	(cond ((derived-mode-p 'org-mode)
		   (message "preview-org-html-mode has recieved a major update - xwidgets support, refresh configurations and more! \n M-x customize-group preview-org-html-mode")
		   (preview-org-html--open-browser)
		   (preview-org-html--config))
		  (t
		   (preview-org-html-mode -1)
		   (user-error "`%s' not supported by preview-org-html preview, only `org mode'!" major-mode)))))

(defun preview-org-html--stop-preview ()
  "Stop the preview-org-html preview."
  (preview-org-html--kill-preview-buffer)
  (preview-org-html--unconfig))


;;;###autoload
(define-minor-mode preview-org-html-mode
  "(Optionally) live preview for Org exports to HTML."
  :lighter " preview-org-html"
  (if preview-org-html-mode
      (preview-org-html--start-preview)
    (preview-org-html--stop-preview)))

(provide 'preview-org-html-mode)

;;; preview-org-html-mode.el ends here
