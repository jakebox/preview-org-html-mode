;;; preview-org-html-mode.el --- Semi-live Xwidgets preview of org-exported HTML

;; Author: Jake B
;; Url: https://github.com/jakebox/preview-org-html-mode
;; Version: 0.1
;; Keywords: org, html, preview, xwideget

;;; Commentary:

;;; License:

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

;;; Code:

(require 'org)
(require 'xwidget)


(defgroup preview-org-html nil
  "Easy Orgmode HTML preview with Xwidgets browser."
  :group 'org-mode
  :link '(url-link :tag "Homepage" "https://github.com/jakebox/"))

(defcustom preview-org-html-auto-refresh-on-save t
  "If non-nil, automatically export org file to html and refresh preview.
If nil, only refresh when `org-html-export-to-html' is called manually."
  :type 'boolean
  :group 'preview-org-html)

(defvar preview-org-html--xwidget-buffer-name nil)
(defvar preview-org-html--previewed-buffer-name nil)


;; https://emacs.stackexchange.com/questions/7116/pop-a-window-into-a-frame
(defun preview-org-html-pop-window-to-frame ()
  "Pop a window to a frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(defun preview-org-html-refresh ()
  "Exports the org file to HTML and refreshes the Xwidgets preview."
  ;; WIP, might make this user-accessible so it can be bound to a key. Not sure if that would be useful or not.
  (org-html-export-to-html)
  (preview-org-html--reload-preview))

(defun preview-org-html--reload-preview ()
  "Reload xwidget preview."
  (xwidget-webkit-reload))

(defun preview-org-html--kill-xwidget-buffer ()
  "Kill the xwidget preview buffer."
  (when (bound-and-true-p preview-org-html--xwidget-buffer-name) ;; Only do these things if the preview is around
	(progn 
	  (let ((kill-buffer-query-functions nil))
		(kill-buffer preview-org-html--xwidget-buffer-name))
	  (delete-window)
	  (pop-to-buffer preview-org-html--previewed-buffer-name))))

(defun preview-org-html--config ()
  "Configure the buffer for preview-org-html-mode. Add auto-stop hooks.
Also configure the refresh system (refresh only on export or automatically export and refresh on save)."
  (setq preview-org-html--previewed-buffer-name (buffer-name))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook))
    (add-hook hook #'preview-org-html--stop-preview nil t))
  (if (eq preview-org-html-auto-refresh-on-save t)
      (add-hook 'after-save-hook #'preview-org-html-refresh nil t) ;; More automated. On save file, export the org file and refresh the preview.
    (advice-add 'org-html-export-to-html :after #'preview-org-html--reload-preview))) ;; Less automated. On export, refresh the preview.

(defun preview-org-html--unconfig ()
  "Unconfigure preview-org-html-mode (remove hooks and advice)."
  (if (eq preview-org-html-auto-refresh-on-save t) ;; Condtionally remove some hooks/advice
	  (remove-hook 'after-save-hook #'preview-org-html-refresh t)
    (advice-remove 'org-html-export-to-html #'preview-org-html--reload-preview)) ;; Just reload on export, not on save.
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Remove hooks
    (remove-hook hook #'preview-org-html--stop-preview t))
  (dolist (var '(preview-org-html--xwidget-buffer-name preview-org-html--previewed-buffer-name)) ;; Reset variables
	(set var nil)))

(defun preview-org-html--open-xwidget ()
  "Open xwidget browser on current file."
  (let ((html-file-name (concat (file-name-sans-extension (concat "file://" buffer-file-name)) ".html")))
	(split-window-right)
	(other-window 1)
	(xwidget-webkit-browse-url html-file-name)
	(setq preview-org-html--xwidget-buffer-name (get-buffer (buffer-name))))
  (previous-window-any-frame)
  (preview-org-html--reload-preview))

(defun preview-org-html--start-preview ()
  "Begin the preview-org-html preview."
  (when buffer-file-name
	(cond ((derived-mode-p 'org-mode)
		   (progn
			 (preview-org-html--open-xwidget)
			 (preview-org-html--config)))
		  (t
		   (preview-org-html-mode -1)
		   (user-error "`%s' not supported by preview-org-html preview, only org mode!" major-mode)))))

(defun preview-org-html--stop-preview ()
  "Stop the preview-org-html preview."
  (preview-org-html--kill-xwidget-buffer)
  (preview-org-html--unconfig))


;;;###autoload
(define-minor-mode preview-org-html-mode
  "(Optionally) live preview for Org exports to HTML."
  :lighter "preview-org-html"
  (if preview-org-html-mode
      (preview-org-html--start-preview)
    (preview-org-html--stop-preview)))

(provide 'preview-org-html-mode)

;;; preview-org-html-mode.el ends here
