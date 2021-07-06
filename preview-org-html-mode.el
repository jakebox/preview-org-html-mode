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

(defcustom preview-org-html-refresh-configuration 'save
  "If 'timer, update preview on timer.
If 'save, update on save.
If 'instant, update ASAP (may cause slowdowns).
If 'export, update on manual export \(using `org-html-export-to-html')."
  :type '(choice
		  (symbol :tag "Update preview on a timer." 'timer)
		  (symbol :tag "Update preview on manual save." 'save)
		  (symbol :tag "Update preview instantly." 'instant)
		  (symbol :tag "Update preview one manual export." 'export))
  :options '(timer save instant export)
  :group 'preview-org-html)

(defcustom preview-org-html--timer-interval 4
  "Seconds to wait between exports when preview-org-html-refresh-configuration is set to 'timer."
  :type 'integer
  :group 'preview-org-html)

(defvar preview-org-html--xwidget-buffer nil)
(defvar preview-org-html--previewed-buffer-name nil)
(defvar preview-org-html--refresh-timer nil)

(defun preview-org-html-check-test ()
  (interactive)
  (cond ((eq (get-buffer preview-org-html--previewed-buffer-name) (window-buffer (selected-window)))
		 (message "Visible and focused"))
		((get-buffer-window preview-org-html--previewed-buffer-name)
		 (message "Visible and unfocused")) 
		(t
		 (message "Not visible")))
  )

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
  ;; Unless the previewed file isn't focused AND the timer/instant mode is on, refresh
  (unless (or (eq (eq (get-buffer preview-org-html--previewed-buffer-name) (window-buffer (selected-window))) nil)
			  (or (let ((state preview-org-html-refresh-configuration)) (eq state 'timer) (eq state 'instant))))
	(progn 
	  (org-html-export-to-html)
	  (preview-org-html--reload-preview))))

(defun preview-org-html--reload-preview ()
  "Reload xwidget preview."
  (xwidget-webkit-reload))

(defun preview-org-html--kill-xwidget-buffer ()
  "Kill the xwidget preview buffer."
  (when (bound-and-true-p preview-org-html--xwidget-buffer) ;; Only do these things if the preview is around
	(progn
	  (let ((kill-buffer-query-functions nil))
		(kill-buffer preview-org-html--xwidget-buffer))
	  (delete-window)
	  (pop-to-buffer preview-org-html--previewed-buffer-name))))

(defun preview-org-html--run-with-timer ()
  (setq preview-org-html--refresh-timer (run-at-time 1 preview-org-html--timer-interval 'preview-org-html-refresh)))


(defun preview-org-html--config ()
  "Configure the buffer for 'preview-org-html-mode'. Add auto-stop hooks.
Also configure the refresh system (refresh only on export or automatically export and refresh on save)."
  (setq preview-org-html--previewed-buffer-name (buffer-name))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Configure exit hooks
    (add-hook hook #'preview-org-html--stop-preview nil t))
  (cond ((eq preview-org-html-refresh-configuration 'instant) ;; TODO instantly
		 (add-hook 'post-self-insert-hook #'preview-org-html-refresh nil t)) ;; On self insert refresh
		((eq preview-org-html-refresh-configuration 'save)
		 (add-hook 'after-save-hook #'preview-org-html-refresh nil t)) ;; More automated. On save file, export the org file and refresh the preview.
		((eq preview-org-html-refresh-configuration 'timer) ;; every X seconds
		 (preview-org-html--run-with-timer))
		((eq preview-org-html-refresh-configuration 'export)
		 (advice-add 'org-html-export-to-html :after #'preview-org-html--reload-preview)))) ;; On export, refresh the preview.

(defun preview-org-html--unconfig ()
  "Unconfigure 'preview-org-html-mode' (remove hooks and advice)."
  (cond ((eq preview-org-html-refresh-configuration 'instant) ;; TODO
		 (remove-hook 'post-self-insert-hook #'preview-org-html-refresh t))
		((eq preview-org-html-refresh-configuration 'save)
		 (remove-hook 'after-save-hook #'preview-org-html-refresh t))
		((eq preview-org-html-refresh-configuration 'timer)
		 (cancel-timer preview-org-html--refresh-timer))
		((eq preview-org-html-refresh-configuration 'export)
		 (advice-remove 'org-html-export-to-html #'preview-org-html--reload-preview)) )
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Remove hooks
    (remove-hook hook #'preview-org-html--stop-preview t))
  (dolist (var '(preview-org-html--xwidget-buffer preview-org-html--previewed-buffer-name)) ;; Reset variables
	(set var nil)))

(defun preview-org-html--open-xwidget ()
  "Open xwidget browser on current file."
  (let ((html-file-name (concat (file-name-sans-extension (concat "file://" buffer-file-name)) ".html")))
	(split-window-right)
	(other-window 1)
	(xwidget-webkit-browse-url html-file-name)
	(setq preview-org-html--xwidget-buffer (get-buffer (buffer-name))))
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
