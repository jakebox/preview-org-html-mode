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
"If 'save, update on save.
If 'export, update on manual export \(using `org-html-export-to-html').
If 'timer, update preview on timer.
If 'instant, update ASAP (may cause slowdowns)."
  :type '(choice
		  (symbol :tag "Update preview manually by running `preview-org-html-refresh'."    'manual)
		  (symbol :tag "Update preview on manual save."    'save)
		  (symbol :tag "Update preview one manual export." 'export)
		  (symbol :tag "Update preview on a timer."        'timer)
		  (symbol :tag "Update preview instantly."         'instant))
  :options '(save export timer instant)
  :group 'preview-org-html)

(defcustom preview-org-html--timer-interval 4
  "Seconds to wait between exports when in 'timer mode."
  :type 'integer
  :group 'preview-org-html)

(defvar preview-org-html--xwidget-buffer nil)
(defvar preview-org-html--previewed-buffer-name nil)
(defvar preview-org-html--refresh-timer nil)


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
  ;; Refresh the preview.
  (interactive)
  (cond ((eq preview-org-html-refresh-configuration 'manual) ;; WIP If in manual mode it doesn't matter what buffer is active
		 (progn (pop-to-buffer preview-org-html--previewed-buffer-name nil t) (org-html-export-to-html)
				(preview-org-html--reload-preview)))
		((unless (or (eq (eq (get-buffer preview-org-html--previewed-buffer-name) (window-buffer (selected-window))) nil) ;; In timer and instant modes the visible buffer matters
					 (or (let ((state preview-org-html-refresh-configuration)) (eq state 'timer) (eq state 'instant))))
		   (progn
			 (org-html-export-to-html)
			 (preview-org-html--reload-preview))))))

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
  "Configure timer to refresh preview for 'timer mode."
  (setq preview-org-html--refresh-timer
		(run-at-time 1 preview-org-html--timer-interval 'preview-org-html-refresh)))

(defun preview-org-html--config ()
  "Configure the buffer for 'preview-org-html-mode'. Add auto-stop hooks.
Also configure the refresh system (refresh only on export or automatically export and refresh on save)."
  (setq preview-org-html--previewed-buffer-name (buffer-name))
  (dolist (hook '(kill-buffer-hook kill-emacs-hook)) ;; Configure exit hooks
    (add-hook hook #'preview-org-html--stop-preview nil t))
  (let ((conf preview-org-html-refresh-configuration))
	(cond
	 ((eq conf 'instant) (add-hook 'post-self-insert-hook #'preview-org-html-refresh nil t)) ;; TODO Instantly (on self insert refresh)
	 ((eq conf 'save)    (add-hook 'after-save-hook #'preview-org-html-refresh nil t)) ;; On save
	 ((eq conf 'timer)   (preview-org-html--run-with-timer)) ;; every X seconds
	 ((eq conf 'export)  (advice-add 'org-html-export-to-html :after #'preview-org-html--reload-preview))))) ;; On export

(defun preview-org-html--unconfig ()
  "Unconfigure 'preview-org-html-mode' (remove hooks and advice)."
  (let ((conf preview-org-html-refresh-configuration))
	(cond ((eq conf 'instant) (remove-hook 'post-self-insert-hook #'preview-org-html-refresh t)) ;; TODO
		  ((eq conf 'save)    (remove-hook 'after-save-hook #'preview-org-html-refresh t))
		  ((eq conf 'timer)   (cancel-timer preview-org-html--refresh-timer))
		  ((eq conf 'export)  (advice-remove 'org-html-export-to-html #'preview-org-html--reload-preview))))
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
