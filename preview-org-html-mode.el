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

;; Helper funcs
;; https://stackoverflow.com/questions/10929915/how-do-i-answer-y-automatically-kill-matching-buffers-asks-if-i-should-kill-a-m
(defun preview-org-html--kill-matching-buffers-no-ask (regexp)
  (cl-letf (((symbol-function 'kill-buffer-ask) #'kill-buffer))
    (kill-matching-buffers regexp)))

;; https://emacs.stackexchange.com/questions/7116/pop-a-window-into-a-frame
(defun preview-org-html-pop-window-to-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))
;; -------------


(defgroup preview-org-html nil
  "Easy Orgmode HTML preview with Xwidgets browser."
  :group 'org-mode
  :link '(url-link :tag "Homepage" "https://github.com/jakebox/"))

(defcustom preview-org-html-auto-refresh-on-save t
  "If non-nil, automatically export org file to html and refresh preview. If nil, only refresh when `org-html-export-to-html' is called manually."
  :type 'boolean
  :group 'preview-org-html)


;; -----------------------


(defun preview-org-html-refresh ()
  "Exports the org file to HTMl and refreshes the Xwidgets preview."
  (org-html-export-to-html)
  (preview-org-html--reload-preview))

(defun preview-org-html--reload-preview ()
  "Reload xwidget preview."
  (xwidget-webkit-reload))

(defun preview-org-html--config-preview ()
  "Configure the method to preview (refresh only export or auto export and refresh on save)."
  (if (eq preview-org-html-auto-refresh-on-save t)
      (add-hook 'after-save-hook #'preview-org-html-refresh nil t) ;; Export/refresh on save.
      (advice-add 'org-html-export-to-html :after #'preview-org-html--reload-preview))) ;; Just reload on export, not on save.

(defun preview-org-html--unconfig ()
  "Unconfigure preview-org-html (remove hooks and advice)."
  (if (eq preview-org-html-auto-refresh-on-save t)
      (setq-local after-save-hook nil) ;; Export/refresh on save.
    (advice-remove 'org-html-export-to-html #'preview-org-html--reload-preview)) ;; Just reload on export, not on save.
  (remove-hook 'kill-buffer-hook #'preview-org-html-stop-preview t)
  (remove-hook 'kill-emacs-hook #'preview-org-html-stop-preview t)) 

(defun preview-org-html--open-xwidget ()
  "Open xwidgets browser on current file."
  (setq-local html-file-name (concat (file-name-sans-extension (concat "file://" buffer-file-name)) ".html"))
  (split-window-right)
  (other-window 1)
  (xwidget-webkit-browse-url html-file-name)
  (other-window 1)
  (preview-org-html--reload-preview))

(defun preview-org-html-start-preview ()
  "Begin the preview-org-html preview."
  (when buffer-file-name
    (add-hook 'kill-buffer-hook #'preview-org-html-stop-preview nil t)
    (add-hook 'kill-emacs-hook #'preview-org-html-stop-preview nil t)
    (cond ((derived-mode-p 'org-mode)
           (preview-org-html--open-xwidget)
           (preview-org-html--config-preview))
          (t
           (preview-org-html-mode -1)
           (user-error "`%s' not supported by preview-org-html preview, only org mode!" major-mode)))))

(defun preview-org-html-stop-preview ()
  "Stop the preview-org-html preview."
  (message "ended preview-org-html")
  (setq kill-buffer-query-functions nil)
  (preview-org-html--kill-matching-buffers-no-ask "\*xwidget\*")
  (setq kill-buffer-query-functions '(process-kill-buffer-query-function)) ;; reset back to default (this is a terrible way to do this)
  (preview-org-html--unconfig))


;;;###autoload
(define-minor-mode preview-org-html-mode
  "(Optionally) live preview for Org exports to HTML."
  :lighter " preview-org-html"
  (if preview-org-html-mode
      (preview-org-html-start-preview)
    (preview-org-html-stop-preview)))

(provide 'preview-org-html-mode)

;;; preview-org-html-mode.el ends here

