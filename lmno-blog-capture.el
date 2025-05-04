;;; lmno-blog-capture.el --- Simple blog capture minor mode  -*- lexical-binding: t; -*-

;; Author: Mike
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, tools, blog
;; URL: https://github.com/pdxmph/lmno-blog-capture

;;; Commentary:
;; A tiny minor mode and capture function to write quick blog posts
;; into a single Markdown file. Provides customizable destination file.

;;; Code:

(defgroup lmno-blog-capture nil
  "Quickly capture blog posts into a Markdown file."
  :group 'convenience
  :prefix "lmno-blog-capture-")

(defcustom lmno-blog-capture-destination "~/blog/lmno.md"
  "Filepath to which captured blog entries will be appended."
  :type 'file
  :group 'lmno-blog-capture)

(define-minor-mode lmno-blog-capture-mode
  "Mode for capturing blog entries like org-capture."
  :lighter ">> BlogCapture"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'lmno-blog-capture-finalize)
            (define-key map (kbd "C-c C-k") #'lmno-blog-capture-abort)
            map))

(defun lmno-capture-post ()
  "Start a blog post capture session in a temporary buffer."
  (interactive)
  (let* ((title (read-string "Post title: "))
         (date (format-time-string "%Y-%m-%d"))
         (heading (format "# [%s] %s\n\n" date title))
         (buf (get-buffer-create "*LMNO Blog Capture*")))
    (split-window-below -12)
    (other-window 1)
    (switch-to-buffer buf)
    (erase-buffer)
    (insert heading)
    (markdown-mode)
    (lmno-blog-capture-mode 1)
    (message "Write your post. C-c C-c to save, C-c C-k to cancel.")))

(defun lmno-blog-capture-finalize ()
  "Save the capture to the blog file and close the capture window."
  (interactive)
  (let ((content (buffer-string))
        (dest lmno-blog-capture-destination))
    (with-current-buffer (find-file-noselect dest)
      (goto-char (point-min))
      (insert content "\n")
      (save-buffer))
    (lmno--close-capture-window)
    (message "Blog post saved.")))

(defun lmno-blog-capture-abort ()
  "Cancel the blog capture."
  (interactive)
  (lmno--close-capture-window)
  (message "Blog capture aborted."))

(defun lmno--close-capture-window ()
  "Close the capture buffer and its window if more than one real window remains;
otherwise just kill the buffer."
  (let ((buf (current-buffer)))
    (when (> (count-windows) 1)
      (delete-window))
    (kill-buffer buf)))


;; Keybinding
;; Add to your config:
;; (require 'lmno-blog-capture)
;; (global-set-key (kbd "C-c n b") #'lmno-capture-post)

(provide 'lmno-blog-capture)
;;; lmno-blog-capture.el ends here
