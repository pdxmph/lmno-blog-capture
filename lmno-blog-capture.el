;;; lmno-blog-capture.el --- Simple blog capture minor mode  -*- lexical-binding: t; -*-

;; Author: Mike
;; Version: 0.2
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, tools, blog
;; URL: https://github.com/pdxmph/lmno-blog-capture

;;; Commentary:
;; A tiny minor mode and capture function to write quick blog posts
;; into a single Markdown file. Provides customizable destination file.

;;; Configuration:
;; (setq lmno-blog-capture-destination "~/bigbox/lmno.md")

;;; Code:

(defgroup lmno-blog-capture nil
  "Quickly capture blog posts into a Markdown file."
  :group 'convenience
  :prefix "lmno-blog-capture-")

(defcustom lmno-blog-capture-destination "~/blog/lmno.md"
  "Filepath to which captured blog entries will be appended."
  :type 'file
  :group 'lmno-blog-capture)

(defcustom lmno-blog-capture-drafts-file "~/blog/drafts_lmno.md"
  "Filepath for draft blog entries."
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
         (timestamp (format-time-string "%Y-%m-%d %a"))
         (heading (format "# [%s] %s\n\n" timestamp title))
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
      ;; Add blank line if file has content and doesn't start with blank line
      (when (and (not (= (point-max) (point-min)))
                 (not (looking-at "^$")))
        (insert "\n"))
      (goto-char (point-min))
      (insert content)
      ;; Ensure content ends with newline
      (unless (= (char-before) ?\n)
        (insert "\n"))
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

(defun lmno-move-heading-between-draft-and-published ()
  "Move current Markdown L1 heading between drafts and published files.
If in drafts file, moves to published. If in published, moves to drafts.
Always inserts at the beginning of the target file."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (published-file (expand-file-name lmno-blog-capture-destination))
         (drafts-file (expand-file-name lmno-blog-capture-drafts-file))
         (target-file (cond
                       ((string= (file-truename current-file) (file-truename drafts-file)) 
                        published-file)
                       ((string= (file-truename current-file) (file-truename published-file)) 
                        drafts-file)
                       (t (user-error "This file is not the drafts or published blog file")))))
    ;; Save and cut the heading
    (save-excursion
      ;; Find the beginning of the current L1 heading
      (beginning-of-line)
      (while (and (not (looking-at "^# ")) (not (bobp)))
        (forward-line -1))
      (unless (looking-at "^# ")
        (user-error "Not on a Markdown L1 heading"))
      (let ((start (point)))
        ;; Find the end of the current heading (next L1 heading or end of buffer)
        (forward-line 1)
        (while (and (not (looking-at "^# ")) (not (eobp)))
          (forward-line 1))
        ;; Cut the heading and its content
        (let ((heading-text (buffer-substring-no-properties start (point))))
          (delete-region start (point))
          ;; Paste into target file at beginning
          (with-current-buffer (find-file-noselect target-file)
            (goto-char (point-min))
            ;; Add blank line if file has content and doesn't start with blank line
            (when (and (not (= (point-max) (point-min)))
                       (not (looking-at "^$")))
              (insert "\n"))
            (goto-char (point-min))
            (insert heading-text)
            ;; Ensure content ends with newline
            (unless (= (char-before) ?\n)
              (insert "\n"))
            (save-buffer)))))
    (message "Moved heading to %s" (file-name-nondirectory target-file))))


;; Keybinding
;; Add to your config:
;; (require 'lmno-blog-capture)
;; (global-set-key (kbd "C-c n b") #'lmno-capture-post)
;; (global-set-key (kbd "C-c n m") #'lmno-move-heading-between-draft-and-published)
;;
;; For Doom Emacs users:
;; (map! :leader
;;       :desc "Blog commands"
;;       "l" '(:ignore t :which-key "blog")
;;       :desc "Move heading to other blog file"
;;       "l r" #'lmno-move-heading-between-draft-and-published
;;       :desc "New lmno blog entry"
;;       "l b" #'lmno-capture-post)

(provide 'lmno-blog-capture)
;;; lmno-blog-capture.el ends here
