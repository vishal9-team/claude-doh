;;; claude-doh.el --- Auto-pop Claude vterm buffer when generation finishes -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup claude-doh nil "Run and monitor Claude sessions in vterm." :group 'tools)

(defcustom claude-last-directory "~/" "Last directory for `claude-vterm`." :type 'directory :group 'claude-doh)
(defcustom claude-launch-command "cd %s && source ~/.bashrc && conda activate pytorch-test && claude"
  "Shell command template for launching Claude (%s = directory)." :type 'string :group 'claude-doh)

(defconst claude-running-regexp "âœ».*esc to interrupt" "Regexp present while Claude is generating.")

(defvar-local claude-last-point-max 0)
(defvar-local claude-was-running nil)
(defvar-local claude-stable-ticks 0)
(defvar-local claude-buffer-marker nil)
(defvar-local claude-watch-timer nil)

;; Notification
(defun claude-watch-buffer (buf)
  "Poll vterm BUF and split window when Claude finishes."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((tail (buffer-substring-no-properties (max (point-min) (- (point-max) 800)) (point-max)))
             (running (string-match-p claude-running-regexp tail)))
        (when running (setq claude-was-running t))
        (if (= (point-max) claude-last-point-max) (cl-incf claude-stable-ticks) (setq claude-stable-ticks 0))
        (setq claude-last-point-max (point-max))
        (when (and claude-was-running (not running) (>= claude-stable-ticks 2))
          (setq claude-was-running nil claude-stable-ticks 0)
          (unless (get-buffer-window buf t)
            (set-window-buffer (split-window-right) buf)))
        (setq claude-watch-timer (run-at-time 1 nil #'claude-watch-buffer buf))))))

(defun claude-restart-watch (&optional buf)
  "Restart claude watch timer for BUF (defaults to current buffer)."
  (interactive)
  (let ((buf (or buf (current-buffer))))
    (with-current-buffer buf
      (when (timerp claude-watch-timer) (cancel-timer claude-watch-timer))
      (claude-watch-buffer buf))))

;; Buffer Management
(defun claude-buffer-p (buf)
  "Return non-nil if BUF is a Claude buffer."
  (and (buffer-live-p buf) (buffer-local-value 'claude-buffer-marker buf)))

(defun claude-read-buffer ()
  "Prompt for a Claude buffer."
  (let ((bufs (seq-filter #'claude-buffer-p (buffer-list))))
    (unless bufs (user-error "No Claude buffers"))
    (get-buffer (completing-read "Claude: " (mapcar #'buffer-name bufs) nil t))))

(defun claude-list-buffers ()
  "Switch to a Claude buffer." (interactive) (switch-to-buffer (claude-read-buffer)))

;; Prompt by region
(defun claude-send-region (beg end)
  "Send region to a Claude buffer."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)) (buf (claude-read-buffer)))
    (with-current-buffer buf (vterm-send-string text))
    (pop-to-buffer buf)))

;; Launch
(defun claude-vterm ()
  "Start a Claude session in vterm."
  (interactive)
  (let* ((dir (read-directory-name "Directory: " claude-last-directory))
         (name (read-string "Buffer name: "))
         (buf-name (format "*cc-%s-%s*" (file-name-nondirectory (directory-file-name (expand-file-name dir))) name)))
    (setq claude-last-directory dir)
    (require 'vterm)
    (let ((vterm-buffer-name buf-name)) (vterm))
    (setq-local claude-buffer-marker t claude-last-point-max 0 claude-was-running nil claude-stable-ticks 0)
    (run-at-time 2 nil #'claude-watch-buffer (current-buffer))
    (vterm-send-string (concat (format claude-launch-command (shell-quote-argument (expand-file-name dir))) "\n"))))

(global-set-key (kbd "C-c p c") 'claude-vterm)
(global-set-key (kbd "C-c p l") 'claude-list-buffers)
(global-set-key (kbd "C-c p s") 'claude-send-region)

(provide 'claude-doh)
;;; claude-doh.el ends here
