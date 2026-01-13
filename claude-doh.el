;;; claude-doh.el --- Auto-pop Claude vterm buffer when generation finishes -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup claude-doh nil
  "Run and monitor Claude sessions in vterm."
  :group 'tools)

(defcustom claude-last-directory "~/"
  "Last directory used for `claude-vterm`."
  :type 'directory
  :group 'claude-doh)

(defcustom claude-launch-command
  "cd %s && source ~/.bashrc && conda activate pytorch-test && claude"
  "Shell command template used to launch Claude in vterm. The directory path is substituted for %s using `shell-quote-argument`."
  :type 'string
  :group 'claude-doh)

(defconst claude-running-regexp
  "✻.*esc to interrupt"
  "Regexp present while Claude is actively generating output.")

;; Per-buffer state (important: multiple Claude buffers must not interfere)
(defvar-local claude-last-point-max 0)
(defvar-local claude-was-running nil)
(defvar-local claude-stable-ticks 0)

(defun claude-watch-buffer (buf)
  "Poll vterm BUF and split window when Claude finishes generating output."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((current-max (point-max))
             (tail (buffer-substring-no-properties
                    (max (point-min) (- (point-max) 800))
                    (point-max)))
             (running-now (string-match-p claude-running-regexp tail)))

        ;; Record that Claude has entered the running state
        (when running-now
          (setq claude-was-running t))

        ;; Count consecutive stable polls
        (if (= current-max claude-last-point-max)
            (cl-incf claude-stable-ticks)
          (setq claude-stable-ticks 0))

        (setq claude-last-point-max current-max)

        ;; Completion condition:
        ;; 1. Claude was previously running
        ;; 2. Running marker is gone
        ;; 3. Output stable for at least 2 polls
        (when (and claude-was-running
                   (not running-now)
                   (>= claude-stable-ticks 2))
          (setq claude-was-running nil
                claude-stable-ticks 0)
          (unless (get-buffer-window buf t)
            ;; Like `C-x 3`, but keep focus in the original window
            (let ((win (split-window-right)))
              (set-window-buffer win buf))))

        ;; Continue polling
        (run-at-time 1 nil #'claude-watch-buffer buf)))))

(defun claude-vterm ()
  "Start a Claude session in vterm and watch for completion."
  (interactive)
  (let* ((dir (read-directory-name "Directory: " claude-last-directory))
         (folder-name
          (file-name-nondirectory
           (directory-file-name (expand-file-name dir))))
         (name (read-string "Buffer name: "))
         (buf-name (format "*cc-%s-%s*" folder-name name)))
    (setq claude-last-directory dir)
    (require 'vterm)
    (let ((vterm-buffer-name buf-name))
      (vterm))
    ;; Initialize per-buffer watcher state
    (with-current-buffer (current-buffer)
      (setq claude-last-point-max 0
            claude-was-running nil
            claude-stable-ticks 0)
      (run-at-time 2 nil #'claude-watch-buffer (current-buffer)))
    ;; Launch Claude using configurable command
    (vterm-send-string
     (concat
      (format claude-launch-command
              (shell-quote-argument (expand-file-name dir)))
      "\n"))))

(provide 'claude-doh)
;;; claude-doh.el ends here
