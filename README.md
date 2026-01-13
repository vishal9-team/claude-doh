# claude-doh
Minimal emacs + cc support - YMMV

1. Load the file

(load-file "/full/path/to/claude-watch.el")

2. Customize this command variable
   
(setq claude-launch-command
      "cd %s && source ~/.bashrc && conda activate llm && claude")
