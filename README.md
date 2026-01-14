# claude-doh

Minimal emacs support for claude (things that make even Homer go 'DOH!') - YMMV

![Demo](assets/simpson-doh.gif)

# Requirements

Install *vterm* https://github.com/akermu/emacs-libvterm and optionally *Ivy* https://github.com/abo-abo/swiper for better completion.
  
# Setup
1. Save and load the file

(load-file "/full/path/to/claude-doh.el")

2. Customize Claude Code launch variable

(setq claude-launch-command
      "cd %s && source ~/.bashrc && conda activate llm && claude")

# Commands
| Command | Keybinding | Description |
|---------|------------|-------------|
| `claude-vterm` | `C-c p c` | Start a new Claude session in vterm |
| `claude-list-buffers` | `C-c p l` | Switch to an existing Claude buffer |
| `claude-send-region` | `C-c p s` | Send selected region to a Claude buffer |      
| `claude-restart-watch` | — | Restart the completion watcher for current buffer |                                                                                                                               
 
