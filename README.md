# lmno-blog-capture

**Simple Emacs minor mode to capture blog posts into a single Markdown file.**

## Features

- Prompts for title and makes your timestamp automatically
- Drop into a small capture window
- Append entries to your chosen Markdown file
- Minimal dependencies (built-in Emacs functions + markdown-mode)

## Installation

1. Place `lmno-blog-capture.el` in your `load-path`, e.g. `~/.emacs.d/lisp/`.
2. Add to your Emacs config:
   ```elisp
   (require 'lmno-blog-capture)
   ;; optional: customize target file
   (setq lmno-blog-capture-destination "~/myblog/posts.md")
   ;; bind a key
   (global-set-key (kbd "C-c n b") #'lmno-capture-post)
   ```
3. Restart Emacs or evaluate the buffer.

## Usage

- `M-x lmno-capture-post` or your keybinding (`C-c n b` by default)
- Enter a post title when prompted
- Write your content in the pop-up capture area
- `C-c C-c` to save and close, or `C-c C-k` to abort

## Customization

- **`lmno-blog-capture-destination`**: File path to append new posts. Customize via `M-x customize-group RET lmno-blog-capture RET`.

## Contributing

Bug reports and pull requests welcome on [GitHub](https://github.com/yourusername/lmno-blog-capture).

## License

MIT License
