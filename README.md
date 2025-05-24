# lmno-blog-capture

**Simple Emacs minor mode to capture blog posts into a single Markdown file.**

_No affiliation with [lmno.lol][] besides thinking it's cool._

## Features

- Prompts for title and makes your timestamp automatically
- Drop into a small capture window
- Append entries to your chosen Markdown file
- Move posts between drafts and published files
- Minimal dependencies (built-in Emacs functions + markdown-mode)

## Installation

1. Place `lmno-blog-capture.el` in your `load-path`, e.g. `~/.emacs.d/lisp/`.
2. Add to your Emacs config:
   ```elisp
   (require 'lmno-blog-capture)
   ;; optional: customize target files
   (setq lmno-blog-capture-destination "~/myblog/posts.md")
   (setq lmno-blog-capture-drafts-file "~/myblog/drafts.md")
   ;; bind keys
   (global-set-key (kbd "C-c n b") #'lmno-capture-post)
   (global-set-key (kbd "C-c n m") #'lmno-move-heading-between-draft-and-published)
   ```
3. Restart Emacs or evaluate the buffer.

## Usage

### Capturing new posts
- `M-x lmno-capture-post` or your keybinding (`C-c n b` by default)
- Enter a post title when prompted
- Write your content in the pop-up capture area
- `C-c C-c` to save and close, or `C-c C-k` to abort

### Moving posts between drafts and published
- Navigate to a heading in either your drafts or published file
- `M-x lmno-move-heading-between-draft-and-published` or your keybinding (`C-c n m` by default)
- The heading and its content will be moved to the other file

## Customization

- **`lmno-blog-capture-destination`**: File path to append new posts (default: `~/blog/lmno.md`)
- **`lmno-blog-capture-drafts-file`**: File path for draft posts (default: `~/blog/drafts_lmno.md`)

Customize via `M-x customize-group RET lmno-blog-capture RET`.

## Contributing

Bug reports and pull requests welcome on [GitHub](https://github.com/pdxmph/lmno-blog-capture).

## License

MIT License


[lmno.lol]: https://lmno.lol
