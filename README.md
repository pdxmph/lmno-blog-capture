# lmno-blog-capture

**Simple Emacs minor mode to capture blog posts into a single Markdown file.**

_No affiliation with [lmno.lol][] besides thinking it's cool._

## Features

- Prompts for title and makes your timestamp automatically
- Drop into a small capture window
- Append entries to your chosen Markdown file
- **Draft support**: Maintain separate draft and published files, easily move posts between them
- Minimal dependencies (built-in Emacs functions + markdown-mode)

## How Drafts Work

This package supports a two-file workflow:
- **Published file**: Your main blog file where finished posts live
- **Drafts file**: A separate file for work-in-progress posts

You can capture new posts to either file, then move individual posts between them as needed. This lets you work on posts over time before publishing them to your main blog.

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
- Open either your drafts or published file in Emacs
- Place your cursor anywhere within the post you want to move
- `M-x lmno-move-heading-between-draft-and-published` or your keybinding (`C-c n m` by default)
- The entire post (heading and content) moves to the top of the other file
- Works in both directions: draft→published or published→draft

## Customization

- **`lmno-blog-capture-destination`**: File path to append new posts (default: `~/blog/lmno.md`)
- **`lmno-blog-capture-drafts-file`**: File path for draft posts (default: `~/blog/drafts_lmno.md`)

Customize via `M-x customize-group RET lmno-blog-capture RET`.

## Contributing

Bug reports and pull requests welcome on [GitHub](https://github.com/pdxmph/lmno-blog-capture).

## License

MIT License


[lmno.lol]: https://lmno.lol
