# emacs-conf

## Usage

Probably requires a recent version of Emacs.

Put this in your `.emacs.el`:

```elisp
(add-to-list 'load-path "/path/to/emacs-conf/")
(load "top")
```

## General editing (top.el)
* Unsets "C-z" so that it can be used as a prefix elsewhere.
* Disables the menu bar, scroll bar, and toolbar.
* Add "C-x C-m" as an alternative to "M-x".
* Change various settings to encourage commands that want to show a
  buffer to avoid splitting an existing window or opening a new
  frame.
* Add "C-w" to do the same thing as "C-<backspace>".
* Add "C-x C-k" to call 'kill-region'.
* Add "C-zl" to toggle whether lines are truncated in the current
  buffer.
* Set shell colors (for use with the "shell" command.)
* Set a small 7x13 pixel font.
* Run 'server-start' on startup.
* Enable the commands downcase-region, upcase-region, and
  narrow-to-region.
* Change "M-m" to kill the entire current line (similar to "<home>",
  "C-k", "C-k").
* Change "C-a" to either go to the first non-whitespace character on
  the current line, or if already there, go to the absolute
  beginning of the line.
* Set the backup directory to "~/.emacs_backups".

## Development (dev.el)
* Show column numbers in the mode line.
* Scroll compilation output until the first error. (Bug: only works
  when compilation window is active.)
* Sets various c-mode options for Blender development
  in 'c-mode-blender-hook'.
* Enable electric-pair-mode, typing things like an open parenthesis
  or open bracket will automatically insert the closing one.
* Add the command 'c-section-comment'. Takes the text on the current
  line and surrounds it with /* ... */, padding with asterisks such
  that it ends up at 72 characters in length. Bound to "C-zs".
* Add commands 'compile-dir' and 'gud-gdb-dir', which do the same
  thing as 'compile' and 'gud-gdb', but ask for a directory
  first. They also protect the window after starting their
  command. gud-gdb-dir will also set the gud-gdb-colors-mode
  explained below. Bound to "C-z1" and "C-z2".
* Add keys "C-z C-z" and "C-zz" to comment and uncomment a region.
* Add key "C-zh" to switch between header and source file (with
  ff-find-other-file).
* Add key "C-zm" to open a manpage.
* Add key "C-zc" to recompile in the "*compilation*" buffer.

## Miscellaneous Features

### Compilation (compilation-always-kill.el)
Includes 'compilation-always-kill.el' by Kevin Ryde. This allows
starting a new compile while a compile is already running, without
asking if it's OK to kill.

### Git (git.el)
TODO: verify that the command in git.el aren't already available
through other means in Emacs.

The function 'git-root-dir' finds the absolute path of the git root
directory (assuming the current directory either is a git root
directory or a subdirectory of one.)

Note: many of the commands in git.el are not very robust if run
outside of a directory controlled by git (subdirectories are fine
though.)

Keys:
* "C-zvs" runs "git status"
* "C-zvcc" runs "git commit"
* "C-zvca" runs "git commit --amend"
* "C-zva" runs "git add filename", where filename is the current
  buffer's filename.
* "C-zvl" looks for the commit that last touched the current line
  of the current buffer. If found, the commit's log is echoed, and
  the commit hash is copied to the kill ring.
* "C-zvrr" starts an interactive rebase, with the starting commit
  of the same commit that "C-zvl" would find.
* "C-zvrc" runs "git rebase --continue"
* "C-c C-j" runs rgrep, but skips the question about what directory
  to search and uses the git root.
* "C-cf" takes a filename and tries to find it somewhere in the git
  repository. If a single result is found, the file is
  opened. Otherwise it will show all matching results.

### GUD (gud-gdb-colors-mode.el)
Adds a command 'gud-gdb-colors-mode' which gives gud-gdb nicer
syntax highlighting. Function names and line numbers in the
backtrace format will be highlighted.

### JSON (json-mode.el)
Includes 'json-mode.el' for nicer editing and syntax highlighting
of JSon files. (Not sure who original author is, not sure what the
license is either.)

### TeX (tex.el)
Modifies the 'latex-run-command' to output PDF rather than DVI,
modifies the 'tex-dvi-view-command' to run Evince rather than
xdvi. These are still bound to the default "C-c C-v" and "C-c C-b",
respectively.

### Theme (theme.el)
Includes a heavily modified version of the Zenburn theme by
Dirk-Jan C. Binnema. My version kinda kills the "nice low-contrast"
intent of the original, and uses Tango colors instead.

### Windows/Frames (window.el)
* Enable winner-mode by default.
* Go to the next window with "C-M-j".
* Go to the previous window with "C-M-k".
* Toggle the "protected" status of a window with "C-z C-w". A
  protected window won't be picked when a new buffer opens, helpful
  for things like a compilation buffer or shell that you always
  want around.
* Enter and exit a writing mode with "C-z ]" and "C-z [",
  respectively. When in writing mode, visual-line-mode wrapping is
  used, and the text is centered in the window with a width of 79
  characters.
* <f7> and <f8> split the window below and right, respectively. All
  windows in the frame will be rebalanced as well.

## License and Authorship
Some files list their own license. Anything else (written by Nicholas
Bishop) is public domain.

Note that I (Nicholas Bishop) do not claim to be wizardly with Emacs, so
much of what you find here might not be good!
