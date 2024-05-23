# hmenu

`hmenu` is a small wrapper around dmenu—it can't do very much, though I
guess that's the point. It might be most notable for being a discount
`yeganesh`.

## Features

1. Display commands in order of usage and, optionally, apply a frequency decay every time an item is selected.
2. Specify extra files to consider, which will be opened by a program of your choice (e.g., `xdg-open`).
3. Open certain executables inside of your terminal.

## Configuration

Hmenu is configured using a [TOML] file inside the `$XDG_CONFIG_HOME/hmenu` directory (probably `~/.config/hmenu`).
See `example.toml` for an example configuration.

Note that, while having a config file is optional,
the `$XDG_CONFIG_HOME/hmenu` directory will be created in either case,
in order to store the history file there.

### Configuration option

The configuration file takes the following arguments:

  - `open` : A custom opening script (default: `xdg-open`).

  - `files`: Files one wishes to edit; they will be opened according to
    `open`. This can also take directories

    ``` toml
    files = [ "~/.config/emacs/" ]
    ```

    in which case all *files* in that directory will be added to the list of files that `hmenu` cares about.
    Directories can also be traversed recursively:

    ``` toml
    # Files from `~/.config/emacs/' and all of its subdirectories.
    files = [ "~/.config/emacs/" ]
    ```

  - `executable`: Custom dmenu executable (default: `dmenu`).

  - `terminal`: A terminal emulator (default: `xterm`).

  - `tty-programs`: A list of programs to be opened in the above
    terminal emulator. For example, specifying

    ``` toml
    tty-programs = [ "htop" ]
    ```

    will effectively enable one to start `htop` directly from dmenu without having to open an extraneous terminal.

  - `decay`: A frequency decay; all non-selected items will be multiplied by this number (default: `1`).
    A good value may be something like `0.997`

### Command line options

  - `--histFile`
      - Short: `-f`
      - Description: Path to the history file to use.

  - `--files-only`
      - Short: `-o`
      - Description: Whether to only show the user-specified files.

  - `--decay`
      - Short: `-d`
      - Description: A frequency decay; all non-selected items will be
        multiplied by this

All options after `--` will be directly forwarded to `dmenu`, so you may
specify options in the following way:

    hmenu -f /path/to/file -- -i -f -nb '#282A36' -nf '#BBBBBB' -sb '#8BE9FD' -sf '#000000' -fn 'Inconsolata Regular-10'

If you want to only use the "display commands in order of usage feature" of `hmenu`, check out [hmenu-hist].

# Installation

## Stack

Build with `stack build`, then copy the executable to a convenient
location (or just use `stack install`, to copy the executable to the
local-bin-path).

## Cabal

Build with `cabal install`.

[TOML]: https://github.com/toml-lang/toml
[hmenu-hist]: https://gitlab.com/slotThe/hmenu-hist
