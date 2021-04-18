# hmenu

`hmenu` is a small wrapper for dmenu to make certain things easier.  It
can't do very much, though I guess that's the point of it.  It might be
most notable for being a discount `yeganesh`.

## Features

1. Display commands in order of usage.
2. Optionally, apply a frequency decay every time an item is selected.
3. Open certain files inside of your terminal or according to a given
   `open` script.

## Configuration

`hmenu` is configured using a `.toml` file in an `hmenu` directory
inside `$XDG_CONFIG_HOME` directory (probably `~/.config/hmenu`).  See
the `example.toml` for an example configuration.

The config file is _optional_ but we will create the
`$XDG_CONFIG_HOME/hmenu` directory in either case in order to store the
history file there.

The config file takes the following arguments:
  - `open` : A custom opening script (default: `xdg-open`).
  - `files`: Files one wishes to edit; they will be opened according to
    `open`.
  - `executable`: Custom dmenu executable (default: `dmenu`).
  - `terminal`: A terminal emulator (default: `xterm`).
  - `tty-programs`: A list of programs to be opened in the above
    terminal emulator.
  - `decay`: A frequency decay; all non-selected items will be
    multiplied by this number (default: `1`).  A good value may be
    something like `0.997`.

`hmenu` also has the following command line options:

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

If you want to only use the "display commands in order of usage feature"
of `hmenu`, check out [hmenu-hist]

## Installation

### Building

#### Stack

Build with `stack build`, then copy the executable to a convenient
location (or just use `stack install`, to copy the executable to the
local-bin-path).

#### Cabal

Build with `cabal install`.

[hmenu-hist]: https://gitlab.com/slotThe/hmenu-hist
