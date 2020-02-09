# hmenu

`hmenu` is a small wrapper for dmenu to make certain things easier.  It can't do
very much, though I guess that's the point of it.  It might be most notable for
being a discount `yeganesh`.

# Features

1.  Display commands in order of usage.
2.  Optional config file where one may specify the following:
    -   Files one wishes to edit.
    -   A custom opening script (default: "xdg-open").
    -   Custom dmenu executable (default: "dmenu").
    -   A terminal emulator     (default: "xterm").
    -   A list of programs to be opened in said terminal emulator.

`hmenu` is configured using a `.toml` file in an `hmenu` directory inside
`XDG_CONFIG_HOME` directory (probably `~/.config/hmenu`).  See the
`example.toml` for an example configuration.

`hmenu` has the following command line options:
    -   `--histFile`
        - Short: `-f`
        - Description: Path to the history file to use.

All options after "--" will be directly forwarded to `dmenu`, so you may specify
options in the following way:

    hmenu -f /path/to/file -- -i -f -nb '#282A36' -nf '#BBBBBB' -sb '#8BE9FD' -sf '#000000' -fn 'Inconsolata Regular-10'

If you want to only use the "display commands in order of usage feature" of
`hmenu`, check out [hmenu-hist](https://gitlab.com/slotThe/hmenu-hist).

# Installation

## Building

### Stack

Build with `stack build`, then copy the executable to a convenient location.

### Cabal

Build with `cabal v2-install hmenu`.
