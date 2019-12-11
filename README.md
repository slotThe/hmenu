# hmenu

hmenu is a small wrapper for dmenu to make certain things easier.  It can't do
very much right now, but this might change in the near future.


# Features

`hmenu` is configured using a `.toml` file in an `hmenu` directory inside
`XDG_CONFIG_HOME` directory (probably `~/.config/hmenu`).  In this config file
you may specify:

-   Files you wish to edit (Custom prefix for files available).
-   A custom opening script.
-   Your custom dmenu executable.

See the `example.toml` for an example configuration.

At the moment, all arguments that passed to `hmenu` will be directly forwarded
to `dmenu` (this may change in the future when we get our own command line
options), so you may specify options in the following way:

    hmenu -i -f -nb '#282A36' -nf '#BBBBBB' -sb '#8BE9FD' -sf '#000000' -fn 'Inconsolata Regular-10'


# Installation

Build with `stack build`, then copy the executable to a convenient location.
