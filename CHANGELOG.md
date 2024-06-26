# 0.3.0
## Breaking changes
- Changed name from `hmenu` to `hdmenu`. Update your configuration paths accordingly.

## Added
-   Directories can now be evaluated recursively; for that, suffix the directory with two stars:

    ``` toml
        files = [ "~/.config/zsh/**" ]
    ```

# 0.2.4
## Added
-   Users may now specify directories in the files list.  This lists all their
    contents and puts those files in the list.
-   New command line option `--histFile` (short: `f`) to specify a history file
    to use.
-   New command line option `--files-only` (short: `o`) to *only* show the
    user-defined files.

## Changed
-   We now explicitly ignore directories in when they occur as sub directories
    of specified paths.
-   Fixed a bug related to parsing filenames with spaces.
-   Compatibility with GHC 9.2.5, dropped support for GHC < 8.10.x.

# 0.2.3
## Changed
-   The history file got a new layout.

# 0.2.2
## Added
-   Users may now specify a list of applications to be opened in a terminal.

# 0.2.1
## Changed
-   Files are not selected via prefixes anymore, but instead with a simple check
    if the selected item was in the files list.

# 0.2.0
## Added
-   Often used commands get displayed first.

# 0.1.0
Initial release
