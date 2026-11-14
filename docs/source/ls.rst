LS(1)                                                                        General Commands Manual                                                                        LS(1)
=====

NAME
       ls - List directory contents.  Ignore files and directories starting with a '.' by default

SYNOPSIS
       ls  [--help]  [--format]  [-C  ]  [-l|--long]  [-x  ]  [-T|--tabsize] [-m ] [--zero] [-D|--dired] [--hyperlink] [-1 ] [-o ] [-g ] [-n|--numeric-uid-gid] [--quoting-style]
       [-N|--literal] [-b|--escape] [-Q|--quote-name] [-q|--hide-control-chars] [--show-control-chars] [--time] [-c ] [-u ] [--hide] [-I|--ignore] [-B|--ignore-backups] [--sort]
       [-S ] [-t ] [-v ] [-X ]  [-U  ]  [-L|--dereference]  [--dereference-command-line-symlink-to-dir]  [-H|--dereference-command-line]  [-G|--no-group]  [--author]  [-a|--all]
       [-A|--almost-all]  [-f  ]  [-d|--directory]  [-h|--human-readable]  [-k|--kibibytes]  [--si]  [--block-size]  [-i|--inode]  [-r|--reverse]  [-R|--recursive]  [-w|--width]
       [-s|--size] [--color] [--indicator-style] [-F|--classify] [--file-type] [-p  ]  [--time-style]  [--full-time]  [-Z|--context]  [--group-directories-first]  [-V|--version]
       [paths]

DESCRIPTION
       List directory contents.  Ignore files and directories starting with a '.' by default

OPTIONS
       --help Print help information.

       --format
              Set the display format.

       -C     Display the files in columns.

       -l, --long
              Display detailed information.

       -x     List entries in rows instead of in columns.

       -T, --tabsize <COLS>
              Assume tab stops at each COLS instead of 8

       -m     List entries separated by commas.

       --zero List entries separated by ASCII NUL characters.

       -D, --dired
              generate output designed for Emacs' dired (Directory Editor) mode

       --hyperlink[=<WHEN>] [default: never]
              hyperlink file names WHEN

              Possible values:

                     • always

                     • auto

                     • never

       -1     List one file per line.

       -o     Long format without group information.  Identical to --format=long with --no-group.

       -g     Long format without owner information.

       -n, --numeric-uid-gid
              -l with numeric UIDs and GIDs.

       --quoting-style
              Set quoting style.

              Possible values:

                     • literal

                     • locale

                     • shell

                     • shell-escape

                     • shell-always

                     • shell-escape-always

                     • clocale

                     • c

                     • escape

       -N, --literal
              Use literal quoting style. Equivalent to `--quoting-style=literal`

       -b, --escape
              Use escape quoting style. Equivalent to `--quoting-style=escape`

       -Q, --quote-name
              Use C quoting style. Equivalent to `--quoting-style=c`

       -q, --hide-control-chars
              Replace control characters with '?' if they are not escaped.

       --show-control-chars
              Show control characters 'as is' if they are not escaped.

       --time <field>
              Show  time  in  `<field>`: access time (-u): atime, access, use; change time (-t): ctime, status.  modification time: mtime, modification.  birth time: birth, cre‐
              ation;

       -c     If the long listing format (e.g., -l, -o) is being used, print the status change time (the 'ctime' in the inode) instead of the modification time. When  explicitly
              sorting by time (--sort=time or -t) or when not using a long listing format, sort according to the status change time.

       -u     If  the  long  listing  format  (e.g.,  -l,  -o)  is  being  used,  print  the status access time instead of the modification time. When explicitly sorting by time
              (--sort=time or -t) or when not using a long listing format, sort according to the access time.

       --hide <PATTERN>
              do not list implied entries matching shell PATTERN (overridden by -a or -A)

       -I, --ignore <PATTERN>
              do not list implied entries matching shell PATTERN

       -B, --ignore-backups
              Ignore entries which end with ~.

       --sort <field>
              Sort by `<field>`: name, none (-U), time (-t), size (-S), extension (-X) or width

              Possible values:

                     • name

                     • none

                     • time

                     • size

                     • version

                     • extension

                     • width

       -S     Sort by file size, largest first.

       -t     Sort by modification time (the 'mtime' in the inode), newest first.

       -v     Natural sort of (version) numbers in the filenames.

       -X     Sort alphabetically by entry extension.

       -U     Do not sort; list the files in whatever order they are stored in the directory.  This is especially useful when listing very large directories, since not doing any
              sorting can be noticeably faster.

       -L, --dereference
              When showing file information for a symbolic link, show information for the file the link references rather than the link itself.

       --dereference-command-line-symlink-to-dir
              Do not follow symlinks except when they link to directories and are given as command line arguments.

       -H, --dereference-command-line
              Do not follow symlinks except when given as command line arguments.

       -G, --no-group
              Do not show group in long format.

       --author
              Show author in long format. On the supported platforms, the author always matches the file owner.

       -a, --all
              Do not ignore hidden files (files with names that start with '.').

       -A, --almost-all
              In a directory, do not ignore all file names that start with '.', only ignore '.' and '..'.

       -f     List all files in directory order, unsorted. Equivalent to -aU. Disables --color unless explicitly specified.

       -d, --directory
              Only list the names of directories, rather than listing directory contents.  This will not follow symbolic links unless one of  `--dereference-command-line  (-H)`,
              `--dereference (-L)`, or `--dereference-command-line-symlink-to-dir` is specified.

       -h, --human-readable
              Print human readable file sizes (e.g. 1K 234M 56G).

       -k, --kibibytes
              default to 1024-byte blocks for file system usage; used only with -s and per directory totals

       --si   Print human readable file sizes using powers of 1000 instead of 1024.

       --block-size <BLOCK_SIZE>
              scale sizes by BLOCK_SIZE when printing them

       -i, --inode
              print the index number of each file

       -r, --reverse
              Reverse whatever the sorting method is e.g., list files in reverse alphabetical order, youngest first, smallest first, or whatever.

       -R, --recursive
              List the contents of all directories recursively.

       -w, --width <COLS>
              Assume that the terminal is COLS columns wide.

       -s, --size
              print the allocated size of each file, in blocks

       --color
              Color output based on file type.

              Possible values:

                     • always

                     • auto

                     • never

       --indicator-style
              Append indicator with style WORD to entry names: none (default),  slash (-p), file-type (--file-type), classify (-F)

              Possible values:

                     • none

                     • slash

                     • file-type

                     • classify

       -F, --classify[=<when>]
              Append a character to each file name indicating the file type. Also, for regular files that are executable, append '*'. The file type indicators are '/' for direc‐
              tories, '@' for symbolic links, '|' for FIFOs, '=' for sockets, '>' for doors, and nothing for regular files. when may be omitted, or one of:
                  none - Do not classify. This is the default.
                  auto - Only classify if standard output is a terminal.
                  always - Always classify.  Specifying --classify and no when is equivalent to --classify=always. This will not follow symbolic links listed on the command line
              unless the --dereference-command-line (-H), --dereference (-L), or --dereference-command-line-symlink-to-dir options are specified.

              Possible values:

                     • always

                     • auto

                     • never

       --file-type
              Same as --classify, but do not append '*'

       -p     Append / indicator to directories.

       --time-style <TIME_STYLE>
              time/date format with -l; see TIME_STYLE below

       --full-time
              like -l --time-style=full-iso

       -Z, --context
              print any security context of each file

       --group-directories-first
              group directories before files; can be augmented with a --sort option, but any use of --sort=none (-U) disables grouping

       -V, --version
              Print version

EXTRA
       The  TIME_STYLE  argument can be full-iso, long-iso, iso, locale or +FORMAT. FORMAT is interpreted like in date. Also the TIME_STYLE environment variable sets the default
       style to use.

VERSION
       v(uutils coreutils) 0.10.0

EXAMPLES
       List files one per line:

       ls -1

       List all files, including hidden files:

       ls [-a|--all]

       List files with a trailing symbol to indicate file type (directory/, symbolic_link@, executable*, ...):

       ls [-F|--classify]

       List all files in [l]ong format (permissions, ownership, size, and modification date):

       ls [-la|-l --all]

       List files in [l]ong format with size displayed using human-readable units (KiB, MiB, GiB):

       ls [-lh|-l --human-readable]

       List files in [l]ong format, sorted by [S]ize (descending) recursively:

       ls [-lSR|-lS --recursive]

       List files in [l]ong format, sorted by [t]ime the file was modified and in reverse order (oldest first):

       ls [-ltr|-lt --reverse]

       Only list directories:

       ls [-d|--directory] */

       The examples are provided by the tldr-pages project <https://tldr.sh> under the CC BY 4.0 License. Please note that, as uutils is a work in progress, some examples  might
       fail.

                                                                                    2026-09-03                                                                              LS(1)
