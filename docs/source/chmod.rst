CHMOD(1)                                                                     General Commands Manual                                                                     CHMOD(1)

NAME
       chmod - Change the mode of each FILE to MODE.  With --reference, change the mode of each FILE to that of RFILE.

SYNOPSIS
       chmod  [--help]  [-c|--changes]  [-f|--quiet]  [-v|--verbose]  [--no-preserve-root]  [--preserve-root]  [-R|--recursive]  [--reference]  [-H ] [-L ] [-P ] [--dereference]
       [-h|--no-dereference] [-V|--version] [MODE] [FILE]

DESCRIPTION
       Change the mode of each FILE to MODE.  With --reference, change the mode of each FILE to that of RFILE.

OPTIONS
       --help Print help information.

       -c, --changes
              like verbose but report only when a change is made

       -f, --quiet
              suppress most error messages

       -v, --verbose
              output a diagnostic for every file processed

       --no-preserve-root
              do not treat '/' specially (the default)

       --preserve-root
              fail to operate recursively on '/'

       -R, --recursive
              change files and directories recursively

       --reference
              use RFILE's mode instead of MODE values

       -H     if a command line argument is a symbolic link to a directory, traverse it

       -L     traverse every symbolic link to a directory encountered

       -P     do not traverse any symbolic links (default)

       --dereference
              affect the referent of each symbolic link (this is the default), rather than the symbolic link itself

       -h, --no-dereference
              affect symbolic links instead of any referenced file (useful only on systems that can change the ownership of a symlink)

       -V, --version
              Print version

       [MODE]

       [FILE]

EXTRA
       Each MODE is of the form [ugoa]*([-+=]([rwxXst]*|[ugo]))+|[-+=]?[0-7]+.

VERSION
       v(uutils coreutils) 0.10.0

EXAMPLES
       Give the [u]ser who owns a file the right to e[x]ecute it:

       chmod u+x path/to/file

       Give the [u]ser rights to [r]ead and [w]rite to a file/directory:

       chmod u+rw path/to/file_or_directory

       Remove e[x]ecutable rights from the [g]roup:

       chmod g-x path/to/file

       Give [a]ll users rights to [r]ead and e[x]ecute:

       chmod a+rx path/to/file

       Give [o]thers (not in the file owner's group) the same rights as the [g]roup:

       chmod o=g path/to/file

       Remove all rights from [o]thers:

       chmod o= path/to/file

       Change permissions recursively giving [g]roup and [o]thers the ability to [w]rite:

       chmod [-R|--recursive] g+w,o+w path/to/directory

       Recursively give [a]ll users [r]ead permissions to files. Also give e[X]ecute permissions to files that have at least one execution permission and to all sub-directories:

       chmod [-R|--recursive] a+rX path/to/directory

       The examples are provided by the tldr-pages project <https://tldr.sh> under the CC BY 4.0 License. Please note that, as uutils is a work in progress, some examples  might
       fail.

                                                                                    2026-09-03                                                                           CHMOD(1)
