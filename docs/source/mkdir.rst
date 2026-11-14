MKDIR(1)                                                                     General Commands Manual                                                                     MKDIR(1)

NAME
       mkdir - Create the given DIRECTORY(ies) if they do not exist

SYNOPSIS
       mkdir [-m|--mode] [-p|--parents] [-v|--verbose] [-Z ] [--context] [-h|--help] [-V|--version] <dirs>

DESCRIPTION
       Create the given DIRECTORY(ies) if they do not exist

OPTIONS
       -m, --mode
              set file mode (not implemented on windows)

       -p, --parents
              make parent directories as needed

       -v, --verbose
              print a message for each printed directory

       -Z     set SELinux security context of each created directory to the default type

       --context <CTX>
              like -Z, or if CTX is specified then set the SELinux or SMACK security context to CTX

       -h, --help
              Print help

       -V, --version
              Print version

       <dirs>

EXTRA
       Each MODE is of the form [ugoa]*([-+=]([rwxXst]*|[ugo]))+|[-+=]?[0-7]+.

VERSION
       v(uutils coreutils) 0.10.0

EXAMPLES
       Create specific directories:

       mkdir path/to/directory1 path/to/directory2 ...

       Create specific directories and their parents if needed:

       mkdir [-p|--parents] path/to/directory1 path/to/directory2 ...

       Create directories with specific permissions:

       mkdir [-m|--mode] rwxrw-r-- path/to/directory1 path/to/directory2 ...

       Create multiple nested directories recursively:

       mkdir [-p|--parents] path/to/{a,b}/{x,y,z}/{h,i,j}

       The  examples are provided by the tldr-pages project <https://tldr.sh> under the CC BY 4.0 License. Please note that, as uutils is a work in progress, some examples might
       fail.

                                                                                    2026-09-03                                                                           MKDIR(1)
