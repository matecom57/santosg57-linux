PWD(1)                                                                       General Commands Manual                                                                       PWD(1)

NAME
       pwd - Display the full filename of the current working directory.

SYNOPSIS
       pwd [-L|--logical] [-P|--physical] [-h|--help] [-V|--version] [operands]

DESCRIPTION
       Display the full filename of the current working directory.

OPTIONS
       -L, --logical
              use PWD from environment, even if it contains symlinks

       -P, --physical
              avoid all symlinks

       -h, --help
              Print help

       -V, --version
              Print version

VERSION
       v(uutils coreutils) 0.10.0

EXAMPLES
       Print the current directory:

       pwd

       Print the current directory, and resolve all symlinks (i.e. show the "physical" path):

       pwd [-P|--physical]

       Display help:

       pwd --help

       The  examples are provided by the tldr-pages project <https://tldr.sh> under the CC BY 4.0 License. Please note that, as uutils is a work in progress, some examples might
       fail.

                                                                                    2026-09-03                                                                             PWD(1)
