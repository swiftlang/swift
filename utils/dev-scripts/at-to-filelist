#!/bin/bash

# Transform an @ file into a file list.
#
# The intended use case is given an @ path passed to swift, use this script and
# bash to automagically expand the @ path by transforming:
#
# @/foo/bar/baz.txt
#
#   =>
#
# <(at-to-filelist /foo/bar/baz.txt)
#
# For those unaware, <(...) causes bash to output the subshell's output into a
# file and then replace <(...) with the path to that temporary file.
#
# Example:
#
# Consider a swiftc command line that uses the @ symbol.
#
#   swiftc @/foo/bar/baz.txt
#
# When this is run, the @ command is expanded into a filelist in a temporary
# file. This doesn't work with -### since -### outputs the command line with a
# temporary file for the filelist, but uses a path to a temporary file that
# doesn't exist, e.g.:
#
#   swift -frontend -filelist /tmp/tmp.filelist ...
#
# To run this command, you use the at-to-filelist command as follows:
#
#   swift -frontend -filelist <(at-to-filelist /foo/bar/baz.txt)
cat ${1} | tr -s ";" "\n"

echo
