#!/usr/bin/env python3

# On Darwin, dynamic libraries have an install name.  At link time, the
# linker can work with a dylib anywhere in the filesystem, but it will
# write the dylib's install name into the resulting image, and at load
# time that dylib will normally be expected to be found at exactly that
# path.  However, if the install name in an image begins with `@rpath`,
# it will instead be searched for in the image's runtime search path
# list.  That list may contain absolute paths, but it may also contain
# paths beginning with `@executable_path` or `@loader_path`, meaning the
# path containing the running executable or the image being loaded,
# respectively.
#
# Many of Swift's dylibs are meant to be installed on the system, which
# means they have install names like this:
#   /usr/lib/swift/libswiftFoo.dylib
# To support back-deployment, they also provide magic override symbols
# ($ld$install_name) for all the OS versions preceding the addition of
# of the library.  When the linker finds a dylib with a matching override
# for the OS deployment target, it ignores the normal install name and
# uses the override path in the linked image's load command.  Swift's
# libraries use override paths that begin with `@rpath`, and Swift
# builds images with a runtime search path list that starts with
# /usr/lib/swift but then falls back on a path relative to the image;
# thus, apps will use the system libraries if available but will
# otherwise use fallback libraries.
#
# When we're working on Swift, we usually want to test the libraries
# we just built rather than the system libraries.  There are two ways
# to achieve that.  The first is to override dyld's runtime search path
# with DYLD_LIBRARY_PATH; this will take precedence over even an
# absolute install name.  The second is to make sure the dylibs are
# loaded via an @rpath install name and then link the program with an
# rpath that will use the just-built libraries.  Unfortunately, the
# toolchain will ordinarily use an absolute install name instead of
# an @rpath if the deployment target is old enough, subverting testing.
#
# This script looks for dependent dylibs with an absolute path in
# /usr/lib/swift and changes them to use @rpath.

import argparse
import re
import subprocess
import sys


def main(arguments):
    parser = argparse.ArgumentParser(
        description='Change absolute install names to use @rpath')
    parser.add_argument('bin', help='the binary')

    args = parser.parse_args(arguments)
    rpathize(args.bin)


def rpathize(filename):
    dylibsOutput = None

    try:
        # `dyldinfo` has been replaced with `dyld_info`, so we try it first
        # before falling back to `dyldinfo`
        dylibsOutput = subprocess.check_output(
            ['xcrun', 'dyld_info', '-dependents', filename],
            universal_newlines=True)
    except subprocess.CalledProcessError:
        sys.stderr.write("falling back to 'xcrun dyldinfo' ...\n")
        dylibsOutput = subprocess.check_output(
            ['xcrun', 'dyldinfo', '-dylibs', filename],
            universal_newlines=True)

    # The output from dyldinfo -dylibs is a line of header followed by one
    # install name per line, indented with spaces.
    dylib_regex = re.compile(
        r"(^|.*\s)(?P<path>/usr/lib/swift/(?P<filename>.*\.dylib))\s*$")

    # Build a command to invoke install_name_tool.
    command = ['install_name_tool']
    for line in dylibsOutput.splitlines():
        match = dylib_regex.match(line)
        if match:
            command.append('-change')
            command.append(match.group('path'))
            command.append('@rpath/' + match.group('filename'))
            continue

    # Don't run the command if we didn't find any dylibs to change:
    # it's invalid to invoke install_name_tool without any operations.
    if len(command) == 1:
        return

    # The last argument is the filename to operate on.
    command.append(filename)

    subprocess.check_call(command)


sys.exit(main(sys.argv[1:]))
