#!/usr/bin/env python3

# Postprocess binaries to prepare for their execution on Darwin platforms.
# This includes un-rpathization and codesigning.

import argparse
import os
import re
import subprocess
import sys

utils = os.path.dirname(os.path.realpath(__file__))
get_task_allow_plist = os.path.join(utils, 'get-task-allow.plist')


def main(arguments):
    parser = argparse.ArgumentParser(
        description='Postprocess binaries to prepare for \
                     their execution on Darwin platforms')
    parser.add_argument('bins', nargs='+', help='one or more binary files')

    args = parser.parse_args(arguments)

    for bin in args.bins:
        unrpathize(bin)
        codesign(bin)


# This function rewrites binaries that use these `@rpath`-based load
# commands to use direct /usr/lib/swift paths instead, to work around
# an issue where the DYLD_LIBRARY_PATH override doesn't reliably work
# with runpath-relative loads of system libraries on some dyld versions.
# (rdar://78851265)
def unrpathize(filename):
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

    # Do not rewrite @rpath-relative load commands for these libraries:
    # they are test support libraries that are never installed under
    # /usr/lib/swift and they aren't loaded via DYLD_LIBRARY_PATH.
    allow_list = {
        'libswiftDifferentiationUnittest.dylib',
        'libswiftLocalizationAnalysisTestHelper.dylib',
        'libswiftOSLogTestHelper.dylib',
        'libswiftRemoteMirror.dylib',
        'libswiftRuntimeUnittest.dylib',
        'libswiftStdlibCollectionUnittest.dylib',
        'libswiftStdlibUnicodeUnittest.dylib',
        'libswiftStdlibUnittest.dylib',
        'libswiftStdlibUnittestFoundationExtras.dylib',
        'libswiftSwiftPrivate.dylib',
        'libswiftSwiftPrivateLibcExtras.dylib',
        'libswiftSwiftPrivateThreadExtras.dylib',
        'libswiftSwiftReflectionTest.dylib',
        'libswiftGenericMetadataBuilder.dylib',
    }

    # The output from dyldinfo -dylibs is a line of header followed by one
    # install name per line, indented with spaces.
    dylib_regex = re.compile(
        r"(^|.*\s)(?P<path>@rpath/(?P<filename>libswift.*\.dylib))\s*$")

    # Build a command to invoke install_name_tool.
    command = ['install_name_tool']
    for line in dylibsOutput.splitlines():
        match = dylib_regex.match(line)
        if match and match.group('filename') not in allow_list:
            command.append('-change')
            command.append(match.group('path'))
            command.append('/usr/lib/swift/' + match.group('filename'))
            continue

    # Don't run the command if we didn't find any dylibs to change:
    # it's invalid to invoke install_name_tool without any operations.
    if len(command) == 1:
        return

    # The last argument is the filename to operate on.
    command.append(filename)

    subprocess.check_call(command)


def codesign(filename):
    # "-" is the signing identity for ad-hoc signing.
    command = ['/usr/bin/codesign', '--force', '--sign', '-',
               '--entitlements', get_task_allow_plist,
               filename]
    subprocess.check_call(command)


sys.exit(main(sys.argv[1:]))
