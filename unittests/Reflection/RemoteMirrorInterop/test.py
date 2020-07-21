#!/usr/bin/env python

# Exercise the SwiftRemoteMirrorLegacyInterop API. This works with
# multiple versions of Swift. It builds Swift code using all versions,
# and exercises the Interop API using various combinations of those
# versions' Remote Mirror libraries.
#
# Invoke by passing the various Swift build directories as parameters.

from __future__ import print_function

import itertools
import os
import subprocess
import sys

args = sys.argv[1:]
if len(args) == 0:
    print("Usage:", sys.argv[0], "swift-build-dirs...", file=sys.stderr)
    print(("Note: pass paths to the swift-macosx-x86_64"
           " directories, or /usr to test the OS."), file=sys.stderr)
    sys.exit(1)

absoluteArgs = [os.path.abspath(arg) for arg in args]
swiftcs = [os.path.join(arg, 'bin', 'swiftc') for arg in absoluteArgs]


def libPath(path):
    libPath = os.path.join(path, 'lib', 'swift', 'macosx')
    if not os.path.isdir(libPath):
        libPath = os.path.join(path, 'lib', 'swift')
    return libPath


swiftlibs = [libPath(arg) for arg in absoluteArgs]
mirrorlibs = [os.path.join(lib, 'libswiftRemoteMirror.dylib')
              for lib in swiftlibs]

os.chdir(os.path.dirname(sys.argv[0]))

# Build the remote mirror test harness program.
subprocess.check_call(['clang',
                       '-framework', 'Foundation',
                       '-I', '../../../include/swift/SwiftRemoteMirror',
                       '-I', '../../../include/',
                       '-o', '/tmp/test',
                       '-Wall', '-Wextra',
                       '-g', 'test.m'])

# Build a test library with each Swift compiler passed in.
for i, (swiftc, swiftlib) in enumerate(zip(swiftcs, swiftlibs)):
    subprocess.check_call(
        ['xcrun', swiftc, '-emit-library', 'test.swift',
         '-o', os.path.join('/tmp', 'libtest' + str(i) + '.dylib'),
         '-Xlinker', '-rpath', '-Xlinker', swiftlib])

# Run the test harness with all combinations of the remote mirror libraries.
for i in range(len(swiftcs) + 1):
    for localMirrorlibs in itertools.combinations(mirrorlibs, i):
        for i, arg in enumerate(absoluteArgs):
            print('Testing', arg, 'with mirror libs:')
            for lib in localMirrorlibs:
                print('\t', lib)
            callArgs = ['/tmp/test']
            dylibPath = os.path.join('/tmp', 'libtest' + str(i) + '.dylib')
            callArgs.append(dylibPath)
            callArgs += list(localMirrorlibs)
            print(' '.join(callArgs))
            subprocess.call(callArgs)
            print('DONE')
            print('')
        print(localMirrorlibs)
