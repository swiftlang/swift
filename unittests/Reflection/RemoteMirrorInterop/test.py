#!/usr/bin/env python

# Exercise the SwiftRemoteMirrorLegacyInterop API. This works with
# multiple versions of Swift. It builds Swift code using all versions,
# and exercises the Interop API using various combinations of those
# versions' Remote Mirror libraries.
#
# Invoke by passing the various Swift build directories as parameters.

import itertools
import os
import subprocess
import sys

args = sys.argv[1:]
if len(args) == 0:
  print >> sys.stderr, "Usage:", sys.argv[0], "swift-build-dirs..."
  print >> sys.stderr, "Note: pass paths to the swift-macosx-x86_64 directories."
  sys.exit(1)

swiftcs = [os.path.join(arg, 'bin', 'swiftc') for arg in args]
mirrorlibs = [os.path.join(arg, 'lib', 'swift', 'macosx', 'libswiftRemoteMirror.dylib')
              for arg in args]

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
for i, swiftc in enumerate(swiftcs):
  subprocess.check_call(['xcrun', swiftc, '-emit-library', 'test.swift',
                         '-o', os.path.join('/tmp', 'libtest' + str(i) + '.dylib')])

# Run the test harness with all combinations of the remote mirror libraries.
for i in range(len(swiftcs) + 1):
  for localMirrorlibs in itertools.combinations(mirrorlibs, i):
    for i, arg in enumerate(args):
      print 'Testing', arg, 'with mirror libs:'
      for l in localMirrorlibs:
        print '\t', l
      callArgs = (['/tmp/test', os.path.join('/tmp', 'libtest' + str(i) + '.dylib')]
                  + list(localMirrorlibs))
      print ' '.join(callArgs)
      subprocess.call(callArgs)
      print 'DONE'
      print ''
    print localMirrorlibs
