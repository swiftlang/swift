#!/usr/bin/env python

# This tool helps assess the impact of automatically applying
# heuristics that omit 'needless' words from APIs imported from Clang
# into Swift.

import getopt
import sys
import subprocess

# Print help
def help():
    print('omit-needless-words.py [options] -m <modulename>')
    print('')
    print('Summary:')
    print("\tDetermines the effects of omitting 'needless' words from imported APIs")
    print('')
    print('Options:')
    print('\t-s <sdkname>\t\t\tThe SDK to use (e.g., macosx)')
    print("\t--sdk=<sdkname>'\t\tDefaults to 'macosx'")
    print('')
    print('\t-t <triple>\t\t\tThe target triple to use (e.g., x86_64-apple-macosx10.10)')
    print("\t--target=<triple>")
    print('')
    print('\t-i <executable>\t\t\tThe swift-ide-test executable')
    print("\t--swift-ide-test=<executable>\tDefaults to 'swift-ide-test'")
    print('')
    print('\t-d <executable>\t\t\tThe tool to use to diff the results')
    print("\t--diff_tool=<executable>\tDefaults to 'opendiff'")
    print('')
    print('\t-b\t\t\t\tOnly emit the "before" result')
    print('\t--only-before')
    print('')
    print('\t--before-file=<filename>\tEmit "before" results to the given file')
    print('\t\t\t\t\tDefaults to <modulename>.before.txt')
    print('')
    print('\t-a\t\t\t\tOnly emit the "after" result')
    print('\t--only-after')
    print('')
    print('\t--after-file=<filename>\t\tEmit "after" results to the given file')
    print('\t\t\t\t\tDefaults to <modulename>.after.txt')
    print('')
    print('\t-q\t\t\t\tSuppress printing of status messages')
    print('')
    print('Examples:')
    print('\tpython omit-needless-words.py -m AppKit')

# Configuration information
sdk = 'macosx'
target = ''
module = ''
source_filename = 'omit-needless-words.swift'
swift_ide_test = 'swift-ide-test'
diff_tool = 'opendiff'
only_before=0
only_after=0

# Parse command-line arguments.
try:
    opts, args = getopt.getopt(sys.argv[1:], 'hs:t:m:i:d:baq',
                               ['help', 'sdk=', 'target=', 'module=',
                                'swift-ide-test=','diff_tool=','only-before',
                                'only-after', 'before-file=', 'after-file='])
except getopt.GetoptError:
    help()
    sys.exit(2)

before_filename = ""
after_filename = ""
verbose = 1
for opt, arg in opts:
    if opt in ('-h', '--help'):
        help()
        sys.exit()

    if opt in ('-s', '--sdk'):
        sdk = arg
        continue

    if opt in ('-t', '--target'):
        target = arg
        continue

    if opt in ('-m', '--module'):
        module = arg
        continue

    if opt in ('-i', '--swift-ide-test'):
        swift_ide_test = arg
        continue

    if opt in ('-d', '--diff_tool'):
        diff_tool = arg
        continue

    if opt in ('-b', '--only-before'):
        only_before=1
        continue

    if opt in ('-a', '--only-after'):
        only_after=1
        continue

    if opt == '--before-file':
        before_filename = arg
        continue

    if opt == '--after-file':
        after_filename = arg
        continue

    if opt == '-q':
        verbose = 0
        continue
    
    help()
    sys.exit(2)

if module == '':
    help()
    sys.exit(2)

if target == '':
    if sdk == 'macosx':
        target = 'x86_64-apple-macosx10.11'
    if sdk == 'iphoneos':
        target = 'arm64-apple-ios9.0'
    if sdk == 'iphonesimulator':
        target = 'x86_64-apple-ios9.0'
    if sdk == 'watchos':
        target = 'armv7k-apple-watchos2.0'
    if sdk == 'watchos.simulator':
        target = 'i386-apple-watchos2.0'
    if sdk == 'appletvos':
        target = 'arm64-apple-tvos9'
    if sdk == 'appletvos.simulator':
        target = 'x86_64-apple-tvos9'

# Figure out the SDK root for the requested SDK
sdkroot = subprocess.check_output(['xcrun', '--show-sdk-path', '--sdk', sdk]).rstrip()
if verbose != 0:
    print('SDK Root = %s' % (sdkroot))

swift_ide_test_cmd = [swift_ide_test, '-print-module', '-source-filename', source_filename, '-sdk', sdkroot, '-target', target, '-module-print-skip-overlay', '-skip-unavailable', '-module-print-submodules', '-skip-imports', '-module-to-print=%s' % (module)]
omit_needless_words_args = ['-enable-omit-needless-words', '-enable-infer-default-arguments']

# Determine the output files.
if before_filename == "":
  before_filename = '%s.before.txt' % (module)
if after_filename == "":
  after_filename = '%s.after.txt' % (module)

# Create a .swift file we can feed into swift-ide-test
subprocess.call(['touch', source_filename])

if only_after == 0:
  # Print the interface without omitting needless words
  if verbose != 0:
    print('Writing %s...' % before_filename)
  before_file = open(before_filename, 'w')
  subprocess.call(swift_ide_test_cmd, stdout=before_file)
  before_file.close()

if only_before == 0:
  # Print the interface omitting needless words
  if verbose != 0:
    print('Writing %s...' % after_filename)
  after_file = open(after_filename, 'w')
  subprocess.call(swift_ide_test_cmd + omit_needless_words_args, stdout=after_file)
  after_file.close()

# Remove the .swift file we fed into swift-ide-test
subprocess.call(['rm', '-f', source_filename])

# Diff them.
if diff_tool != '':
    subprocess.call([diff_tool, before_filename, after_filename])
