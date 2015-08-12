#!/usr/bin/python

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
    print('\t-i <executable>\t\t\tThe swift-ide-test executable')
    print("\t--swift-ide-test=<executable>\tDefaults to 'swift-ide-test'")
    print('')
    print('\t-d <executable>\t\t\tThe tool to use to diff the results')
    print("\t--diff_tool=<executable>\tDefaults to 'opendiff'")
    print('')
    print('Examples:')
    print('\tpython omit-needless-words.py -m AppKit')

# Configuration information
sdk = 'macosx'
module = ''
source_filename = 'omit-needless-words.swift'
swift_ide_test = 'swift-ide-test'
diff_tool = 'opendiff'

# Parse command-line arguments.
try:
    opts, args = getopt.getopt(sys.argv[1:], 'hs:m:i:d:',
                               ['help', 'sdk=', 'module=','swift-ide-test=',
                                'diff_tool='])
except getopt.GetoptError:
    help()
    sys.exit(2)

for opt, arg in opts:
    if opt in ('-h', '--help'):
        help()
        sys.exit()

    if opt in ('-s', '--sdk'):
        sdk = arg
        continue

    if opt in ('-m', '--module'):
        module = arg
        continue

    if opt in ('-i', '--swift-ide-test'):
        swift_ide_test = arg
        continue

    if opt in ('-d', '--diff_tool'):
        swift_ide_test = arg
        continue

    help()
    sys.exit(2)

if module == '':
    help()
    sys.exit(2)
    
# Figure out the SDK root for the requested SDK
sdkroot = subprocess.check_output(['xcrun', '--show-sdk-path', '--sdk', sdk]).rstrip()
print('SDK Root = %s' % (sdkroot))

swift_ide_test_cmd = [swift_ide_test, '-print-module', '-source-filename', source_filename, '-sdk', sdkroot, '-module-print-skip-overlay', '-skip-unavailable', '-module-print-submodules', '-skip-parameter-names', '-module-to-print=%s' % (module)]
omit_needless_words_args = ['-enable-omit-needless-words']

# Determine the output files.
before_filename = '%s.before.txt' % (module)
after_filename = '%s.after.txt' % (module)

# Create a .swift file we can feed into swift-ide-test
subprocess.call(['touch', source_filename])

# Print the interface without omitting needless words
print('Writing %s...' % before_filename)
before_file = open(before_filename, 'w')
subprocess.call(swift_ide_test_cmd, stdout=before_file)
before_file.close()

# Print the interface omitting needless words
print('Writing %s...' % after_filename)
after_file = open(after_filename, 'w')
subprocess.call(swift_ide_test_cmd + omit_needless_words_args, stdout=after_file)
after_file.close()

# Remove the .swift file we fed into swift-ide-test
subprocess.call(['rm', '-f', source_filename])

# Diff them.
if diff_tool != '':
    subprocess.call([diff_tool, before_filename, after_filename])
