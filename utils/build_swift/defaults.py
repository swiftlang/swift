# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Default option value definitions.
"""


from .argparse import CompilerVersion


__all__ = [
    # Command line configuarable
    'BUILD_VARIANT',
    'CMAKE_GENERATOR',
    'COMPILER_VENDOR',
    'SWIFT_USER_VISIBLE_VERSION',
    'CLANG_USER_VISIBLE_VERSION',
    'SWIFT_ANALYZE_CODE_COVERAGE',
    'DARWIN_XCRUN_TOOLCHAIN',
    'DARWIN_DEPLOYMENT_VERSION_OSX',
    'DARWIN_DEPLOYMENT_VERSION_IOS',
    'DARWIN_DEPLOYMENT_VERSION_TVOS',
    'DARWIN_DEPLOYMENT_VERSION_WATCHOS',
    'UNIX_INSTALL_PREFIX',
    'DARWIN_INSTALL_PREFIX',

    # Constants
]

# Options that can be "configured" by command line options

BUILD_VARIANT = 'Debug'
CMAKE_GENERATOR = 'Ninja'

COMPILER_VENDOR = 'none'
SWIFT_USER_VISIBLE_VERSION = CompilerVersion('4.2')
CLANG_USER_VISIBLE_VERSION = CompilerVersion('7.0.0')
SWIFT_ANALYZE_CODE_COVERAGE = 'false'

DARWIN_XCRUN_TOOLCHAIN = 'default'
DARWIN_DEPLOYMENT_VERSION_OSX = '10.9'
DARWIN_DEPLOYMENT_VERSION_IOS = '7.0'
DARWIN_DEPLOYMENT_VERSION_TVOS = '9.0'
DARWIN_DEPLOYMENT_VERSION_WATCHOS = '2.0'

UNIX_INSTALL_PREFIX = '/usr'
DARWIN_INSTALL_PREFIX = ('/Applications/Xcode.app/Contents/Developer/'
                         'Toolchains/XcodeDefault.xctoolchain/usr')

# Options that can only be "configured" by editing this file.
#
# These options are not exposed as command line options on purpose.  If you
# need to change any of these, you should do so on trunk or in a branch.
