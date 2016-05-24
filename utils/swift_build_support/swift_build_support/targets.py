# swift_build_support/targets.py - Build target helpers -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import platform


def host_target():
    """
    Return the build target for the current host machine, if it is one of the
    recognized targets. Otherwise, return None.
    """
    system = platform.system()
    machine = platform.machine()

    if system == 'Linux':
        if machine == 'x86_64':
            return 'linux-x86_64'
        elif machine.startswith('armv7'):
            # linux-armv7* is canonicalized to 'linux-armv7'
            return 'linux-armv7'
        elif machine.startswith('armv6'):
            # linux-armv6* is canonicalized to 'linux-armv6'
            return 'linux-armv6'
        elif machine == 'aarch64':
            return 'linux-aarch64'
        elif machine == 'ppc64':
            return 'linux-powerpc64'
        elif machine == 'ppc64le':
            return 'linux-powerpc64le'

    elif system == 'Darwin':
        if machine == 'x86_64':
            return 'macosx-x86_64'

    elif system == 'FreeBSD':
        if machine == 'amd64':
            return 'freebsd-x86_64'

    elif system == 'CYGWIN_NT-10.0':
        if machine == 'x86_64':
            return 'cygwin-x86_64'

    return None


def stdlib_deployment_targets():
    """
    Return deployment targets for the Swift stdlib, based on the host machine.
    If the host machine is not one of the recognized ones, return None.
    """
    system = platform.system()
    machine = platform.machine()

    if system == 'Linux':
        if machine == 'x86_64':
            return [
                'linux-x86_64',
                'android-armv7',
            ]
        elif machine.startswith('armv6'):
            # linux-armv6* is canonicalized to 'linux-armv6'
            return ['linux-armv6']
        elif machine.startswith('armv7'):
            # linux-armv7* is canonicalized to 'linux-armv7'
            return ['linux-armv7']
        elif machine == 'aarch64':
            return ['linux-aarch64']
        elif machine == 'ppc64':
            return ['linux-powerpc64']
        elif machine == 'ppc64le':
            return ['linux-powerpc64le']
    elif system == 'Darwin':
        if machine == 'x86_64':
            return [
                'macosx-x86_64',
                'iphonesimulator-i386',
                'iphonesimulator-x86_64',
                'appletvsimulator-x86_64',
                'watchsimulator-i386',
                # Put iOS native targets last so that we test them last
                # (it takes a long time).
                'iphoneos-arm64',
                'iphoneos-armv7',
                'appletvos-arm64',
                'watchos-armv7k',
            ]
    elif system == 'FreeBSD':
        if machine == 'amd64':
            return ['freebsd-x86_64']
    elif system == 'CYGWIN_NT-10.0':
        if machine == 'x86_64':
            return ['cygwin-x86_64']

    return None


def install_prefix():
    """
    Returns the default path at which built Swift products (like bin, lib,
    and include) will be installed, based on the host machine's operating
    system.
    """
    if platform.system() == 'Darwin':
        return '/Applications/Xcode.app/Contents/Developer/Toolchains/' + \
            'XcodeDefault.xctoolchain/usr'
    else:
        return '/usr'


def darwin_toolchain_prefix(darwin_install_prefix):
    """
    Given the install prefix for a Darwin system, and assuming that that path
    is to a .xctoolchain directory, return the path to the .xctoolchain
    directory.
    """
    return os.path.split(darwin_install_prefix)[0]
