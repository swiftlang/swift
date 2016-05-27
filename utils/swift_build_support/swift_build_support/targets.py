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


class StdlibDeploymentTarget(object):

    class OSX(object):
        x86_64 = 'macosx-x86_64'
        allArchs = [x86_64]

    class iOS(object):
        armv7 = 'iphoneos-armv7'
        armv7s = 'iphoneos-armv7s'
        arm64 = 'iphoneos-arm64'
        allArchs = [armv7, armv7s, arm64]

    class iOSSimulator(object):
        i386 = 'iphonesimulator-i386'
        x86_64 = 'iphonesimulator-x86_64'
        allArchs = [i386, x86_64]

    class AppleTV(object):
        arm64 = 'appletvos-arm64'
        allArchs = [arm64]

    class AppleTVSimulator(object):
        x86_64 = 'appletvsimulator-x86_64'
        allArchs = [x86_64]

    class AppleWatch(object):
        armv7k = 'watchos-armv7k'
        allArchs = [armv7k]

    class AppleWatchSimulator(object):
        i386 = 'watchsimulator-i386'
        allArchs = [i386]

    class Linux(object):
        x86_64 = 'linux-x86_64'
        armv6 = 'linux-armv6'
        armv7 = 'linux-armv7'
        aarch64 = 'linux-aarch64'
        ppc64 = 'linux-ppc64'
        ppc64le = 'linux-ppc64le'
        s390x = 'linux-s390x'
        allArchs = [x86_64, armv6, armv7, aarch64, ppc64, ppc64le, s390x]

    class FreeBSD(object):
        amd64 = 'freebsd-x86_64'
        allArchs = [amd64]

    class Cygwin(object):
        x86_64 = 'cygwin-x86_64'
        allArchs = [x86_64]

    class Android(object):
        armv7 = 'android-armv7'
        allArchs = [armv7]

    @staticmethod
    def host_target():
        """
        Return the host target for the build machine, if it is one of
        the recognized targets. Otherwise, return None.
        """
        system = platform.system()
        machine = platform.machine()

        if system == 'Linux':
            if machine == 'x86_64':
                return StdlibDeploymentTarget.Linux.x86_64
            elif machine.startswith('armv7'):
                # linux-armv7* is canonicalized to 'linux-armv7'
                return StdlibDeploymentTarget.Linux.armv7
            elif machine.startswith('armv6'):
                # linux-armv6* is canonicalized to 'linux-armv6'
                return StdlibDeploymentTarget.Linux.armv6
            elif machine == 'aarch64':
                return StdlibDeploymentTarget.Linux.aarch64
            elif machine == 'ppc64':
                return StdlibDeploymentTarget.Linux.ppc64
            elif machine == 'ppc64le':
                return StdlibDeploymentTarget.Linux.ppc64le
            elif machine == 's390x':
                return StdlibDeploymentTarget.Linux.s390x

        elif system == 'Darwin':
            if machine == 'x86_64':
                return StdlibDeploymentTarget.OSX.x86_64

        elif system == 'FreeBSD':
            if machine == 'amd64':
                return StdlibDeploymentTarget.FreeBSD.amd64

        elif system == 'CYGWIN_NT-10.0':
            if machine == 'x86_64':
                return StdlibDeploymentTarget.Cygwin.x86_64

        return None

    @staticmethod
    def default_stdlib_deployment_targets():
        """
        Return targets for the Swift stdlib, based on the build machine.
        If the build machine is not one of the recognized ones, return None.
        """

        host_target = StdlibDeploymentTarget.host_target()
        if host_target is None:
            return None

        # OSX build machines configure all Darwin platforms by default.
        # Put iOS native targets last so that we test them last
        # (it takes a long time).
        if host_target == StdlibDeploymentTarget.OSX.x86_64:
            return [host_target] + \
                StdlibDeploymentTarget.iOSSimulator.allArchs + \
                StdlibDeploymentTarget.AppleTVSimulator.allArchs + \
                StdlibDeploymentTarget.AppleWatchSimulator.allArchs + \
                StdlibDeploymentTarget.iOS.allArchs + \
                StdlibDeploymentTarget.AppleTV.allArchs + \
                StdlibDeploymentTarget.AppleWatch.allArchs
        else:
            # All other machines only configure their host stdlib by default.
            return [host_target]


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
