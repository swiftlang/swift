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


class Platform(object):
    """
    Abstract representation of a platform Swift can run on.
    """

    def __init__(self, name, archs):
        """
        Create a platform with the given name and list of architectures.
        """
        self.name = name
        self.targets = [Target(self, arch) for arch in archs]

        # Add a property for each arch.
        for target in self.targets:
            setattr(self, target.arch, target)


class Target(object):
    """
    Abstract representation of a target Swift can run on.
    """

    def __init__(self, platform, arch):
        self.platform = platform
        self.arch = arch

    @property
    def name(self):
        return "{}-{}".format(self.platform.name, self.arch)


class StdlibDeploymentTarget(object):
    OSX = Platform("macosx", archs=["x86_64"])

    iOS = Platform("iphoneos", archs=["armv7", "armv7s", "arm64"])
    iOSSimulator = Platform("iphonesimulator", archs=["i386", "x86_64"])

    AppleTV = Platform("appletvos", archs=["arm64"])
    AppleTVSimulator = Platform("appletvsimulator", archs=["x86_64"])

    AppleWatch = Platform("watchos", archs=["armv7k"])
    AppleWatchSimulator = Platform("watchsimulator", archs=["i386"])

    Linux = Platform("linux", archs=[
        "x86_64",
        "armv6",
        "armv7",
        "aarch64",
        "ppc64",
        "ppc64le",
        "s390x"])

    FreeBSD = Platform("freebsd", archs=["x86_64"])

    Cygwin = Platform("cygwin", archs=["x86_64"])

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

        # OS X build machines configure all Darwin platforms by default.
        # Put iOS native targets last so that we test them last
        # (it takes a long time).
        if host_target == StdlibDeploymentTarget.OSX.x86_64:
            return [host_target] + \
                StdlibDeploymentTarget.iOSSimulator.targets + \
                StdlibDeploymentTarget.AppleTVSimulator.targets + \
                StdlibDeploymentTarget.AppleWatchSimulator.targets + \
                StdlibDeploymentTarget.iOS.targets + \
                StdlibDeploymentTarget.AppleTV.targets + \
                StdlibDeploymentTarget.AppleWatch.targets
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
