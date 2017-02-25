# swift_build_support/targets.py - Build target helpers -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import platform


class Platform(object):
    """
    Abstract representation of a platform Swift can run on.
    """

    def __init__(self, name, archs, sdk_name=None):
        """
        Create a platform with the given name and list of architectures.
        """
        self.name = name
        self.targets = [Target(self, arch) for arch in archs]
        # FIXME: Eliminate this argument; apparently the SDK names are
        # internally a private implementation detail of the build script, so we
        # should just make them the same as the platform name.
        self.sdk_name = name.upper() if sdk_name is None else sdk_name

        # Add a property for each arch.
        for target in self.targets:
            setattr(self, target.arch, target)

    @property
    def is_darwin(self):
        """Convenience function for checking if this is a Darwin platform."""
        return isinstance(self, DarwinPlatform)

    @property
    def supports_benchmark(self):
        # By default, we don't support benchmarks on most platforms.
        return False

    def contains(self, target_name):
        """
        Returns True if the given target name belongs to a one of this
        platform's targets.
        """
        for target in self.targets:
            if target.name == target_name:
                return True
        return False


class DarwinPlatform(Platform):
    def __init__(self, name, archs, sdk_name=None, is_simulator=False):
        self.is_simulator = is_simulator
        super(DarwinPlatform, self).__init__(name, archs, sdk_name)

    @property
    def is_embedded(self):
        """Check if this is a Darwin platform for embedded devices."""
        return self.name != "macosx"

    @property
    def supports_benchmark(self):
        # By default, on Darwin we support benchmarks on all non-simulator
        # platforms.
        return not self.is_simulator


class Target(object):
    """
    Abstract representation of a target Swift can run on.
    """

    def __init__(self, platform, arch):
        self.platform = platform
        self.arch = arch
        # Delegate to the platform, this is usually not arch specific.
        self.supports_benchmark = self.platform.supports_benchmark

    @property
    def name(self):
        return "{}-{}".format(self.platform.name, self.arch)


class StdlibDeploymentTarget(object):
    OSX = DarwinPlatform("macosx", archs=["x86_64"],
                         sdk_name="OSX")

    iOS = DarwinPlatform("iphoneos", archs=["armv7", "armv7s", "arm64"],
                         sdk_name="IOS")
    iOSSimulator = DarwinPlatform("iphonesimulator", archs=["i386", "x86_64"],
                                  sdk_name="IOS_SIMULATOR",
                                  is_simulator=True)

    # Never build/test benchmarks on iOS armv7s.
    iOS.armv7s.supports_benchmark = False

    AppleTV = DarwinPlatform("appletvos", archs=["arm64"],
                             sdk_name="TVOS")
    AppleTVSimulator = DarwinPlatform("appletvsimulator", archs=["x86_64"],
                                      sdk_name="TVOS_SIMULATOR",
                                      is_simulator=True)

    AppleWatch = DarwinPlatform("watchos", archs=["armv7k"],
                                sdk_name="WATCHOS")
    AppleWatchSimulator = DarwinPlatform("watchsimulator", archs=["i386"],
                                         sdk_name="WATCHOS_SIMULATOR",
                                         is_simulator=True)

    Linux = Platform("linux", archs=[
        "x86_64",
        "armv6",
        "armv7",
        "aarch64",
        "powerpc64",
        "powerpc64le",
        "s390x"])

    FreeBSD = Platform("freebsd", archs=["x86_64"])

    Cygwin = Platform("cygwin", archs=["x86_64"])

    Android = Platform("android", archs=["armv7"])

    Windows = Platform("windows", archs=["x86_64"])

    # The list of known platforms.
    known_platforms = [
        OSX,
        iOS, iOSSimulator,
        AppleTV, AppleTVSimulator,
        AppleWatch, AppleWatchSimulator,
        Linux,
        FreeBSD,
        Cygwin,
        Android,
        Windows]

    # Cache of targets by name.
    _targets_by_name = dict((target.name, target)
                            for platform in known_platforms
                            for target in platform.targets)

    @staticmethod
    def host_target():
        """
        Return the host target for the build machine, if it is one of
        the recognized targets. Otherwise, throw a NotImplementedError.
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
                return StdlibDeploymentTarget.Linux.powerpc64
            elif machine == 'ppc64le':
                return StdlibDeploymentTarget.Linux.powerpc64le
            elif machine == 's390x':
                return StdlibDeploymentTarget.Linux.s390x

        elif system == 'Darwin':
            if machine == 'x86_64':
                return StdlibDeploymentTarget.OSX.x86_64

        elif system == 'FreeBSD':
            if machine == 'amd64':
                return StdlibDeploymentTarget.FreeBSD.x86_64

        elif system == 'CYGWIN_NT-10.0':
            if machine == 'x86_64':
                return StdlibDeploymentTarget.Cygwin.x86_64

        elif system == 'Windows':
            if machine == "AMD64":
                return StdlibDeploymentTarget.Windows.x86_64

        raise NotImplementedError('System "%s" with architecture "%s" is not '
                                  'supported' % (system, machine))

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

    @classmethod
    def get_target_for_name(cls, name):
        return cls._targets_by_name.get(name)


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
