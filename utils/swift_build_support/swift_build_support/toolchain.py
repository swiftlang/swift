# swift_build_support/toolchain.py ------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
"""
Represent toolchain - the versioned executables.
"""
# ----------------------------------------------------------------------------

import os
import platform

from build_swift.build_swift import cache_utils
from build_swift.build_swift.shell import which
from build_swift.build_swift.wrappers import xcrun

from . import shell


__all__ = [
    'host_toolchain',
]


class Toolchain(object):
    """Represents native host toolchain
    """

    def find_tool(self, *names):
        raise NotImplementedError('Subclasses must implement this method')


# Declare properties for each tools.
# These properties are loaded lazily and assignable.
def _register(name, *tool):
    def _getter(self):
        return self.find_tool(*tool)
    _getter.__name__ = name
    setattr(Toolchain, name, cache_utils.reify(_getter))


if platform.system() == 'Windows':
    _register("cc", "clang-cl")
    _register("cxx", "clang-cl")
else:
    _register("cc", "clang")
    _register("cxx", "clang++")

_register("ninja", "ninja", "ninja-build")
_register("cmake", "cmake")
_register("distcc", "distcc")
_register("distcc_pump", "distcc-pump", "pump")
_register("llvm_profdata", "llvm-profdata")
_register("llvm_cov", "llvm-cov")
_register("lipo", "lipo")
_register("libtool", "libtool")
_register("ld", "ld")
if 'ANDROID_DATA' in os.environ:
    _register("ranlib", "llvm-ranlib")
    _register("ar", "llvm-ar")
else:
    _register("ranlib", "ranlib")
    _register("ar", "ar")
_register("llvm_ar", "llvm-ar")
_register("llvm_nm", "llvm-nm")
_register("llvm_ranlib", "llvm-ranlib")
_register("sccache", "sccache")
_register("swiftc", "swiftc")
_register("swift_build", "swift-build")


class Darwin(Toolchain):
    def __init__(self, sdk, toolchain):
        super(Darwin, self).__init__()
        self.xcrun_sdk = sdk
        self.xcrun_toolchain = toolchain

    def find_tool(self, *names):
        for name in names:
            # NOTE: xcrun searches from developer tools directory *and* from
            #       PATH. Relatively slow, but we don't need `which` for
            #       Darwin.
            found = xcrun.find(name,
                               sdk=self.xcrun_sdk,
                               toolchain=self.xcrun_toolchain)
            if found is not None:
                return found
        return None


class GenericUnix(Toolchain):
    def __init__(self, suffixes):
        super(GenericUnix, self).__init__()

        # On these platforms, search 'clang', 'clang++' unconditionally.
        # To determine the llvm_suffix.
        ret = self.find_clang(['clang', 'clang++'], suffixes)
        if ret is None:
            self.cc = None
            self.cxx = None
            # We don't have clang, then we don't have any llvm tools.
            self.llvm_suffixes = []
        else:
            found, suffix = ret
            self.cc, self.cxx = found

            if suffix == '':
                # Some platform may have `clang`, `clang++`, `llvm-cov-3.6`
                # but not `llvm-cov`. In that case, we assume `clang` is
                # corresponding to the best version of llvm tools found.
                self.llvm_suffixes = suffixes
            else:
                # Otherwise, we must have llvm tools with the same suffix as
                # `clang` or `clang++`
                self.llvm_suffixes = [suffix]

    def find_clang(self, tools, suffixes):
        for suffix in suffixes:
            ret = [which(t + suffix) for t in tools]
            if all(t is not None for t in ret):
                return (ret, suffix)
        return None

    def find_llvm_tool(self, tool):
        for suffix in self.llvm_suffixes:
            found = which(tool + suffix)
            if found is not None:
                # If we found the tool with the suffix, lock suffixes to it.
                self.llvm_suffix = [suffix]
                return found
        return None

    def find_tool(self, *names):
        for name in names:
            if name.startswith('llvm-'):
                found = self.find_llvm_tool(name)
            else:
                found = which(name)
            if found is not None:
                return found
        return None


class MacOSX(Darwin):
    def __init__(self, toolchain='default'):
        super(MacOSX, self).__init__(sdk='macosx', toolchain=toolchain)


class Linux(GenericUnix):
    def __init__(self):
        super(Linux, self).__init__(['', '-3.8', '-3.7', '-3.6', '-3.5'])


class FreeBSD(GenericUnix):
    def __init__(self):
        # For testing toolchain initializer on non-FreeBSD systems
        sys = platform.system()
        if sys != 'FreeBSD':
            suffixes = ['']
        # See: https://github.com/apple/swift/pull/169
        # Building Swift from source requires a recent version of the Clang
        # compiler with C++14 support.
        elif self._release_date and self._release_date >= 1100000:
            suffixes = ['']
        else:
            suffixes = ['38', '37', '36', '35']
        super(FreeBSD, self).__init__(suffixes)

    @cache_utils.reify
    def _release_date(self):
        """Return the release date for FreeBSD operating system on this host.
        If the release date cannot be ascertained, return None.
        """
        # For details on `sysctl`, see:
        # http://www.freebsd.org/cgi/man.cgi?sysctl(8)
        out = shell.capture(['sysctl', '-n', 'kern.osreldate'],
                            dry_run=False, echo=False, optional=True)
        if out is None:
            return None
        return int(out)


class OpenBSD(GenericUnix):
    def __init__(self):
        super(OpenBSD, self).__init__([''])


class Cygwin(Linux):
    # Currently, Cygwin is considered as the same as Linux.
    pass


class Windows(Toolchain):
    def find_tool(self, *names):
        for name in names:
            found = which(name)
            if found is not None:
                return found
        return None


class Haiku(GenericUnix):
    def __init__(self):
        super(Haiku, self)


def host_toolchain(**kwargs):
    sys = platform.system()
    if sys == 'Darwin':
        return MacOSX(kwargs.pop('xcrun_toolchain', 'default'))
    elif sys == 'Linux':
        return Linux()
    elif sys == 'FreeBSD':
        return FreeBSD()
    elif sys == 'OpenBSD':
        return OpenBSD()
    elif sys.startswith('CYGWIN'):
        return Cygwin()
    elif sys == 'Windows':
        return Windows()
    elif sys == 'Haiku':
        return Haiku()
    else:
        raise NotImplementedError('The platform "%s" does not have a defined '
                                  'toolchain.' % sys)
