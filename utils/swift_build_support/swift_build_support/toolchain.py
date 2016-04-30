# swift_build_support/toolchain.py ------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
"""
Represent toolchain - the versioned executables.
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import platform
import subprocess

from . import cache_util
from . import xcrun
from .which import which

__all__ = [
    'host_toolchain',
]


class Toolchain(object):
    """Represents native host toolchain
    """

    def find_tool(self, tool, versioned=False):
        raise NotImplementedError('Subclasses must implement this method')

    # NOTE: We are declaring attribute for each tools because we need
    #       them be assignable. For example, `toolchain.cc` can be
    #       overriden by user supplied argument: --host-cc=/path/to/clang
    #       And we don't want to override __getattr__ because that would need
    #       heuristic command name inference. e.g. llvm_cov -> llvm-cov

    @cache_util.reify
    def cc(self):
        '''Return Clang host tool path if found. `None` otherwise.
        '''
        return self.find_tool('clang')

    @cache_util.reify
    def cxx(self):
        '''Return Clang host tool path if found. `None` otherwise.
        '''
        return self.find_tool('clang++')

    @cache_util.reify
    def cmake(self):
        '''Return CMake host tool path if found. `None` otherwise.
        '''
        return self.find_tool('cmake')

    @cache_util.reify
    def ninja(self):
        '''Return Ninja host tool path if found. `None` otherwise.
        '''
        return self.find_tool('ninja', 'ninja-build')

    @cache_util.reify
    def distcc(self):
        '''Return distcc host tool path if found. `None` otherwise.
        '''
        return self.find_tool('distcc')

    @cache_util.reify
    def distcc_pump(self):
        '''Return distcc-pump host tool path if found. `None` otherwise.
        '''
        return self.find_tool('distcc-pump', 'pump')

    @cache_util.reify
    def llvm_profdata(self):
        '''Return llvm-profdata host tool path if found. `None` otherwise.
        '''
        return self.find_tool('llvm-profdata')

    @cache_util.reify
    def llvm_cov(self):
        '''Return llvm-cov host tool path if found. `None` otherwise.
        '''
        return self.find_tool('llvm-cov')


class Darwin(Toolchain):
    def __init__(self, sdk, toolchain):
        super(Darwin, self).__init__()
        self.xcrun_sdk = sdk
        self.xcrun_toolchain = toolchain

    def find_tool(self, *names):
        for name in names:
            # NOTE: xcrun searches from developer tools directory *and* from
            #       PATH. Relatively slow, but we don't need `which `for
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
        # See: https://github.com/apple/swift/pull/169
        # Building Swift from source requires a recent version of the Clang
        # compiler with C++14 support.
        if self._release_date and self._release_date >= 1100000:
            suffixes = ['']
        else:
            suffixes = ['38', '37', '36', '35']
        super(FreeBSD, self).__init__(suffixes)

    @cache_util.reify
    def _release_date():
        """Return the release date for FreeBSD operating system on this host.
        If the release date cannot be ascertained, return None.
        """
        try:
            # For details on `sysctl`, see:
            # http://www.freebsd.org/cgi/man.cgi?sysctl(8)
            out = subprocess.check_output(['sysctl', '-n', 'kern.osreldate'])
            return int(out)
        except subprocess.CalledProcessError:
            return None


class Cygwin(Linux):
    # FIXME: Currently, Cygwin is considered as the same as Linux.
    #        I'm not sure it's correct or not.
    #        Please some Cygwin user could confirm this or fix it.
    pass


def host_toolchain(**kwargs):
    sys = platform.system()
    if sys == 'Darwin':
        return MacOSX(kwargs.pop('xcrun_toolchain', 'default'))
    elif sys == 'Linux':
        return Linux()
    elif sys == 'FreeBSD':
        return FreeBSD()
    elif sys.startswith('CYGWIN'):
        return Cygwin()
    else:
        raise NotImplementedError(
            'toolchain() is not supported in this platform')
