# swift_build_support/cmake.py - Detect host machine's CMake -*- python -*-
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
#
# Find the path to a CMake executable on the host machine.
#
# ----------------------------------------------------------------------------

from numbers import Number
import platform
import subprocess

from . import xcrun
from .which import which


def host_cmake(xcrun_toolchain):
    """
    Return the path to `cmake`, using tools provided by the host platform.
    If `cmake` cannot be found on OS X, return None.
    If `cmake` cannot be found on Linux, return a probable path.
    """
    if platform.system() == 'Darwin':
        return xcrun.find(xcrun_toolchain, 'cmake')
    else:
        cmake = which('cmake')
        if cmake:
            return cmake
        else:
            return '/usr/local/bin/cmake'


class CMakeOptions(object):
    """List like object used to define cmake options
    """

    def __init__(self):
        self._options = []

    def define(self, var, value):
        """Utility to define cmake options in this object.

        opts.define("FOO", "BAR")       # -> -DFOO=BAR
        opts.define("FLAG:BOOL", True)  # -> -FLAG:BOOL=TRUE
        """
        if var.endswith(':BOOL'):
            value = self.true_false(value)
        if value is None:
            value = ""
        elif not isinstance(value, (str, Number)):
            raise ValueError('define: invalid value: %s' % value)
        self._options.append('-D%s=%s' % (var, value))

    @staticmethod
    def true_false(value):
        if hasattr(value, 'lower'):
            value = value.lower()
        if value in [True, 1, 'true', 'yes', '1']:
            return 'TRUE'
        if value in [False, 0, 'false', 'no', '0']:
            return 'FALSE'
        raise ValueError("true_false: invalid value: %s" % value)

    def __len__(self):
        return self._options.__len__()

    def __iter__(self):
        return self._options.__iter__()

    def __add__(self, other):
        ret = CMakeOptions()
        ret._options += self._options
        ret._options += list(other)
        return ret

    def __iadd__(self, other):
        self._options += list(other)
        return self


class CMake(object):

    def __init__(self, args, host_cc, host_cxx, host_distcc):
        self.args = args
        self.host_cc = host_cc
        self.host_cxx = host_cxx
        self.host_distcc = host_distcc

    def common_options(self):
        """Return options used for all products, including LLVM/Clang
        """
        args = self.args
        options = CMakeOptions()
        define = options.define

        options += ['-G', args.cmake_generator]

        sanitizers = []
        if args.enable_asan:
            sanitizers.append('Address')
        if args.enable_ubsan:
            sanitizers.append('Undefined')
        if sanitizers:
            define("LLVM_USE_SANITIZER", ";".join(sanitizers))

        if args.export_compile_commands:
            define("CMAKE_EXPORT_COMPILE_COMMANDS", "ON")

        if args.distcc:
            define("CMAKE_C_COMPILER:PATH", self.host_distcc)
            define("CMAKE_C_COMPILER_ARG1", self.host_cc)
            define("CMAKE_CXX_COMPILER:PATH", self.host_distcc)
            define("CMAKE_CXX_COMPILER_ARG1", self.host_cxx)
        else:
            define("CMAKE_C_COMPILER:PATH", self.host_cc)
            define("CMAKE_CXX_COMPILER:PATH", self.host_cxx)

        if args.cmake_generator == 'Xcode':
            define("CMAKE_CONFIGURATION_TYPES",
                   "Debug;Release;MinSizeRel;RelWithDebInfo")

        if args.clang_compiler_version:
            major, minor, patch = args.clang_compiler_version
            define("LLVM_VERSION_MAJOR:STRING", major)
            define("LLVM_VERSION_MINOR:STRING", minor)
            define("LLVM_VERSION_PATCH:STRING", patch)

        return options

    def build_args(self):
        """Return arguments to the build tool used for all products
        """
        args = self.args
        jobs = args.build_jobs
        if args.distcc:
            jobs = str(
                subprocess.check_output([self.host_distcc, '-j'])).rstrip()

        build_args = list(args.build_args)

        if args.cmake_generator == 'Ninja':
            build_args += ['-j%s' % jobs]
            if args.verbose_build:
                build_args += ['-v']

        elif args.cmake_generator == 'Unix Makefiles':
            build_args += ['-j%s' % jobs]
            if args.verbose_build:
                build_args += ['VERBOSE=1']

        elif args.cmake_generator == 'Xcode':
            build_args += ['-parallelizeTargets',
                           '-jobs', str(jobs)]

        return build_args
