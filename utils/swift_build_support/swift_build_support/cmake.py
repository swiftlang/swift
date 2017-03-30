# swift_build_support/cmake.py - Detect host machine's CMake -*- python -*-
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
#
# Find the path to a CMake executable on the host machine.
#
# ----------------------------------------------------------------------------

from __future__ import absolute_import

from numbers import Number

from . import shell


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

    def __init__(self, args, toolchain):
        self.args = args
        self.toolchain = toolchain

    def common_options(self):
        """Return options used for all products, including LLVM/Clang
        """
        args = self.args
        toolchain = self.toolchain
        options = CMakeOptions()
        define = options.define

        options += ['-G', args.cmake_generator]

        sanitizers = []
        if args.enable_asan:
            sanitizers.append('Address')
        if args.enable_ubsan:
            sanitizers.append('Undefined')
        if args.enable_tsan:
            sanitizers.append('Thread')
        if sanitizers:
            define("LLVM_USE_SANITIZER", ";".join(sanitizers))

        if args.export_compile_commands:
            define("CMAKE_EXPORT_COMPILE_COMMANDS", "ON")

        if args.distcc:
            define("CMAKE_C_COMPILER:PATH", toolchain.distcc)
            define("CMAKE_C_COMPILER_ARG1", toolchain.cc)
            define("CMAKE_CXX_COMPILER:PATH", toolchain.distcc)
            define("CMAKE_CXX_COMPILER_ARG1", toolchain.cxx)
        else:
            define("CMAKE_C_COMPILER:PATH", toolchain.cc)
            define("CMAKE_CXX_COMPILER:PATH", toolchain.cxx)

        if args.cmake_generator == 'Xcode':
            define("CMAKE_CONFIGURATION_TYPES",
                   "Debug;Release;MinSizeRel;RelWithDebInfo")

        if args.clang_user_visible_version:
            major, minor, patch, _ = args.clang_user_visible_version.components
            define("LLVM_VERSION_MAJOR:STRING", major)
            define("LLVM_VERSION_MINOR:STRING", minor)
            define("LLVM_VERSION_PATCH:STRING", patch)
            define("CLANG_VERSION_MAJOR:STRING", major)
            define("CLANG_VERSION_MINOR:STRING", minor)
            define("CLANG_VERSION_PATCH:STRING", patch)


        if args.build_ninja and args.cmake_generator == 'Ninja':
            define('CMAKE_MAKE_PROGRAM', toolchain.ninja)
        elif args.cmake_generator == 'Ninja' and toolchain.ninja is not None:
            define('CMAKE_MAKE_PROGRAM', toolchain.ninja)

        return options

    def build_args(self):
        """Return arguments to the build tool used for all products
        """
        args = self.args
        toolchain = self.toolchain
        jobs = args.build_jobs
        if args.distcc:
            jobs = shell.capture([toolchain.distcc, '-j'],
                                 dry_run=False, echo=False).rstrip()

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
