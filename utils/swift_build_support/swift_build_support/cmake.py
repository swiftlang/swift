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

import itertools
import os


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

    def __contains__(self, item):
        return self._options.__contains__(item)

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
        if args.enable_lsan:
            sanitizers.append('Leaks')
        if sanitizers:
            define("LLVM_USE_SANITIZER", ";".join(sanitizers))

        if args.enable_sanitize_coverage:
            define("LLVM_USE_SANITIZE_COVERAGE", "ON")

        if args.export_compile_commands:
            define("CMAKE_EXPORT_COMPILE_COMMANDS", "ON")

        if args.distcc:
            define("CMAKE_C_COMPILER_LAUNCHER:PATH", toolchain.distcc)
            define("CMAKE_CXX_COMPILER_LAUNCHER:PATH", toolchain.distcc)

        if args.cmake_c_launcher:
            define("CMAKE_C_COMPILER_LAUNCHER:PATH", args.cmake_c_launcher)
        if args.cmake_cxx_launcher:
            define("CMAKE_CXX_COMPILER_LAUNCHER:PATH", args.cmake_cxx_launcher)

        define("CMAKE_C_COMPILER:PATH", toolchain.cc)
        define("CMAKE_CXX_COMPILER:PATH", toolchain.cxx)
        define("CMAKE_LIBTOOL:PATH", toolchain.libtool)

        if self.__is_xcode_generator:
            define("CMAKE_CONFIGURATION_TYPES",
                   "Debug;Release;MinSizeRel;RelWithDebInfo")

        if args.clang_user_visible_version:
            major, minor, patch = \
                args.clang_user_visible_version.components[0:3]
            define("LLVM_VERSION_MAJOR:STRING", major)
            define("LLVM_VERSION_MINOR:STRING", minor)
            define("LLVM_VERSION_PATCH:STRING", patch)
            define("CLANG_VERSION_MAJOR:STRING", major)
            define("CLANG_VERSION_MINOR:STRING", minor)
            define("CLANG_VERSION_PATCH:STRING", patch)

        if self.__is_ninja_generator and \
                (args.build_ninja or toolchain.ninja is not None):
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

        if self.__is_ninja_generator:
            build_args += ['-j%s' % jobs]
            if args.verbose_build:
                build_args += ['-v']

        elif self.__is_make_generator:
            build_args += ['-j%s' % jobs]
            if args.verbose_build:
                build_args += ['VERBOSE=1']

        elif self.__is_xcode_generator:
            build_args += ['-parallelizeTargets',
                           '-jobs', str(jobs)]

        return build_args

    def generate(self, source_dir, build_dir, cmake_options=None):
        cmake_options = [] if cmake_options is None else cmake_options
        shell.makedirs(build_dir)
        extra_environment = {}
        if self.args.distcc:
            extra_environment['DISTCC_HOSTS'] = 'localhost,lzo,cpp'
        with shell.pushd(build_dir):
            shell.call([self.toolchain.cmake] + self.common_options() +
                       cmake_options + self.args.extra_cmake_options +
                       [source_dir],
                       env=extra_environment)

    def generate_if_needed(self, source_dir, build_dir, cmake_options=None):
        # Compute the generator output file to check for, to determine if we
        # must reconfigure. We only handle Ninja for now.
        #
        # This is important for ensuring that if a Cmake configuration fails in
        # CI, that we will still be willing to rerun the configuration process.
        cmake_options = [] if cmake_options is None else cmake_options
        generator_output_path = None
        if self.__is_ninja_generator:
            generator_output_path = os.path.join(build_dir, 'build.ninja')

        # Configure if necessary
        cmake_cache_path = os.path.join(build_dir, 'CMakeCache.txt')
        if self.args.reconfigure or \
                not os.path.exists(cmake_cache_path) or \
                (generator_output_path is not None and
                    not os.path.exists(generator_output_path)):
            self.generate(source_dir, build_dir, cmake_options)

    def build_targets(self, build_dir, build_variant, targets=None):
        targets = ['all'] if targets is None else targets
        self.__cmake_build(build_dir,
                           cmake_arguments=self.__cmake_config(build_variant),
                           build_arguments=self.build_args(),
                           targets=targets)

    def install_targets(self, build_dir, targets=None, environment=None):
        targets = [] if targets is None else targets
        self.__cmake_build(build_dir,
                           targets=targets,
                           environment=environment)

    def __cmake_build(self, build_dir, cmake_arguments=None,
                      build_arguments=None, targets=None, environment=None):
        cmake_arguments = [] if cmake_arguments is None else cmake_arguments
        build_arguments = [] if build_arguments is None else build_arguments
        targets = [] if targets is None else targets
        distcc_pump = []
        if self.args.distcc:
            distcc_pump = [self.toolchain.distcc_pump]

        if self.__is_xcode_generator:
            targets = self.__transform_targets_for_xcode(targets)

        shell.call(distcc_pump + [self.toolchain.cmake, '--build', build_dir] +
                   cmake_arguments + ['--'] + build_arguments + targets,
                   env=environment)

    def __cmake_config(self, build_variant):
        if self.__is_xcode_generator:
            return [
                '--target', 'ZERO_CHECK',
                '--config', build_variant
            ]
        else:
            return []

    @property
    def __is_xcode_generator(self):
        return self.args.cmake_generator == 'Xcode'

    @property
    def __is_ninja_generator(self):
        return self.args.cmake_generator == 'Ninja'

    @property
    def __is_make_generator(self):
        return self.args.cmake_generator == 'Unix Makefiles'

    def __transform_targets_for_xcode(self, targets):
        # Xcode generator uses "ALL_BUILD" instead of "all".
        # Also, xcodebuild uses -target instead of bare names.
        targets = ['ALL_BUILD'
                   if t == 'all' else t
                   for t in targets]
        targets = itertools.chain.from_iterable(
            [['--target', t] for t in targets])
        return targets
