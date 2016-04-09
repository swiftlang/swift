# build_script/cmake.py -----------------------------------------*- python -*-
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
Represent CMake command
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import re
import os.path
import itertools
import subprocess

from .utils import CachedProperty
from . import shell
from . import targets


class CMakeOptions(object):
    def __init__(self, items=None):
        if items is None:
            items = []
        self._options = items

    def define(self, var, value):
        if var.endswith(':BOOL'):
            value = self.true_false(value)
        elif value is None:
            value = ""
        self._options.append('-D%s=%s' % (var, value))

    def unset(self, var):
        var = var.rsplit(':', 1)[0]  # strip :BOOL etc
        self._options.append('-U' + var)

    def true_false(self, value):
        if value in [True, 1, 'true', 'True', 'TRUE', '1']:
            return 'TRUE'
        elif value in [False, 0, 'false', 'False', 'FALSE', '0']:
            return 'FALSE'
        else:
            raise ValueError('true_false: unknown value: ' + str(value))

    def false_true(self, value):
        if self.true_false == 'TRUE':
            return 'FALSE'
        else:
            return 'TRUE'

    def __len__(self):
        return self._options.__len__()

    def __iter__(self):
        return self._options.__iter__()

    def __add__(self, other):
        return CMakeOptions(self._options + other)

    def __iadd__(self, other):
        self._options += other
        return self

class CMake(object):
    def __init__(self, path,
                 host_cc, host_cxx,
                 host_distcc, host_distcc_pump,
                 ninja_path,
                 args):
        self.path = path
        self.host_cc = host_cc
        self.host_cxx = host_cxx
        self.distcc = host_distcc
        self.distcc_pump = host_distcc_pump
        self.args = args
        self.ninja_path = ninja_path
        self.generator = args.cmake_generator

        self.sanitizers = []
        if args.enable_asan:
            self.sanitizers.append('Address')

        if args.enable_ubsan:
            self.sanitizers.append('Undefined')

    # static utility functions

    def is_release_build_type(self, build_type):
        return build_type in ['Release', 'RelWithDebInfo']

    def common_cross_c_flags(self, deployment_target):
        sys, arch = targets.split(deployment_target)
        if sys == 'iphonesimulator':
            return ('-arch', arch,
                    '-mios-simulator-version-min=' +
                    self.args.darwin_deployment_version_ios)
        if sys == 'iphoneos':
            return ('-arch', arch,
                    '-miphoneos-version-min=' +
                    self.args.darwin_deployment_version_ios)
        if sys == 'appletvsimulator':
            return ('-arch', arch,
                    '-mtvos-simulator-version-min=' +
                    self.args.darwin_deployment_version_tvos)
        if sys == 'appletvos':
            return ('-arch', arch,
                    '-mtvos-version-min=' +
                    self.args.darwin_deployment_version_tvos)
        if sys == 'watchsimulator':
            return ('-arch', arch,
                    '-mwatchos-simulator-version-min=' +
                    self.args.darwin_deployment_version_watchos)
        if sys == 'watchos':
            return ('-arch', arch,
                    '-mwatchos-version-min=' +
                    self.args.darwin_deployment_version_watchos)
        return []

    def _parse_clang_compiler_version(self, value):
        version_re = re.compile(r'([0-9]*)\.([0-9]*)\.([0-9]*)')
        m = version_re.match(value)
        if m is None:
            return None
        return (m.group(1), m.group(2), m.group(3),)

    # Property accessors

    @CachedProperty
    def version(self):
        import subprocess
        try:
            output = shell.query([self.path, '--version'])
            version_re = re.compile(r'cmake_version ([0-9\.]+)')
            m = version_re.match(output)
            if m is not None:
                return m[1]
            else:
                return None
        except subprocess.CalledProcessError:
            return None

    def needs_to_specify_standard_computed_defaults(self):
        if self.version == '3.4.0':
            return True
        else:
            return False

    @CachedProperty
    def computed_jobs(self):

        jobs = self.args.build_jobs
        if self.args.distcc:
            # When we use distcc, query job count with distcc command
            jobs = int(shell.query([self.distcc, '-j']))
        return jobs

    # Guts

    def _common_configure_options(self):
        '''\
        Return common CMake options.
        '''

        options = CMakeOptions(['-G', self.generator])
        define = options.define

        if self.generator == 'Ninja' and self.ninja_path is not None:
            # If we use just built ninja, set the path of the exectable.
            define('CMAKE_MAKE_PROGRAM', self.ninja_path)

        if len(self.sanitizers):
            sanitizers_str = ';'.join(self.sanitizers)
            define('LLVM_USE_SANITIZER', sanitizers_str)

        if self.args.export_compile_commands:
            define('CMAKE_EXPORT_COMPILE_COMMANDS:BOOL', True)

        if self.args.distcc:
            define('CMAKE_C_COMPILER:PATH', self.distcc)
            define('CMAKE_CXX_COMPILER:PATH', self.distcc)
            define('CMAKE_C_COMPILER_ARG1', self.host_cc)
            define('CMAKE_CXX_COMPILER_ARG1', self.host_cxx)
        else:
            define('CMAKE_C_COMPILER:PATH', self.host_cc)
            define('CMAKE_CXX_COMPILER:PATH', self.host_cxx)

        if self.generator == 'Xcode':
            define('CMAKE_CONFIGURATION_TYPES',
                   'Debug;Release;MinSizeRel;RelWithDebInfo')

        if self.args.clang_compiler_version is not None:
            (major, minor, patch) = self._parse_clang_compiler_version(
                self.args.clang_compiler_version)
            define('LLVM_VERSION_MAJOR:STRING', major)
            define('LLVM_VERSION_MINOR:STRING', minor)
            define('LLVM_VERSION_PATCH:STRING', patch)

        return options

    def configure(self, source_dir, build_dir, options, use_module_cache=True):
        '''\
        Configure cmake project with given paramters.
        '''

        args = [self.path, ]
        args += self._common_configure_options()
        args += options
        args += self.args.user_config_args
        args += [source_dir, ]

        # Create build directory if not exists
        if not os.path.exists(build_dir):
            os.makedirs(build_dir)

        # Clean the product-local module cache.
        if use_module_cache:
            module_cache_dir = os.path.join(build_dir, 'module_cache')
            if os.path.exists(module_cache_dir):
                shell.rmtree(module_cache_dir)
            shell.makedirs(module_cache_dir)

        # Do configure
        with shell.pushd(build_dir):
            shell.invoke(args)

    def _build_args(self):
        result = []

        # Parallel jobs
        jobs = self.computed_jobs

        # Generator depend build arguments.
        if self.generator == 'Ninja':
            result += ['-j%d' % jobs, ]
            if self.args.verbose_build:
                result += ['-v', ]
        elif self.generator == 'Unix Makefiles':
            result += ['-j%d' % jobs, ]
            if self.args.verbose_build:
                result += ['VERBOSE=1', ]
        elif self.generator == 'Xcode':
            result += ['-parallelizeTargets', '-jobs', str(jobs), ]

        return result 

    def build(self, build_dir, build_targets, config_opts=None):
        '''\
        Invoke `cmake --build`
        '''
        build_cmd = [self.path, ]

        if self.args.distcc:
            # use distcc
            build_cmd = [self.distcc_pump,] + build_cmd

        build_cmd += ['--build', build_dir]
        if self.generator == 'XCode':
            # CMake automatically adds --target ALL_BUILD if we don't pass
            # this.
            build_cmd += ['--target', 'ZERO_CHECK']
        if config_opts is not None:
            build_cmd += config_opts
        build_cmd += ['--', ]  # dash-dash
        build_cmd += self._build_args()

        if self.generator == 'Xcode':
            # Xcode can't restart itself if it turns out we need to
            # reconfigure. Do an advance build to handle that.
            shell.invoke(build_cmd)

            # Xcode generator uses "ALL_BUILD" instead of "all".
            # Also, xcodebuild uses `-target {name}` instead of bare names.
            build_targets = (
                'ALL_BUILD' if t == 'all' else t for t in build_targets)
            build_targets = itertools.chain.from_iterable(
                ['-target', t] for t in build_targets)

        build_cmd += build_targets

        # Do build
        shell.invoke(build_cmd)

    def test(self, build_dir, test_targets, config_opts=None):
        build_cmd = [self.path, '--build', build_dir]
        if self.generator == 'XCode':
            build_cmd += ['--target', 'ZERO_CHECK']

        if config_opts is not None:
            build_cmd += config_opts

        build_cmd += ['--', ]  # dash-dash
        build_cmd += self._build_args()
        
        need_manual_invoke = False
        if self.generator == "Ninja":
            query_cmd = build_cmd + ["--version"]
            result = shell.query(query_cmd,
                                 stderr=subprocess.STDOUT)
            result = result.lower()
            if "llbuild" not in result:
                need_manual_invoke = True

        for target in test_targets:
            if need_manual_invoke:
                # Ninja buffers command output to avoid scrambling the output
                # of parallel jobs, which is awesome... except that it
                # interferes with the progress meter when testing.  Instead of
                # executing ninja directly, have it dump the commands it would
                # run, strip Ninja's progress prefix, and tell the shell to
                # execute that.
                test_cmds = shell.query(build_cmd + ["-n", "-v", target])
                test_cmds = test_cmds.splitlines()
                test_cmds = [re.sub(r"^[^]]*] ", "", t) for t in test_cmds]
                test_cmds = " && ".join(["set -x"] + test_cmds)
                shell.invoke(['sh', '-c', test_cmds])
            else:
                invoke = list(build_cmd)
                if self.generator == 'Xcode':
                    invoke += ["-target", target, ]
                else:
                    invoke += [target, ]
                shell.invoke(invoke)

    def install(self,build_dir, dest_dir, install_targets = ("install", )):
        args = [self.path, '--build', build_dir, '--']
        args += install_targets
        shell.invoke(args,
                     env=[("DESTDIR", dest_dir), ])
