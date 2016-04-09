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
from numbers import Number

from .utils import CachedProperty
from . import shell
from . import targets
from .host import host


class CMakeOptions(object):
    def __init__(self, *args):
        self._options = []
        for var, value in args:
            self.define(var, value)

    def define(self, var, value):
        if var.endswith(':BOOL'):
            value = self.true_false(value)
        elif value is None:
            value = ""
        elif (not isinstance(value, str) and
              not isinstance(value, Number)):
            raise ValueError('value must be string or number')
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
    def __init__(self, path,
                 host_cc, host_cxx,
                 host_distcc, host_distcc_pump,
                 ninja_build,
                 args):
        self.path = path
        self.host_cc = host_cc
        self.host_cxx = host_cxx
        self.distcc = host_distcc
        self.distcc_pump = host_distcc_pump
        self.ninja_build = ninja_build
        self.args = args

        self.generator = args.cmake_generator

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
        if sys == 'android':
            return ('-arch', arch)
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

    def num_parallel_lto_link_jobs(self, gb_per_job):
        sys_memory_in_bytes = host.system_memory_in_bytes()
        link_jobs = int(sys_memory_in_bytes / 1000000000 / gb_per_job)
        link_jobs = min(self.computed_jobs, link_jobs)
        return link_jobs

    def common_options(self):
        '''\
        Return common CMake options.
        '''
        args = self.args

        options = CMakeOptions()
        define = options.define

        # If we use just built ninja, set the path of the exectable.
        if self.generator == 'Ninja' and self.ninja_build is not None:
            ninja_path = self.ninja_build.ninja_bin_path
            define('CMAKE_MAKE_PROGRAM', ninja_path)

        sanitizers = []
        if args.enable_asan:
            sanitizers.append('Address')
        if args.enable_ubsan:
            sanitizers.append('Undefined')
        if len(sanitizers):
            sanitizers_str = ';'.join(sanitizers)
            define('LLVM_USE_SANITIZER', sanitizers_str)

        if args.export_compile_commands:
            define('CMAKE_EXPORT_COMPILE_COMMANDS:BOOL', True)

        if args.distcc:
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

        if args.clang_compiler_version is not None:
            (major, minor, patch) = self._parse_clang_compiler_version(
                args.clang_compiler_version)
            define('LLVM_VERSION_MAJOR:STRING', major)
            define('LLVM_VERSION_MINOR:STRING', minor)
            define('LLVM_VERSION_PATCH:STRING', patch)

        if len(args.extra_cmake_options):
            options += args.extra_cmake_options

        return options

    def is_configured(self, build_dir):
        cmake_cache_path = os.path.join(build_dir, 'CMakeCache.txt')
        if not os.path.exists(cmake_cache_path):
            return False

        # Compute the generator output file to check for, to determine if we
        # must reconfigure. We only handle Ninja for now.
        #
        # This is important for ensuring that if a CMake configuration fails in
        # CI, that we will still be willing to rerun the configuration process.
        generator_output_path = None
        if self.generator == 'Ninja':
            generator_output_path = os.path.join(build_dir,
                                                 'build.ninja')
        if generator_output_path is not None:
            if not os.path.exists(generator_output_path):
                return False
        return True

    def configure(self, source_dir, build_dir, options, use_module_cache=True):
        '''\
        Configure cmake project with given paramters.
        '''

        command = [self.path, '-G', self.generator]
        command += self.common_options()
        command += options
        command += self.args.user_config_args
        command += [source_dir, ]

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
            shell.invoke(command)

    def _build_args(self):
        result = list(self.args.build_args)

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
            build_cmd = [self.distcc_pump, ] + build_cmd

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
            # NOTE: In dry_run mode, querying '--version' fails if
            #       it's not already configured. We treat it as no manual
            #       `cmake --build` invocation
            if not shell.dry_run:
                query_cmd = build_cmd + ["--version"]
                result = shell.query(query_cmd,
                                     stderr=subprocess.STDOUT, echo_=False)
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
                test_cmds = re.sub(r"^[^]]*] ", "", test_cmds,
                                   flags=re.MULTILINE)
                # FIXME: ignore_errors=True is just a workaround for current
                #        Swift source tree
                shell.runscript(test_cmds, ignore_errors=True)
            else:
                test_cmd = list(build_cmd)
                if self.generator == 'Xcode':
                    test_cmd += ["-target", target, ]
                else:
                    test_cmd += [target, ]
                shell.invoke(test_cmd)

    def install(self, build_dir, dest_dir, install_targets=("install", )):
        args = [self.path, '--build', build_dir, '--']
        args += install_targets
        shell.invoke(args,
                     env=[("DESTDIR", dest_dir), ])
