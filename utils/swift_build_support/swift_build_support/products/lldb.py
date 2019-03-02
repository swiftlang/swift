# swift_build_support/products/lldb.py --------------------------*- python -*-
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

from . import product
from .. import shell

import datetime
import multiprocessing
import os
import os.path
import shlex
import sys


class LLDB(product.Product):

    @classmethod
    def builder_class(cls):
        def chooser(product_class, args, toolchain, workspace, host):
            if host.platform.is_darwin and args.lldb_build_with_xcode:
                return LLDBXcodeBuilder(
                    product_class, args, toolchain, workspace, host)
            else:
                return LLDBCMakeBuilder(
                    product_class, args, toolchain, workspace, host)
        return chooser


class LLDBBuilderMixin(object):
    def __init__(self, args, toolchain, workspace, host):
        self.__args = args
        self.__toolchain = toolchain
        self.__workspace = workspace
        self.__host = host

    @property
    def _lldb_is_buildbot_build(self):
        return ('JENKINS_HOME' in os.environ and
                'JOB_NAME' in os.environ and
                'BUILD_NUMBER' in os.environ)

    @property
    def _lldb_build_date(self):
        return datetime.date.today().strftime('%Y-%m-%d')

    @property
    def _results_dir(self):
        results_dir = os.path.join(self._lldb_build_dir, 'test-results')
        shell.makedirs(results_dir)
        return results_dir

    @property
    def _lldb_test_subdir_clause(self):
        # Optionally specify a test subdirectory.
        # TODO: build-script-impl defines --lldb-test-swift-only
        # if self.__args.lldb_test_swift_only:
        #     return ['--test-subdir', 'lang/swift']
        # else:
            return []

    @property
    def _lldb_test_categories(self):
        # Optionally specify category filters.
        # Watchpoint testing is currently disabled: see rdar://38566150.
        # TODO: build-script-impl defines --lldb-test-swift-only
        # if self.__args.lldb_test_swift_only:
        #     return ['--skip-category=watchpoint', '--skip-category=dwo']
        # else:
            return ['--skip-category=watchpoint']

    @property
    def _dotest_extra(self):
        # Options to find the just-built libdispatch and Foundation.
        if self.__host.platform.is_darwin or not self.__args.build_foundation:
            return []
        else:
            libdispatch_source_dir = self.__workspace.source_dir('libdispatch')
            libdispatch_build_dir = \
                self.__workspace.build_dir(self.__host.name, 'libdispatch')
            foundation_build_dir = \
                self.__workspace.build_dir(self.__host.name, 'foundation')
            return [
                '-I{}'.format(foundation_build_dir),
                '-Xcc', '-F{}'.format(
                    os.path.join(
                        foundation_build_dir, 'CoreFoundation-prefix',
                        'System', 'Library', 'Frameworks')),
                '-I{}'.format(os.path.join(foundation_build_dir, 'swift')),
                '-I{}'.format(libdispatch_source_dir),
                '-L{}'.format(foundation_build_dir),
                '-L{}'.format(libdispatch_build_dir),
                '-L{}'.format(os.path.join(libdispatch_build_dir, 'src')),
                '-Xlinker', '-rpath',
                '-Xlinker', os.path.join(libdispatch_build_dir, 'src'),
                '-Xlinker', '-rpath', '-Xlinker', libdispatch_build_dir,
                '-Xlinker', '-rpath', '-Xlinker', foundation_build_dir]

    @property
    def _lldb_build_dir(self):
        return self.__workspace.build_dir(self.__host.name, 'lldb')


class LLDBCMakeBuilder(product.CMakeProductBuilder, LLDBBuilderMixin):
    def __init__(self, product_class, args, toolchain, workspace, host):
        if args.cmake_generator != 'Ninja':
            raise ValueError("LLDB can only be build with Ninja")

        product.CMakeProductBuilder.__init__(self, product_class, args,
                                             toolchain, workspace, host)
        LLDBBuilderMixin.__init__(self, args, toolchain, workspace, host)

        llvm_build_dir = self._workspace.build_dir(self._host.name, 'llvm')
        swift_build_dir = self._workspace.build_dir(self._host.name, 'swift')
        swift_host_build_dir = self._workspace.build_dir(
            self._args.host_target, 'swift')

        # TODO: build-script-impl defines --lldb-cmake-options but nodody uses
        # it. What's the difference with lldb-extra-cmake-args?

        # For cross-compilable hosts, we need to know the triple and it must be
        # the same for both LLVM and Swift
        self._cmake_options.define(
            'LLVM_HOST_TRIPLE:STRING', self._swift_host_triple)

        # TODO: build-script-impl defines --lldb-extra-cmake-args but nobody
        # uses it.

        # Figure out if we think this is a buildbot build.
        # This will influence the lldb version line.
        lldb_is_buildbot_build = self._lldb_is_buildbot_build

        lldb_build_date = self._lldb_build_date

        self._cmake_options.define(
            'CMAKE_BUILD_TYPE:STRING', self._args.lldb_build_variant)
        self._cmake_options.define(
            'LLDB_SWIFTC:PATH',
            os.path.join(swift_host_build_dir, 'bin', 'swiftc'))
        self._cmake_options.define(
            'LLDB_SWIFT_LIBS:PATH', os.path.join(
                swift_host_build_dir, 'lib', 'swift'))
        self._cmake_options.define(
            'CMAKE_INSTALL_PREFIX:PATH', self._host_install_prefix)
        self._cmake_options.define(
            'Clang_DIR:PATH',
            os.path.join(llvm_build_dir, 'lib', 'cmake', 'clang'))
        self._cmake_options.define(
            'LLVM_DIR:PATH',
            os.path.join(llvm_build_dir, 'lib', 'cmake', 'llvm'))
        self._cmake_options.define(
            'LLDB_PATH_TO_SWIFT_SOURCE:PATH',
            self._workspace.source_dir('swift'))
        self._cmake_options.define(
            'LLDB_PATH_TO_SWIFT_BUILD:PATH', swift_build_dir)
        self._cmake_options.define(
            'LLDB_IS_BUILDBOT_BUILD:BOOL', lldb_is_buildbot_build)
        self._cmake_options.define('LLDB_BUILD_DATE:STRING', lldb_build_date)
        self._cmake_options.define('LLDB_ALLOW_STATIC_BINDINGS:BOOL', True)
        self._cmake_options.define(
            'LLDB_INCLUDE_TESTS:BOOL', not self._args.build_toolchain_only)

        if self._host.platform.is_darwin:
            self._cmake_options.define('LLDB_BUILD_FRAMEWORK:BOOL', True)
            self._cmake_options.define('LLDB_CODESIGN_IDENTITY', '')

    def do_test(self):
        # LLDB doesn't use CMake to test
        if not self._should_test:
            return

        results_dir = self._results_dir
        lldb_test_subdir_clause = self._lldb_test_subdir_clause
        lldb_test_categories = self._lldb_test_categories
        dotest_extra = self._dotest_extra

        lldb_build_dir = self._lldb_build_dir

        # TODO: build-script-impl defines --llvm-lit-args, but build-script
        # uses --lit-args for it.
        llvm_lit_args = shlex.split(self._args.lit_args)
        if self._args.enable_asan:
            # Limit the number of parallel tests.
            try:
                num_cpus = multiprocessing.cpu_count()
            except NotImplementedError:
                num_cpus = 1
            llvm_lit_args.append(' --threads={}'.format(num_cpus))

        # Prefer to use lldb-dotest, as building it guarantees that we build
        # all test dependencies. Ultimately we want to delete as much
        # lldb-specific logic from this file as possible and just have a single
        # call to lldb-dotest.

        with shell.pushd(lldb_build_dir):
            shell.call([self._toolchain.ninja, 'unittests/LLDBUnitTests'])
            shell.call([self._toolchain.ninja, 'lldb-test-deps'])
        with shell.pushd(results_dir):
            is_windows = sys.platform.startswith('win32')
            llvm_build_dir = \
                self._workspace.build_dir(self._host.name, 'llvm')
            llvm_lit = os.path.join(
                llvm_build_dir, 'bin',
                'llvm-lit' if not is_windows else 'llvm-lit.py')
            lit_dir = os.path.join(lldb_build_dir, 'lit')
            results_xml = os.path.join(results_dir, 'results.xml')
            lldb_test_build_dir = os.path.join(
                lldb_build_dir, 'lldb-test-build.noindex')
            maybe_python = [sys.executable] if is_windows else []
            shell.call(maybe_python + [llvm_lit, lit_dir] + llvm_lit_args +
                       ['--xunit-xml-output={}'.format(results_xml),
                        '--param',
                        'dotest-args="--build-dir {} {} {} -t -E \\"{}\\""'.
                        format(lldb_test_build_dir,
                               ' '.join(lldb_test_subdir_clause),
                               ' '.join(lldb_test_categories),
                               ' '.join(dotest_extra))])
            # TODO: build-script-impl defines --lldb-test-swift-compatibility
            # but nodoby uses it.
            # FIXME: this should test if the path is executable in compatible
            # systems.
            # if os.path.exists(self.__args.lldb_test_swift_compatibility):
            #     print('Running LLDB swift compatibility tests against {}' \
            #         .format(self.__args.lldb_test_swift_compatibility))
            #     shell.call([
            #         llvm_lit, lit_dir, *llvm_lit_args,
            #         '--xunit-xml-output={}'.format(result_xml),
            #         '--param',
            #         'dotest-args="--build-dir {} {} {} -G swift-history ' \
            #             '--swift-compiler \\"{}\\" -t -E \\"{}\\""'.format(
            #                 lldb_test_build_dir,
            #                 ' '.join(lldb_test_subdir_clause),
            #                 ' '.join(lldb_test_categories),
            #                 self.__args.lldb_test_swift_compatibility,
            #                 ' '.join(dotest_extra)),
            #         '--filter=compat'])

    @property
    def _build_variant(self):
        return self._args.lldb_build_variant

    @property
    def _should_build(self):
        return self._args.build_lldb

    @property
    def _should_test(self):
        # TODO: build-script-impl defines --skip-test-lldb
        return self._args.test


class LLDBXcodeBuilder(LLDBBuilderMixin):
    def __init__(self, product_class, args, toolchain, workspace, host):
        LLDBBuilderMixin.__init__(self, args, toolchain, workspace, host)
        self.__product_class = product_class
        self.__args = args
        self.__toolchain = toolchain
        self.__workspace = workspace
        self.__host = host

    def do_build(self):
        if not self.__args.build_lldb:
            return

        with shell.pushd(self.__workspace.source_dir(
                self.__product_class.product_name())):
            shell.call([
                'xcodebuild',
                '-target', 'desktop',
                '-configuration', self.__lldb_build_mode] +
                self.__lldb_xcode_build_options)

    def do_test(self):
        if not self.__args.test:
            return

        # FIXME: The xcode build project currently doesn't know how to run the
        # lit style tests.
        with shell.pushd(self.__workspace.source_dir(
                self.__product_class.product_name())):
            shell.call([
                'xcodebuild',
                '-scheme', 'lldb-gtest',
                '-configuration', self.__lldb_build_mode] +
                self.__lldb_xcode_build_options)

        results_dir = self._results_dir
        lldb_test_subdir_clause = self._lldb_test_subdir_clause
        lldb_test_categories = self._lldb_test_categories
        dotest_extra = self._dotest_extra

        # Handle test results formatter
        # TODO: build-script-impl defines --lldb-test-with-curses, but nobody
        # uses it
        # if self.__args.lldb_test_with_curses:
        #     lldb_formatter_opts = [
        #         '--results-formatter',
        #         'lldbsuite.test_event.formatter.curses.Curses',
        #         '--results-file', '/dev/stdout']
        # else:
        if True:
            lldb_formatter_opts = [
                '--results-formatter',
                'lldbsuite.test_event.formatter.xunit.XunitFormatter',
                '--results-file', os.path.join(results_dir, 'results.xml'),
                '-O--xpass=success',
                '-O--xfail=success']
            # Setup the xUnit results formatter.
            if self.__host.platform.is_darwin:
                # On non-Darwin, we ignore skipped test entirely so that they
                # don't pollute our xUnit results with non-actionable content.
                lldb_formatter_opts += [
                    '-O-ndsym',
                    '-O-rdebugserver',
                    '-O-rlibc++',
                    '-O-rlong.running'
                    '-O-rbenchmarks',
                    '-O-rrequires.one?.of.darwin']

        # Figure out which C/C++ compiler we should use for building test
        # inferiors.
        # TODO: build-script-impl defines --lldb-test-cc but nobody uses it
        # if self.__args.lldb_test_cc == 'host-toolchain':
        #     # Use the host toolchain: i.e. the toolchain specified by HOST_CC
        #     lldb_dotest_cc_opts = ['-C', self.__args.host_cc]
        # elif self.__args.lldb_test_cc is not None:
        #     # Use exactly the compiler path specified by the user.
        #     lldb_dotest_cc_opts = ['-C', self.__args.lldb_test_cc]
        # else:
        if True:
            # Use the clang that was just built in the tree.
            lldb_dotest_cc_opts = [
                '-C',
                os.path.join(
                    self.__workspace.build_dir(self.__args.host_target,
                                               'llvm'),
                    'bin', 'clang')]

        lldb_dotest_cc_opts += [
            '--filecheck',
            os.path.join(
                self.__workspace.build_dir(self.__args.host_target, 'llvm'),
                'bin', 'FileCheck')]

        # If we need to use the system debugserver, do so explicitly.
        # TODO: build-script-impl defines --lldb-use-system-debugserver
        # if is_darwin and self.__args.lldb_use_system_debugserver:
        #     lldb_test_debug_server = [
        #         '--server',
        #         os.path.join(
        #             shell.capture(['xcode-select', '-p']),
        #             '..', 'SharedFrameworks', 'LLDB.framework', 'Resources',
        #             'debugserver'),
        #         '--out-of-tree-debugserver']
        # else:
        if True:
            lldb_test_debug_server = []

        # Prefer to use lldb-dotest, as building it guarantees that we build
        # all test dependencies. Ultimately we want to delete as much
        # lldb-specific logic from this file as possible and just have a single
        # call to lldb-dotest.

        lldb_build_dir = self._lldb_build_dir
        lldb_executable = os.path.join(lldb_build_dir)
        swiftc_path = os.path.join(
            self.__workspace.build_dir(self.__args.host_target, 'swift'),
            'bin', 'swiftc')
        swift_libs_dir = os.path.join(
            self.__workspace.build_dir(self.__host.name, 'swift'),
            'lib', 'swift')
        lldb_test_build_dir = os.path.join(
            lldb_build_dir, 'lldb-test-build.noindex')
        dotest_path = os.path.join(
            self.__workspace.source_dir('lldb'), 'test', 'dotest.py')
        with shell.pushd(results_dir):
            shell.call([
                dotest_path,
                '--executable', lldb_executable] +
                lldb_test_debug_server +
                lldb_test_subdir_clause +
                lldb_test_categories +
                lldb_dotest_cc_opts +
                lldb_formatter_opts +
                ['--build-dir', lldb_test_build_dir,
                 '-t', '-E', dotest_extra],
                env={
                    'SWIFTC': swiftc_path,
                    'SWIFT_LIBS': swift_libs_dir})
            # TODO: build-script-impl defines --lldb-test-swift-compatibility
            # but nodoby uses it.
            # FIXME: this should test if the path is executable in compatible
            # systems.
            # if os.path.exists(self.__args.lldb_test_swift_compatibility):
            #     print('Running LLDB swift compatibility tests against {}' \
            #         .format(self.__args.lldb_test_swift_compatibility))
            shell.call([
                dotest_path,
                '--executable', lldb_executable] +
                lldb_test_debug_server +
                lldb_test_subdir_clause +
                lldb_test_categories +
                lldb_dotest_cc_opts +
                lldb_formatter_opts +
                ['--build-dir', lldb_test_build_dir,
                 '-G', 'swift-history',
                 '--swift-compiler', self.__args.lldb_test_swift_compatibility,
                 '-t', '-E', dotest_extra],
                env={
                    'SWIFTC': swiftc_path,
                    'SWIFT_LIBS': swift_libs_dir})

    def do_install(self):
        pass

    @property
    def __lldb_build_mode(self):
        if self.__args.lldb_build_variant == 'RelWithDebInfo':
            return 'CustomSwift-Release'
        else:
            return 'CustomSwift-{}'.format(self.__args.lldb_build_variant)

    @property
    def __lldb_xcode_build_options(self):
        llvm_source_dir = self.__workspace.source_dir('llvm')
        clang_source_dir = self.__workspace.source_dir('clang')
        swift_source_dir = self.__workspace.source_dir('swift')
        llvm_build_dir = self.__workspace.build_dir(self.__host.name, 'llvm')
        clang_build_dir = self.__workspace.build_dir(self.__host.name, 'clang')
        swift_build_dir = self.__workspace.build_dir(self.__host.name, 'swift')
        lldb_build_dir = self._lldb_build_dir

        options = [
            'LLDB_PATH_TO_LLVM_SOURCE="{}"'.format(llvm_source_dir),
            'LLDB_PATH_TO_CLANG_SOURCE="{}"'.format(clang_source_dir),
            'LLDB_PATH_TO_SWIFT_SOURCE="{}"'.format(swift_source_dir),
            'LLDB_PATH_TO_LLVM_BUILD="{}"'.format(llvm_build_dir),
            'LLDB_PATH_TO_CLANG_BUILD="{}"'.format(clang_build_dir),
            'LLDB_PATH_TO_SWIFT_BUILD="{}"'.format(swift_build_dir),
            'LLDB_IS_BUILDBOT_BUILD="{}"'.format(
                1 if self._lldb_is_buildbot_build else 0),
            'LLDB_BUILD_DATE="{}"'.format(self._lldb_build_date),
            'SYSROOT="{}"'.format(lldb_build_dir),
            'OBJROOT="{}"'.format(lldb_build_dir),
            '-UseNewBuildSystem=NO',
            # TODO: build-script-impl defines --lldb-extra-xcodebuild-args, but
            # nobody uses it.
            'MACOSX_DEPLOYMENT_TARGET=10.13']

        # TODO: build-script-impl defines --lldb-no-debugserver
        # if self.__args.lldb_no_debugserver:
        #     options += [
        #         'DEBUGSERVER_DISABLE_CODESIGN="1"',
        #         'DEBUGSERVER_DELETE_AFTER_BUILD="1"'
        #     ]

        # TODO: build-script-impl defines --lldb-use-system-debugserver
        # if self.__args.lldb_use_system_debugserver:
        #     options += [
        #         'DEBUGSERVER_USE_FROM_SYSTEM="1"'
        #     ]

        if self.__args.enable_asan:
            options += [
                'ENABLE_ADDRESS_SANITIZER="YES"',
                '-enableAddressSanitizer=YES'
            ]

        if self.__args.enable_ubsan:
            options += [
                'ENABLE_UNDEFINED_BEHAVIOR_SANITIZER="YES"',
                '-enableUndefinedBehaviorSanitizer=YES'
            ]

        if not self.__args.lldb_assertions:
            options += [
                'OTHER_CFLAGS="-DNDEBUG"'
            ]

        return options
