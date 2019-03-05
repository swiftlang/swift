# swift_build_support/products/libdispatch.py -------------------*- python -*-
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

import os.path


class LibDispatch(product.Product):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-corelibs-libdispatch"

    @classmethod
    def builder_class(cls):
        def chooser(product_class, args, toolchain, workspace, host):
            if host.platform.name == 'macosx':
                return LibDispatchMakeBuilder(
                    product_class, args, toolchain, workspace, host)
            else:
                return LibDispatchCMakeBuilder(
                    product_class, args, toolchain, workspace, host)
        return chooser


class LibDispatchStatic(LibDispatch):
    @classmethod
    def product_name(cls):
        return 'libdispatch_static'


class LibDispatchMakeBuilder(product.BuilderHostInstallMixin):
    def __init__(self, product_class, args, toolchain, workspace, host):
        product.BuilderHostInstallMixin.__init__(self, args, host)
        self._source_dir = workspace.source_dir(
            product_class.product_source_name())
        self._build_dir = workspace.build_dir(
            host.name, product_class.product_name())
        self._product = product_class(
            args, toolchain, self._source_dir, self._build_dir)
        self._args = args
        self._host = host
        self._toolchain = toolchain
        self._workspace = workspace

    def do_build(self):
        if not self._args.build_libdispatch:
            return

        if self._args.reconfigure or \
                not os.path.exists(
                    os.path.join(self._build_dir, 'config.status')):
            print('Reconfiguring libdispatch')
            # First time building; need to run autotools and configure
            if self._args.libdispatch_build_variant == 'Release':
                build_variant = 'release'
            elif self._args.libdispatch_build_variant == 'RelWithDebInfo':
                build_variant = 'releasedebuginfo'
            else:
                build_variant = 'debug'

            # FIXME: Is this wrong? Instead of checking the product, it does it
            # for both the static and the non-static products.
            if self._args.build_swift_static_stdlib:
                libdispatch_maybe_static = ['--enable-static=yes']
            else:
                libdispatch_maybe_static = []

            swift_build_dir = self._workspace.build_dir(
                self._host.name, 'swift')
            swiftc_bin = os.path.join(
                self._workspace.build_dir(self._args.host_target, 'swift'),
                'bin', 'swiftc')
            llvm_bin_dir = os.path.join(
                self._workspace.build_dir(self._args.host_target, 'llvm'),
                'bin')
            prefix = os.path.join(
                self._host_install_destdir, self._host_install_prefix)

            shell.makedirs(self._build_dir)
            with shell.pushd(self._build_dir):
                shell.call(['autoreconf', '-fvi'])
                shell.call([
                    os.path.join(self._source_dir, 'configure'),
                    '--with-swift-toolchain={}'.format(swift_build_dir),
                    '--with-build-variant={}'.format(build_variant),
                    '--prefix={}'.format(prefix)] +
                    libdispatch_maybe_static,
                    env={
                        'CC': os.path.join(llvm_bin_dir, 'clang'),
                        'CXX': os.path.join(llvm_bin_dir, 'clang++'),
                        'SWIFTC': swiftc_bin})
        else:
            print('Skipping reconfiguration of libdispatch')

        with shell.pushd(self._build_dir):
            shell.call(['make'])

    def do_test(self):
        # FIXME: allow tests once the unit tests work with libdispatch_static
        # TODO: build-script-impl defined --skip-test-libdispatch
        if not self._args.test or self._product.__class__ == LibDispatchStatic:
            return

        with shell.pushd(self._build_dir):
            shell.call(['make', 'build-tests'])
            print('--- Running tests for {} ---'.format(self._product.name))
            shell.call(['make', 'check'], env={'VERBOSE': '1'})
            print('--- Finished tests for {} ---'.format(self._product.name))


class LibDispatchCMakeBuilder(product.CMakeProductBuilder):
    def __init__(self, product_class, args, toolchain, workspace, host):
        product.CMakeProductBuilder.__init__(
            self, product_class, args, toolchain, workspace, host)

        swift_build_dir = self._workspace.build_dir(self._host.name, 'swift')
        swiftc_bin = os.path.join(
            self._workspace.build_dir(self._args.host_target, 'swift'),
            'bin', 'swiftc')
        llvm_bin_dir = os.path.join(
            self._workspace.build_dir(self._args.host_target, 'llvm'),
            'bin')
        if not self._host.platform.name == 'windows':
            c_compiler = 'clang'
            cxx_compiler = 'clang++'
        else:
            c_compiler = cxx_compiler = 'clang-cl.exe'

        self._cmake_options.define(
            'CMAKE_BUILD_TYPE:STRING', self._build_variant)
        self._cmake_options.define(
            'CMAKE_C_COMPILER:PATH', os.path.join(llvm_bin_dir, c_compiler))
        self._cmake_options.define(
            'CMAKE_CXX_COMPILER:PATH',
            os.path.join(llvm_bin_dir, cxx_compiler))
        self._cmake_options.define('CMAKE_SWIFT_COMPILER:PATH', swiftc_bin)
        self._cmake_options.define(
            'CMAKE_INSTALL_PREFIX:PATH', self._host_install_prefix)
        self._cmake_options.define('CMAKE_INSTALL_LIBDIR:PATH', 'lib')
        self._cmake_options.define('ENABLE_SWIFT:BOOL', True)
        self._cmake_options.define(
            'Swift_DIR',
            os.path.join(swift_build_dir, 'lib', 'cmake', 'swift'))
        # FIXME: Windows doesn't support building the tests at the moment.
        self._cmake_options.define(
            'ENABLE_TESTING:BOOL', self._host.platform.name != 'windows')
        self._cmake_options.define(
            'BUILD_SHARED_LIBS:BOOL', product_class != LibDispatchStatic)

    @property
    def _build_variant(self):
        return self._args.libdispatch_build_variant

    @property
    def _should_build(self):
        return (self._args.build_libdispatch and
                (self._product.__class__ != LibDispatchStatic or
                 self._args.build_swift_static_stdlib))

    @property
    def _should_test(self):
        # FIXME: allow tests once the unit tests work with libdispatch_static
        # TODO: build-script-impl defined --skip-test-libdispatch
        return self._args.test and self._product.__class__ != LibDispatchStatic

    @property
    def _test_executable_target(self):
        # Tests are build during the main build.
        return None

    @property
    def _test_results_targets(self):
        return ['test']
