# swift_build_support/products/foundation.py ---------------------*- python -*-
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

from . import (product, curl, libxml2)
from . import swift
from .. import cmake

import os
import os.path
import sys

class Foundation(product.Product):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-corelibs-foundation"

    @classmethod
    def builder_class(cls):
        return FoundationBuilder


class FoundationStatic(Foundation):
    @classmethod
    def product_name(cls):
        return 'foundation_static'


class FoundationBuilder(product.CMakeProductBuilder,
                        swift.SwiftHostVariantTripleMixin):
    def __init__(self, product_class, args, toolchain, workspace, host):
        product.CMakeProductBuilder.__init__(
            self, product_class, args, toolchain, workspace, host)
        swift.SwiftHostVariantTripleMixin.__init__(self, host)

        swiftc_bin = os.path.join(
            self._workspace.build_dir(self._args.host_target, 'swift'), 'bin',
            'swiftc')
        llvm_bin = os.path.join(
            self._workspace.build_dir(self._args.host_target, 'llvm'), 'bin')
        if not sys.platform.startswith('win32'):
            cc = 'clang'
            cxx = 'clang++'
        else:
            cc = 'clang-cl.exe'
            cxx = 'clang-cl.exe'

        # Because XCTest is needed to build the test, and Foundation is needed
        # to build XCTest, this project is generated twice: once during
        # building, and once during testing, with different options. We prepare
        # a common set of options, and two sets of options for each generate
        # phase. Before each generation, we will replace the _cmake_options to
        # the right ones.

        common_options = cmake.CMakeOptions()
        common_options.define(
            'CMAKE_BUILD_TYPE:STRING', self._build_variant)
        common_options.define(
            'CMAKE_C_COMPILER:PATH', os.path.join(llvm_bin, cc))
        common_options.define(
            'CMAKE_CXX_COMPILER:PATH', os.path.join(llvm_bin, cxx))
        common_options.define('CMAKE_SWIFT_COMPILER:PATH', swiftc_bin)
        common_options.define(
            'CMAKE_INSTALL_PREFIX:PATH', self._host_install_prefix)

        if self._args.build_libicu:
            (swift_host_variant,
                _,
                swift_host_variant_arch) = self._swift_host_variant_triple
            icu_root = os.path.join(
                self._workspace.build_dir(self._host.name, 'libicu'),
                'tmp_install')
            icu_libdir = os.path.join(
                self._workspace.build_dir(self._host.name, 'swift'), 'lib',
                'swift', swift_host_variant, swift_host_variant_arch)
            common_options.define('ICU_ROOT:PATH', icu_root)
            common_options.define(
                'ICU_INCLUDE_DIR:PATH', os.path.join(icu_root, 'include'))
            common_options.define(
                'ICU_UC_LIBRARIES:FILEPATH',
                os.path.join(icu_libdir, 'libicuucswift.so'))
            common_options.define(
                'ICU_UC_LIBRARY:FILEPATH',
                os.path.join(icu_libdir, 'libicuucswift.so'))
            common_options.define(
                'ICU_UC_LIBRARY_DEBUG:FILEPATH',
                os.path.join(icu_libdir, 'libicuucswift.so'))
            common_options.define(
                'ICU_UC_LIBRARY_RELEASE:FILEPATH',
                os.path.join(icu_libdir, 'libicuucswift.so'))
            common_options.define(
                'ICU_I18N_LIBRARIES:FILEPATH',
                os.path.join(icu_libdir, 'libicui18nswift.so'))
            common_options.define(
                'ICU_I18N_LIBRARY:FILEPATH',
                os.path.join(icu_libdir, 'libicui18nswift.so'))
            common_options.define(
                'ICU_I18N_LIBRARY_DEBUG:FILEPATH',
                os.path.join(icu_libdir, 'libicui18nswift.so'))
            common_options.define(
                'ICU_I18N_LIBRARY_RELEASE:FILEPATH',
                os.path.join(icu_libdir, 'libicui18nswift.so'))

        if self._args.build_libdispatch:
            libdispatch_build_dir = self._workspace.build_dir(
                self._host.name, 'libdispatch')
            common_options.define(
                'FOUNDATION_PATH_TO_LIBDISPATCH_SOURCE',
                self._workspace.source_dir('libdispatch'))
            common_options.define(
                'FOUNDATION_PATH_TO_LIBDISPATCH_BUILD',
                libdispatch_build_dir)
        else:
            common_options.define(
                'FOUNDATION_ENABLE_LIBDISPATCH:BOOL', False)

        common_options.define(
            'BUILD_SHARED_LIBS', product_class != FoundationStatic)

        # HACK: Windows builds its own dependencies, but in order to avoid
        # cruft in the main build-script, the dependencies are hidden here.
        self.__dependencies = []
        if self._host.platform.name == 'windows':
            curl_builder = curl.Curl.builder_class()(
                curl.Curl, args, toolchain, workspace, host)
            libxml2_builder = libxml2.LibXML2.builder_class()(
                libxml2.LibXML2, args, toolchain, workspace, host)
            self.__dependencies += [curl_builder, libxml2_builder]

            # FIXME: should ICU_ROOT be calculated like that. It is flaky.
            common_options.define(
                'ICU_ROOT',
                os.path.dirname(os.path.dirname(self._args.icu_uc_path)))
            common_options.define(
                'CURL_LIBRARY', curl_builder.static_lib_path)
            common_options.define(
                'CURL_INCLUDE_DIR', curl_builder.include_dir)
            common_options.define(
                'LIBXML2_LIBRARY', libxml2_builder.static_lib_path)
            common_options.define(
                'LIBXML2_INCLUDE_DIR', libxml2_builder.include_dir)

        self.__build_options = cmake.CMakeOptions()
        self.__build_options += common_options
        self.__build_options.define('ENABLE_TESTING:BOOL', False)

        self.__test_options = cmake.CMakeOptions()
        self.__test_options += common_options
        self.__test_options.define('ENABLE_TESTING:BOOL', True)
        self.__test_options.define(
            'FOUNDATION_PATH_TO_XCTEST_BUILD:PATH',
            self._workspace.build_dir(self._host.name, 'xctest'))

        # Set the PKG_CONFIG_PATH so that CoreFoundation can find the libraries
        # and header files.
        libicu_build_dir = self._workspace.build_dir(self._host.name, 'libicu')
        libicu_pkg_config_path = os.path.join(libicu_build_dir, 'config')
        libicu_ld_library_path = os.path.join(libicu_build_dir, 'lib')
        if 'PKG_CONFIG_PATH' in os.environ:
            os.environ['PKG_CONFIG_PATH'] += os.pathsep + libicu_pkg_config_path
        else:
            os.environ['PKG_CONFIG_PATH'] = libicu_pkg_config_path
        if 'LD_LIBRARY_PATH' in os.environ:
            os.environ['LD_LIBRARY_PATH'] += os.pathsep + libicu_ld_library_path
        else:
            os.environ['PKG_CONFIG_PATH'] = libicu_ld_library_path

    def do_build(self):
        for dependency in self.__dependencies:
            dependency.do_build()
        try:
            old_cmake_options = self._cmake_options
            self._cmake_options = self.__build_options
            product.CMakeProductBuilder.do_build(self)
        finally:
            self._cmake_options = old_cmake_options

    def do_test(self):
        for dependency in self.__dependencies:
            dependency.do_test()
        if not self._should_test:
            return
        try:
            old_cmake_options = self._cmake_options
            product.CMakeProductBuilder.do_test(self, force_regenerate=True)
        finally:
            self._cmake_options = old_cmake_options

    def do_install(self):
        for dependency in self.__dependencies:
            dependency.do_install()
        product.CMakeProductBuilder.do_install(self)

    @property
    def _build_variant(self):
        return self._args.foundation_build_variant

    @property
    def _should_build(self):
        if self._host.platform.name == 'macosx':
            print(
                "Skipping Foundation on OS X -- use the Xcode project instead")
            return False

        return (self._args.build_foundation and
                (self._product.__class__ != FoundationStatic or
                 self._args.build_swift_static_stdlib))

    @property
    def _should_test(self):
        if self._host.platform.name == 'macosx':
            print(
                "Skipping Foundation on OS X -- use the Xcode project instead")
            return False

        # FIXME: allow tests once the unit tests work with foundation_static
        # TODO: build-script-impl defined --skip-test-foundation
        return self._args.test and \
            self._product.__class__ != FoundationStatic and \
            self._args.build_xctest

    @property
    def _test_executable_target(self):
        return 'TestFoundation'

    @property
    def _test_results_targets(self):
        return ['test']
