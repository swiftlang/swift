# swift_build_support/products/product.py -----------------------*- python -*-
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

from .. import cmake

import os.path

class Product(object):
    @classmethod
    def product_name(cls):
        """product_name() -> str

        The identifier-style name to use for this product.
        """
        return cls.__name__.lower()

    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        It provides a customization point for Product subclasses. It is set to
        the value of product_name() by default for this reason.
        """
        return cls.product_name()

    @classmethod
    def get_build_directory_name(cls, host_target):
        return "{}-{}".format(cls.product_name(),
                              host_target.name)

    @classmethod
    def is_build_script_impl_product(cls):
        """is_build_script_impl_product -> bool

        Whether this product is produced by build-script-impl.
        """
        return True

    def do_build(self, host_target):
        """do_build() -> void

        Perform the build, for a non-build-script-impl product.
        """
        raise NotImplementedError

    def do_test(self, host_target):
        """do_build() -> void

        Run the tests, for a non-build-script-impl product.
        """
        raise NotImplementedError

    @classmethod
    def builder_class(cls):
        raise ValueError("builder_class should be overriden by subclasses: {}".format(cls))

    def __init__(self, args, toolchain, source_dir, build_dir):
        self.args = args
        self.toolchain = toolchain
        self.source_dir = source_dir
        self.build_dir = build_dir
        self.cmake_options = cmake.CMakeOptions()

    @property
    def name(self):
        return self.__class__.product_name()


class BuilderHostInstallMixin(object):
    def __init__(self, args, host):
        self.__args = args
        self.__host = host

    @property
    def _host_install_destdir(self):
        if self.__has_cross_compile_hosts:
            # If cross compiling tools, install into a host-specific
            # subdirectory.
            if self.__should_include_host_in_lipo:
                # If this is one of the hosts we should lipo, install into a
                # temporary subdirectory.
                host_install_destdir = os.path.join(self.__args.build_dir,
                                                    'intermediate-install',
                                                    self.__host.name)
            else:
                host_install_destdir = os.path.join(self.__args.install_destdir,
                                                    self.__host.name)
        else:
            host_install_destdir = self.__args.install_destdir

        # Should always end in a path separator; it's a directory.
        return os.path.join(host_install_destdir, '')

    @property
    def _host_install_prefix(self):
        if self._is_cross_tools_host and \
                len(self.__args.cross_compile_install_prefixes) > 0:
            try:
                host_index = \
                    self.__args.cross_compile_hosts.index(self.__host.name)
            except ValueError:
                host_index = None
            if host_index is not None and host_index < \
                    len(self.__args.cross_compile_install_prefixes):
                host_install_prefix = \
                    self.__args.cross_compile_install_prefixes[host_index]
            else:
                # If there is no explicit install prefix for this host, use the
                # last one in the list.
                host_install_prefix = \
                    self.__args.cross_compile_install_prefixes[-1]
        else:
            host_install_prefix = self.__args.install_prefix

        # Should always be an absolute path; otherwise CMake will expand it as a
        # relative path from the build folder.
        if not os.path.isabs(host_install_prefix):
            host_install_prefix = os.path.join(os.path.sep, host_install_prefix)

        # Should always end in a path separator; it's a directory.
        return os.path.join(host_install_prefix, '')

    @property
    def _is_cross_tools_host(self):
        return self.__host.name in self.__args.cross_compile_hosts

    @property
    def _should_skip_local_host_install(self):
        return self.__args.skip_local_host_install and \
                self.__has_cross_compile_hosts and \
                self.__host.name == self.__args.host_target

    @property
    def __has_cross_compile_hosts(self):
        return len(self.__args.cross_compile_hosts) > 0

    @property
    def __should_include_host_in_lipo(self):
        # When building cross-compilers for these hosts, merge all of their
        # contents together with lipo
        return self.__has_cross_compile_hosts and \
            not self.__args.skip_merge_lipo_cross_compile_tools and \
            self.__host.platform.name in ['iphoneos', 'iphonesimulator',
                                          'appletvos', 'appletvsimulator'
                                          'watchos', 'watchossimulator']


class CMakeProductBuilder(BuilderHostInstallMixin):
    def __init__(self, product_class, args, toolchain, workspace, host):
        BuilderHostInstallMixin.__init__(self, args, host)
        self._source_dir = workspace.source_dir(product_class.product_source_name())
        self._build_dir = workspace.build_dir(host.name, product_class.product_name())
        self._product = product_class(args, toolchain, self._source_dir, self._build_dir)
        self._args = args
        self._host = host
        self._toolchain = toolchain
        self._workspace = workspace
        self._cmake_options = cmake.CMakeOptions() + self._product.cmake_options

        if self._is_cross_tools_host:
            if self._args.cross_compile_with_host_tools:
                # Optionally use the freshly-built host copy of clang to build
                # for foreign hosts.
                llvm_build_dir = self._workspace.build_dir(
                    self._args.host_target, 'llvm')
                toolchain.cc = os.path.join(llvm_build_dir, 'bin', 'clang')
                toolchain.cxx = os.path.join(llvm_build_dir, 'bin', 'clang++')
            # CMake can't relink when using Ninja, but that's okay -
            # we don't need a build-local rpath because we can't run
            # cross-compiled products
            if self._args.cmake_generator == 'Ninja':
                self._cmake_options.define('CMAKE_BUILD_WITH_INSTALL_RPATH', 1)

    def do_build(self):
        cmake_invocation = cmake.CMake(self._args, self._toolchain)
        cmake_invocation.generate_if_needed(self._source_dir,
                                            self._build_dir,
                                            self._cmake_options)
        if self._should_build:
            cmake_invocation.build_targets(self._build_dir,
                                           self._build_variant,
                                           self._build_targets)

    def do_test(self, force_regenerate=False):
        if self._should_test:
            product_name = self._product.name
            host_name = self._host.name
            cmake_invocation = cmake.CMake(self._args, self._toolchain)
            if force_regenerate:
                cmake_invocation.generate(
                    self._source_dir, self._build_dir, self._cmake_options)
            if self._test_executable_target:
                print("--- Building tests for {} ---".format(product_name))
                cmake_invocation.build_targets(self._build_dir,
                                               self._build_variant,
                                               [self._test_executable_target])
            if self._is_cross_tools_host:
                print("--- Can't execute tests for {}, skipping... ---".format(
                    host_name))
                return
            print("--- Running tests for {} ---".format(product_name))
            for target in self._test_results_targets:
                print("--- {} ---".format(target))
                cmake_invocation.build_targets(self._build_dir,
                                               self._build_variant,
                                               [target])
                print("-- {} finished --".format(target))
            print("--- Finished tests for {} ---".format(product_name))

    def do_install(self):
        if self._should_install:
            host_install_destdir = self._host_install_destdir
            install_targets = ["install"]
            print('--- Installing {} ---'.format(self._product.name))
            cmake_invocation = cmake.CMake(self._args, self._toolchain)
            cmake_invocation.install_targets(self._build_dir,
                                             install_targets,
                                             {'DESTDIR': host_install_destdir})

    @property
    def _build_targets(self):
        return ['all']

    @property
    def _build_variant(self):
        raise ValueError("_build_variant should be overriden by subclasses: {}".format(self))

    @property
    def _common_cross_cflags(self):
        # In Windows, we build LLVM using cl.exe initially, so we might need to
        # use different CFLAGS.
        is_msvc = os.path.basename(self._toolchain.cc) == 'cl.exe'

        cflags = ' -Wno-unknown-warning-option' if not is_msvc else ''
        cflags += ' -Werror=unguarded-availability-new' if not is_msvc else ''

        host_cflags = {
            'iphonesimulator-i386': " -arch i386 -mios-simulator-version-min={}".format(self._args.darwin_deployment_version_ios),
            'iphonesimulator-x86_64': " -arch x86_64 -mios-simulator-version-min={}".format(self._args.darwin_deployment_version_ios),
            'iphoneos-armv7': " -arch armv7 -miphoneos-version-min={}".format(self._args.darwin_deployment_version_ios),
            'iphoneos-armv7s': " -arch armv7s -miphoneos-version-min={}".format(self._args.darwin_deployment_version_ios),
            'iphoneos-arm64': " -arch arm64 -miphoneos-version-min={}".format(self._args.darwin_deployment_version_ios),
            'appletvsimulator-x86_64': " -arch x86_64 -mtvos-simulator-version-min={}".format(self._args.darwin_deployment_version_tvos),
            'appletvos-arm64': " -arch arm64 -mtvos-version-min={}".format(self._args.darwin_deployment_version_tvos),
            'watchsimulator-i386': " -arch i386 -mwatchos-simulator-version-min={}".format(self._args.darwin_deployment_version_watchos),
            'watchos-armv7k': " -arch armv7k -mwatchos-version-min={}".format(self._args.darwin_deployment_version_watchos),
            'android-armv7': " -arch armv7",
            'android-arm64': " -arch aarch64",
        }

        # MSVC doesn't support multiple archs and probably will not support
        # iOS/tvOS/watchOS versions at all.
        if not is_msvc and self._host.name in host_cflags:
            cflags += host_cflags[self._host.name]

        return cflags

    @property
    def _is_debinfo_build_variant(self):
        return self._build_variant in ['Debug', 'RelWithDebInfo']

    @property
    def _is_release_build_variant(self):
        return self._build_variant in ['Release', 'RelWithDebInfo']

    @property
    def _should_build(self):
        return not self._args.skip_build

    @property
    def _should_test(self):
        return self._args.test

    @property
    def _should_install(self):
        return False and self._should_skip_local_host_install

    @property
    def _swift_host_triple(self):
        # For cross-compilable host, we need to know the triple
        # and it must be the same for both LLVM and Swift
        swift_host_triples = {
            'linux-armv6': 'armv6-unknown-linux-gnueabihf',
            'linux-armv7': 'armv7-unknown-linux-gnueabihf',
            'macosx-x86_64': 'x86_64-apple-macos{}'.format(self._args.darwin_deployment_version_osx),
            'iphonesimulator-i386': 'i386-apple-ios{}'.format(self._args.darwin_deployment_version_ios),
            'iphonesimulator-x86_64': 'x86_64-apple-ios{}'.format(self._args.darwin_deployment_version_ios),
            'iphoneos-armv7': 'armv7-apple-ios{}'.format(self._args.darwin_deployment_version_ios),
            'iphoneos-armv7s': 'armv7s-apple-ios{}'.format(self._args.darwin_deployment_version_ios),
            'iphoneos-arm64': 'arm64-apple-ios{}'.format(self._args.darwin_deployment_version_ios),
            'appletvsimulator-x86_64': 'x86_64-apple-tvos{}'.format(self._args.darwin_deployment_version_tvos),
            'appletvos-arm64': 'arm64-apple-tvos{}'.format(self._args.darwin_deployment_version_tvos),
            'watchsimulator-i386': 'i386-apple-watchos{}'.format(self._args.darwin_deployment_version_watchos),
            'watchos-armv7k': 'armv7k-apple-watchos{}'.format(self._args.darwin_deployment_version_watchos),
        }

        if self._host.name in swift_host_triples:
            return swift_host_triples[self._host.name]
        else:
            return None

    @property
    def _test_executable_target(self):
        return None

    @property
    def _test_results_targets(self):
        return []
