# swift_build_support/products/swift.py -------------------------*- python -*-
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

from . import (cmark, libdispatch, libicu, lldb, llvm, product)
from ..host_specific_configuration import HostSpecificConfiguration

import re
import os
import os.path
import platform


class Swift(product.Product):

    @classmethod
    def builder_class(cls):
        return SwiftBuilder

    def __init__(self, args, toolchain, source_dir, build_dir):
        product.Product.__init__(self, args, toolchain, source_dir,
                                 build_dir)
        # Add any runtime sanitizer arguments.
        self.cmake_options += self._runtime_sanitizer_flags

        # Add any compiler vendor cmake flags.
        self.cmake_options += self._compiler_vendor_flags

        # Add any swift version related cmake flags.
        self.cmake_options += self._version_flags

        # Add benchmark specific flags.
        self.cmake_options += self._benchmark_flags

        # Add any sil ownership flags.
        self.cmake_options += self._sil_ownership_flags

        # Generate the compile db.
        self.cmake_options += self._compile_db_flags

        # Add the flag if we are supposed to force the typechecker to compile
        # with optimization.
        self.cmake_options += self._force_optimized_typechecker_flags

        # Add any exclusivity checking flags for stdlibcore.
        self.cmake_options += self._stdlibcore_exclusivity_checking_flags

    @property
    def _runtime_sanitizer_flags(self):
        sanitizer_list = []
        if self.args.enable_tsan_runtime:
            sanitizer_list += ['Thread']
        if len(sanitizer_list) == 0:
            return []
        return ["-DSWIFT_RUNTIME_USE_SANITIZERS=%s" %
                ";".join(sanitizer_list)]

    @property
    def _compiler_vendor_flags(self):
        if self.args.compiler_vendor == "none":
            return []

        if self.args.compiler_vendor != "apple":
            raise RuntimeError("Unknown compiler vendor?! Was build-script \
updated without updating swift.py?")

        swift_compiler_version = ""
        if self.args.swift_compiler_version is not None:
            swift_compiler_version = self.args.swift_compiler_version

        return [
            "-DSWIFT_VENDOR=Apple",
            "-DSWIFT_VENDOR_UTI=com.apple.compilers.llvm.swift",

            # This has a default of 3.0, so it should be safe to use here.
            "-DSWIFT_VERSION={}".format(self.args.swift_user_visible_version),

            # FIXME: We are matching build-script-impl here. But it seems like
            # bit rot since this flag is specified in another place with the
            # exact same value in build-script-impl.
            "-DSWIFT_COMPILER_VERSION={}".format(swift_compiler_version),
        ]

    @property
    def _version_flags(self):
        r = []
        if self.args.swift_compiler_version is not None:
            swift_compiler_version = self.args.swift_compiler_version
            r.append(
                "-DSWIFT_COMPILER_VERSION={}".format(swift_compiler_version)
            )
        if self.args.clang_compiler_version is not None:
            clang_compiler_version = self.args.clang_compiler_version
            r.append(
                "-DCLANG_COMPILER_VERSION={}".format(clang_compiler_version)
            )
        return r

    @property
    def _benchmark_flags(self):
        if not self.args.benchmark:
            return []

        onone_iters = self.args.benchmark_num_onone_iterations
        o_iters = self.args.benchmark_num_o_iterations
        return [
            "-DSWIFT_BENCHMARK_NUM_ONONE_ITERATIONS={}".format(onone_iters),
            "-DSWIFT_BENCHMARK_NUM_O_ITERATIONS={}".format(o_iters)
        ]

    @property
    def _sil_ownership_flags(self):
        if not self.args.enable_sil_ownership:
            return ["-DSWIFT_STDLIB_ENABLE_SIL_OWNERSHIP=FALSE"]
        return ["-DSWIFT_STDLIB_ENABLE_SIL_OWNERSHIP=TRUE"]

    @property
    def _compile_db_flags(self):
        return ['-DCMAKE_EXPORT_COMPILE_COMMANDS=TRUE']

    @property
    def _force_optimized_typechecker_flags(self):
        if not self.args.force_optimized_typechecker:
            return ['-DSWIFT_FORCE_OPTIMIZED_TYPECHECKER=FALSE']
        return ['-DSWIFT_FORCE_OPTIMIZED_TYPECHECKER=TRUE']

    @property
    def _stdlibcore_exclusivity_checking_flags(self):
        # This is just to get around 80 column limitations.
        result = '-DSWIFT_STDLIB_ENABLE_STDLIBCORE_EXCLUSIVITY_CHECKING={}'
        if not self.args.enable_stdlibcore_exclusivity_checking:
            return [result.format("FALSE")]
        return [result.format("TRUE")]


class SwiftHostVariantTripleMixin(object):
    def __init__(self, host):
        self.__host = host

    @property
    def _swift_host_variant_triple(self):
        triples = {
            'freebsd-x86_64': ('freebsd', 'FREEBSD', 'x86_64'),
            'cygwin-x86_64': ('cygwin', 'CYGWIN', 'x86_64'),
            'haiku-x86_64': ('haiku', 'HAIKU', 'x86_64'),
            'linux-x86_64': ('linux', 'LINUX', 'x86_64'),
            'linux-i686': ('linux', 'LINUX', 'i686'),
            'linux-armv6': ('linux', 'LINUX', 'armv6'),
            'linux-armv7': ('linux', 'LINUX', 'armv7'),
            'linux-aarch64': ('linux', 'LINUX', 'aarch64'),
            'linux-powerpc64': ('linux', 'LINUX', 'powerpc64'),
            'linux-powerpc64le': ('linux', 'LINUX', 'powerpc64le'),
            'linux-s390x': ('linux', 'LINUX', 's390x'),
            'macosx-x86_64': ('macosx', 'OSX', 'x86_64'),
            'iphonesimulator-i386':
                ('iphonesimulator', 'IOS_SIMULATOR', 'i386'),
            'iphonesimulator-x86_64':
                ('iphonesimulator', 'IOS_SIMULATOR', 'x86_64'),
            'iphoneos-armv7': ('iphoneos', 'IOS', 'armv7'),
            'iphoneos-armv7s': ('iphoneos', 'IOS', 'armv7s'),
            'iphoneos-arm64': ('iphoneos', 'IOS', 'arm64'),
            'appletvsimulator-x86_64':
                ('appletvsimulator', 'TVOS_SIMULATOR', 'x86_64'),
            'appletv-arm64': ('appletv', 'TVOS', 'arm64'),
            'watchsimulator-i386':
                ('watchsimulator', 'WATCHOS_SIMULATOR', 'i386'),
            'watchos-armv7k': ('watchos', 'WATCHOS', 'armv7k'),
            'windows-x86_64': ('windows', 'WINDOWS', 'x86_64'),
        }

        if self.__host.name in triples:
            return triples[self.__host.name]
        else:
            return None


class SwiftBuilder(llvm.LLVMBase, SwiftHostVariantTripleMixin):
    def __init__(self, product_class, args, toolchain, workspace, host):
        product.CMakeProductBuilder.__init__(self, product_class, args,
                                             toolchain, workspace, host)
        SwiftHostVariantTripleMixin.__init__(self, host)

        self.__host_config = HostSpecificConfiguration(host.name, args)

        # Switch to the just build LLVM/Clang for compiling Swift and the rest
        # of the products.
        if platform.system() == 'Windows':
            clang_cl = os.path.join(
                self._workspace.build_dir(
                    self._args.host_target, llvm.LLVM.product_name()),
                'bin', 'clang-cl.exe')
            self._toolchain.cc = clang_cl
            self._toolchain.cxx = clang_cl

        if self._args.build_android:
            self._cmake_options.define(
                'SWIFT_ANDROID_NDK_PATH:STRING', self._args.android_ndk)
            self._cmake_options.define(
                'SWIFT_ANDROID_NDK_GCC_VERSION:STRING',
                self._args.android_ndk_gcc_version)
            self._cmake_options.define(
                'SWIFT_ANDROID_API_LEVEL:STRING',
                self._args.android_api_level)
            self.__define_icu_cmake_options(
                'ANDROID', self._args.android_arch,
                icu_uc_path=self._args.icu_uc_path,
                icu_uc_include_path=self._args.icu_uc_include_path,
                icu_i18n_path=self._args.icu_i18n_path,
                icu_i18n_include_path=self._args.icu_i18n_include_path,
                icu_data_path=self._args.icu_data_path)
            self._cmake_options.define(
                'SWIFT_ANDROID_DEPLOY_DEVICE_PATH:STRING',
                self._args.android_deploy_device_path)
            self._cmake_options.define(
                'SWIFT_SDK_ANDROID_ARCHITECTURES:STRING',
                self._args.android_arch)

        if self._host.platform.name == 'windows':
            self.__define_icu_cmake_options(
                'WINDOWS', self._host.arch,
                icu_uc_path=self._args.icu_uc_path,
                icu_uc_include_path=self._args.icu_uc_include_path,
                icu_i18n_path=self._args.icu_i18n_path,
                icu_i18n_include_path=self._args.icu_i18n_include_path,
                icu_data_path=self._args.icu_data_path)
            self._cmake_options.define('SWIFT_INCLUDE_DOCS:BOOL', False)
            self._cmake_options.define(
                'CMAKE_EXE_LINKER_FLAGS:STRING', '/INCREMENTAL:NO')
            self._cmake_options.define(
                'CMAKE_SHARED_LINKER_FLAGS:STRING', '/INCREMENTAL:NO')

        # TODO: build-script-impl defines --darwin-overlay-target
        # if self._args.darwin_overlay_target != "":
        #     host_target = targets.StdlibDeploymentTarget.get_target_for_name(
        #         self._args.host_target)
        #     # TODO: find-overlay-closure.sh needs to be converted into Python
        #     overlay_target_closure = shell.capture(
        #         [os.path.join(
        #             self._source_dir, 'utils',
        #             'find-overlay-deps-closure.sh'),
        #          self._args.darwin_overlay_target, host_target.arch,
        #          host_target.platform.name])
        #     self._cmake_options.define(
        #         'SWIFT_OVERLAY_TARGETS:STRING', overlay_target_closure)

        native_llvm_tools_path = None
        native_clang_tools_path = None
        native_swift_tools_path = None
        if self._is_cross_tools_host:
            # Don't build benchmarks and tests when building cross compiler.
            self.__build_perf_testsuite_this_time = False
            build_external_perf_testsuite_this_time = False
            build_tests_this_time = False

            native_llvm_tools_path = os.path.join(
                self._workspace.build_dir(
                    self._args.host_target, llvm.LLVM.product_name()),
                'bin')
            native_clang_tools_path = os.path.join(
                self._workspace.build_dir(
                    self._args.host_target, llvm.LLVM.product_name()),
                'bin')
            native_swift_tools_path = os.path.join(
                self._workspace.build_dir(
                    self._args.host_target, Swift.product_name()),
                'bin')
        else:
            self.__build_perf_testsuite_this_time = self._args.build_benchmarks
            build_external_perf_testsuite_this_time = \
                self._args.build_external_benchmarks
            build_tests_this_time = self._args.swift_include_tests
            build_tests_this_time = True

        # Command-line parameters override any autodetection that we
        # might have done.
        # TODO: build-script-impl defines --native-llvm-tools-path, ...
        # if self._args.native_llvm_tools_path:
        #     native_llvm_tools_path = self._args.native_llvm_tools_path
        # if self._args.native_clang_tools_path:
        #     native_clang_tools_path = self._args.native_clang_tools_path
        # if self._args.native_swift_tools_path:
        #     native_swift_tools_path = self._args.native_swift_tools_path

        if self._args.clean_llvm:
            self._cmake_options.define(
                'LLVM_TOOLS_BINARY_DIR:PATH',
                os.path.join(os.path.sep, 'tmp', 'dummy'))

        if self._toolchain.lipo:
            self._cmake_options.define('SWIFT_LIPO:PATH', self._toolchain.lipo)

        if 'SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS' not in os.environ:
            swift_enable_runtime_function_counters = \
                self._args.swift_stdlib_assertions
        else:
            swift_enable_runtime_function_counters = (
                os.environ['SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS'].lower() ==
                'true')

        self._cmake_options.define('CMAKE_C_FLAGS', self.__cflags)
        self._cmake_options.define('CMAKE_CXX_FLAGS', self.__cflags)
        self._cmake_options.define(
            'CMAKE_C_FLAGS_RELWITHDEBINFO', '-O2 -DNDEBUG')
        self._cmake_options.define(
            'CMAKE_CXX_FLAGS_RELWITHDEBINFO', '-O2 -DNDEBUG')
        self._cmake_options.define(
            'CMAKE_BUILD_TYPE:STRING', self._build_variant)
        self._cmake_options.define(
            'LLVM_ENABLE_ASSERTIONS:BOOL', self._args.swift_assertions)
        self._cmake_options.define(
            'SWIFT_ANALYZE_CODE_COVERAGE:STRING',
            self._args.swift_analyze_code_coverage.upper())
        self._cmake_options.define(
            'SWIFT_STDLIB_BUILD_TYPE:STRING',
            self._args.swift_stdlib_build_variant)
        self._cmake_options.define(
            'SWIFT_STDLIB_ASSERTIONS:BOOL', self._args.swift_stdlib_assertions)
        self._cmake_options.define(
            'SWIFT_STDLIB_USE_NONATOMIC_RC:BOOL',
            self._args.swift_stdlib_use_nonatomic_rc)
        self._cmake_options.define(
            'SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS:BOOL',
            swift_enable_runtime_function_counters)
        self._cmake_options.define(
            'SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING', native_llvm_tools_path)
        self._cmake_options.define(
            'SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING', native_clang_tools_path)
        self._cmake_options.define(
            'SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING', native_swift_tools_path)
        self._cmake_options.define(
            'SWIFT_INCLUDE_TOOLS:BOOL', self._args.build_swift_tools)
        # TODO: build-script-impl defines --build-swift-remote-mirror
        self._cmake_options.define('SWIFT_BUILD_REMOTE_MIRROR:BOOL', True)
        self._cmake_options.define(
            'SWIFT_STDLIB_SIL_DEBUGGING:BOOL',
            self._args.build_sil_debugging_stdlib)
        self._cmake_options.define(
            'SWIFT_CHECK_INCREMENTAL_COMPILATION:BOOL',
            self._args.check_incremental_compilation)
        # TODO: build-script-impl defines --report-statistics
        self._cmake_options.define('SWIFT_REPORT_STATISTICS:BOOL', False)
        self._cmake_options.define(
            'SWIFT_BUILD_DYNAMIC_STDLIB:BOOL',
            self._args.build_swift_dynamic_stdlib)
        self._cmake_options.define(
            'SWIFT_BUILD_STATIC_STDLIB:BOOL',
            self._args.build_swift_static_stdlib)
        self._cmake_options.define(
            'SWIFT_BUILD_DYNAMIC_SDK_OVERLAY:BOOL',
            self._args.build_swift_dynamic_sdk_overlay)
        self._cmake_options.define(
            'SWIFT_BUILD_STATIC_SDK_OVERLAY:BOOL',
            self._args.build_swift_static_stdlib)
        self._cmake_options.define(
            'SWIFT_BUILD_PERF_TESTSUITE:BOOL',
            self.__build_perf_testsuite_this_time)
        self._cmake_options.define(
            'SWIFT_BUILD_EXTERNAL_PERF_TESTSUITE:BOOL',
            build_external_perf_testsuite_this_time)
        self._cmake_options.define(
            'SWIFT_BUILD_EXAMPLES:BOOL', self._args.build_swift_examples)
        self._cmake_options.define(
            'SWIFT_INCLUDE_TESTS:BOOL', build_tests_this_time)
        # TODO: build-script-impl defines --embed-bitcode-section but nobody
        # uses it
        self._cmake_options.define('SWIFT_EMBED_BITCODE_SECTION:BOOL', False)
        self._cmake_options.define(
            'SWIFT_TOOLS_ENABLE_LTO:STRING', self._args.lto_type)
        self._cmake_options.define(
            'SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL',
            self._args.build_runtime_with_host_compiler)
        self._cmake_options.define(
            'LIBDISPATCH_CMAKE_BUILD_TYPE:STRING',
            self._args.libdispatch_build_variant)

        if self._host.platform.is_darwin:
            if self._is_swift_lto_enabled:
                if self._cmake_needs_to_specify_standard_computed_defaults:
                    self._cmake_options.define(
                        'CMAKE_C_STANDARD_COMPUTED_DEFAULT', 'AppleClang')
                    self._cmake_options.define(
                        'CMAKE_CXX_STANDARD_COMPUTED_DEFAULT', 'AppleClang')
                self._cmake_options.define(
                    'LLVM_ENABLE_MODULE_DEBUGGING:BOOL', False)
                self._cmake_options.define(
                    'SWIFT_PARALLEL_LINK_JOBS',
                    min(self._args.swift_tools_max_parallel_lto_link_jobs,
                        self._args.build_jobs))
            self._cmake_options.define(
                'SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX',
                self._args.darwin_deployment_version_osx)
            self._cmake_options.define(
                'SWIFT_DARWIN_DEPLOYMENT_VERSION_IOS',
                self._args.darwin_deployment_version_ios)
            self._cmake_options.define(
                'SWIFT_DARWIN_DEPLOYMENT_VERSION_TVOS',
                self._args.darwin_deployment_version_tvos)
            self._cmake_options.define(
                'SWIFT_DARWIN_DEPLOYMENT_VERSION_WATCHOS',
                self._args.darwin_deployment_version_watchos)

        host_triple = self._swift_host_triple
        if host_triple:
            self._cmake_options.define('SWIFT_HOST_TRIPLE:STRING', host_triple)

        (swift_host_variant,
            swift_host_variant_sdk,
            swift_host_variant_arch) = self._swift_host_variant_triple

        self._cmake_options.define('SWIFT_HOST_VARIANT', swift_host_variant)
        self._cmake_options.define(
            'SWIFT_HOST_VARIANT_SDK', swift_host_variant_sdk)
        self._cmake_options.define(
            'SWIFT_HOST_VARIANT_ARCH', swift_host_variant_arch)

        if self._args.lit_args:
            self._cmake_options.define('LLVM_LIT_ARGS', self._args.lit_args)

        # TODO: build-script-impl defines --swift-profile-instr-use but nobody
        # uses it
        # if self._args.swift_profile_instr_use:
        #     self._cmake_options.define(
        #         'SWIFT_PROFDATA_FILE', self._args.swift_profile_instr_use)

        if args.coverage_db:
            self._cmake_options.define(
                'COVERAGE_DB', os.path.abspath(self._args.coverage_db))

        if self._args.darwin_toolchain_version:
            self._cmake_options.define(
                'DARWIN_TOOLCHAIN_VERSION',
                self._args.darwin_toolchain_version)

        if self._args.enable_asan or platform.system() == 'Linux':
            self._cmake_options.define(
                'SWIFT_SOURCEKIT_USE_INPROC_LIBRARY:BOOL', True)

        if self._args.darwin_crash_reporter_client:
            self._cmake_options.define(
                'SWIFT_RUNTIME_CRASH_REPORTER_CLIENT:BOOL', True)

        self._cmake_options.define(
            'SWIFT_DARWIN_XCRUN_TOOLCHAIN:STRING',
            self._args.darwin_xcrun_toolchain)

        # TODO: build-script-impl defines --darwin-stdlib-install-name-dir but
        # it is not used
        # if self._args.darwin_stdlib_install_name_dir:
        #     self._cmake_options.define(
        #         'SWIFT_DARWIN_STDLIB_INSTALL_NAME_DIR:STRING',
        #         self._args.darwin_stdlib_install_name_dir)

        if self._args.extra_swift_args:
            self._cmake_options.define(
                'SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS',
                self._args.extra_swift_args)

        self._cmake_options.define(
            'SWIFT_AST_VERIFIER:BOOL', self._args.swift_enable_ast_verifier)
        self._cmake_options.define(
            'SWIFT_SIL_VERIFY_ALL:BOOL', self._args.sil_verify_all)
        self._cmake_options.define(
            'SWIFT_RUNTIME_ENABLE_LEAK_CHECKER:BOOL',
            self._args.swift_runtime_enable_leak_checker)

        if self._args.test_paths:
            test_paths = [os.path.join(self._build_dir,
                                       re.sub(r'^(validation-test|test)',
                                              r'\1-{}'.format(self._host.name),
                                              path))
                          for path in self._args.test_paths]
            self._cmake_options.define(
                'SWIFT_LIT_TEST_PATHS', ";".join(test_paths))

        # TODO: build-script-impl defines --skip-test-sourcekit
        # if self._args.skip_test_sourcekit:
        #     self._cmake_options.define(
        #         'SWIFT_ENABLE_SOURCEKIT_TESTS:BOOL', False)

        if self._args.build_toolchain_only:
            self._cmake_options.define('SWIFT_TOOL_SIL_OPT_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SWIFT_IDE_TEST_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SWIFT_REMOTEAST_TEST_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_LLDB_MODULEIMPORT_TEST_BUILD', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SIL_EXTRACT_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SWIFT_LLVM_OPT_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SWIFT_SDK_ANALYZER_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SWIFT_SDK_DIGESTER_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SOURCEKITD_TEST_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SOURCEKITD_REPL_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_COMPLETE_TEST_BUILD:BOOL', False)
            self._cmake_options.define(
                'SWIFT_TOOL_SWIFT_REFLECTION_DUMP_BUILD:BOOL', False)

        llvm_build_dir = self._workspace.build_dir(
            self._host.name, llvm.LLVM.product_name())

        self._cmake_options.define(
            'CMAKE_INSTALL_PREFIX:PATH', self._host_install_prefix)
        self._cmake_options.define(
            'Clang_DIR:PATH',
            os.path.join(llvm_build_dir, 'lib', 'cmake', 'clang'))
        self._cmake_options.define(
            'LLVM_DIR:PATH',
            os.path.join(llvm_build_dir, 'lib', 'cmake', 'llvm'))
        # FIXME: Clang doesn't have a product because it builds from LLVM, but
        # having the raw source directory name there is wrong.
        self._cmake_options.define(
            'SWIFT_PATH_TO_CMARK_SOURCE:PATH',
            self._workspace.source_dir(cmark.CMark.product_source_name()))
        self._cmake_options.define(
            'SWIFT_PATH_TO_CMARK_BUILD:PATH',
            self._workspace.build_dir(
                self._host.name, cmark.CMark.product_name()))
        self._cmake_options.define(
            'SWIFT_PATH_TO_LIBDISPATCH_SOURCE:PATH',
            self._workspace.source_dir(
                libdispatch.LibDispatch.product_source_name()))

        if self._args.build_libicu:
            libicu_build_dir = self._workspace.build_dir(
                self._host.name, libicu.LibICU.product_name())
            icu_tmpinstall = os.path.join(libicu_build_dir, 'tmp_install')
            self._cmake_options.define(
                'SWIFT_PATH_TO_LIBICU_SOURCE:PATH',
                self._workspace.source_dir(
                    libicu.LibICU.product_source_name()))
            self._cmake_options.define(
                'SWIFT_PATH_TO_LIBICU_BUILD:PATH',
                libicu_build_dir)
            self.__define_icu_cmake_options(
                swift_host_variant_sdk, swift_host_variant_arch,
                icu_uc_include_path=os.path.join(icu_tmpinstall, 'include'),
                icu_i18n_include_path=os.path.join(icu_tmpinstall, 'include'))
            self._cmake_options.define(
                'SWIFT_{}_{}_ICU_STATICLIB:BOOL'.format(
                    swift_host_variant_sdk, swift_host_variant_arch),
                True)

        if self._args.cmake_generator == 'Xcode':
            self._cmake_options.define(
                'SWIFT_CMARK_LIBRARY_DIR:PATH',
                os.path.join(
                    self._workspace.build_dir(
                        self._host.name, cmark.CMark.product_name()),
                    'src', self._args.cmark_build_variant))
        else:
            self._cmake_options.define(
                'SWIFT_CMARK_LIBRARY_DIR:PATH',
                os.path.join(
                    self._workspace.build_dir(
                        self._host.name, cmark.CMark.product_name()),
                    'src'))

        swift_sdks = self.__host_config.sdks_to_configure
        if swift_sdks:
            self._cmake_options.define(
                'SWIFT_SDKS:STRING', ";".join(swift_sdks))

        if self._args.swift_primary_variant_sdk:
            self._cmake_options.define(
                'SWIFT_PRIMARY_VARIANT_SDK:STRING',
                self._args.swift_primary_variant_sdk)
            self._cmake_options.define(
                'SWIFT_PRIMARY_VARIANT_ARCH:STRING',
                self._args.swift_primary_variant_arch)

        if self._args.swift_install_components:
            self._cmake_options.define(
                'SWIFT_INSTALL_COMPONENTS:STRING',
                self._args.swift_install_components)

        if self._args.build_lldb:
            lldb_build_dir = self._workspace.build_dir(
                self._host.name, lldb.LLDB.product_name())
            self._cmake_options.define('LLDB_ENABLE:BOOL', True)
            self._cmake_options.define('LLDB_BUILD_DIR:STRING', lldb_build_dir)

        if self.__build_perf_testsuite_this_time:
            native_swift_tools_path = os.path.join(
                self._workspace.build_dir(
                    self._args.host_target, Swift.product_name()),
                'bin')
            self._cmake_options.define(
                'SWIFT_EXEC:STRING',
                os.path.join(native_swift_tools_path, 'swiftc'))

        if self._args.build_libparser_only:
            # TODO: build-script-impl defines --libparser-ver
            # self._cmake_options.define(
            #     'SWIFT_LIBPARSER_VER:STRING', self._args.libparser_ver)
            pass

    @property
    def _build_targets(self):
        if self._args.build_libparser_only:
            return ['libSwiftSyntaxParser']

        build_targets = ['all'] + self.__host_config.swift_stdlib_build_targets
        if self.__build_perf_testsuite_this_time:
            build_targets += self.__host_config.swift_benchmark_build_targets
            pass
        return build_targets

    @property
    def _build_variant(self):
        return self._args.swift_build_variant

    @property
    def _should_test(self):
        return self._args.test or \
            self._args.long_test or \
            self._args.stress_test

    @property
    def _test_executable_target(self):
        return 'SwiftUnitTests'

    @property
    def _test_results_targets(self):
        test_targets = self.__host_config.swift_test_run_targets
        # TODO: build-script-impl defines --stress-test-sourcekit
        # if self._args.stress_test_sourcekit:
        #     targets.append('stress-SourceKit')
        if not self._args.benchmark:
            test_targets += self.__host_config.swift_benchmark_run_targets
        if self._args.test_paths:
            test_targets = ["{}-custom".format(t)
                            if t.startswith('check-swift') else t
                            for t in test_targets]
        return test_targets

    @property
    def __cflags(self):
        # Don't pass common_cross_c_flags to Swift because CMake code in the
        # Swift project is itself aware of cross-compilation for the host tools
        # and standard library.
        # No need to for equivalents in MSVC, before compiling Swift, we will
        # switch to clang-cl.exe.
        cflags = ' -Wno-unknown-warning-option' \
                 ' -Werror=unguarded-availability-new'

        is_clang_cl = os.path.basename(self._toolchain.cc) == 'clang-cl.exe'

        if self._is_release_build_variant:
            cflags += ' -fno-stack-protector' if not is_clang_cl else ' /GS-'
        if self._args.swift_stdlib_use_nonatomic_rc:
            cflags += ' -DSWIFT_STDLIB_USE_NONATOMIC_RC'
        if self._host.platform.name == 'windows':
            cflags += ' -Wno-c++98-compat -Wno-c++98-compat-pedantic'
        return cflags

    def __define_icu_cmake_options(self, sdk, arch,
                                   icu_uc_path=None, icu_uc_include_path=None,
                                   icu_i18n_path=None,
                                   icu_i18n_include_path=None,
                                   icu_data_path=None):
        if icu_uc_path is not None:
            self._cmake_options.define(
                'SWIFT_{}_{}_ICU_UC:STRING'.format(sdk, arch), icu_uc_path)
        if icu_uc_include_path:
            self._cmake_options.define(
                'SWIFT_{}_{}_ICU_UC_INCLUDE:STRING'.format(sdk, arch),
                icu_uc_include_path)
        if icu_i18n_path:
            self._cmake_options.define(
                'SWIFT_{}_{}_ICU_I18N:STRING'.format(sdk, arch), icu_i18n_path)
        if icu_i18n_include_path:
            self._cmake_options.define(
                'SWIFT_{}_{}_ICU_I18N_INCLUDE:STRING'.format(sdk, arch),
                icu_i18n_include_path)
        if icu_data_path:
            self._cmake_options.define(
                'SWIFT_{}_{}_ICU_DATA:STRING'.format(sdk, arch), icu_data_path)
