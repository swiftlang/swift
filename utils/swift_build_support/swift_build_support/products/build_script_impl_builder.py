# swift_build_support/product_builders/build_script_impl_buil... -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

from . import product_builder
from .. import diagnostics, targets, shell
from ..cmake import CMake
from ..host_specific_configuration import HostSpecificConfiguration
from ..SwiftBuildSupport import (
    SWIFT_REPO_NAME,
    SWIFT_SOURCE_ROOT,
)

import os
import os.path
import pipes
import platform


BUILD_SCRIPT_IMPL_PATH = os.path.join(
    SWIFT_SOURCE_ROOT, SWIFT_REPO_NAME, "utils", "build-script-impl")


class BuildScriptImplBuilder(product_builder.ProductBuilder):
    def __init__(self, product_class, args, toolchain, workspace, host):
        self.__product_class = product_class
        self.__helper = BuildScriptImplHelper(args, toolchain, workspace,
                                              [product_class])
        self.__host = host

    def do_build(self):
        self.__helper.execute(self.__action_name('build'))

    def do_test(self):
        self.__helper.execute(self.__action_name('test'))

    def do_install(self):
        self.__helper.execute(self.__action_name('install'))

    def __action_name(self, action_name):
        return '{}-{}-{}'.format(self.__host.name,
                                 self.__product_class.product_name(),
                                 action_name)


class BuildScriptImplHelper(object):
    def __init__(self, args, toolchain, workspace, product_classes):
        self.__args = args
        self.__toolchain = toolchain
        self.__workspace = workspace
        self.__product_classes = product_classes

    def do_extract_symbols(self, host):
        self.execute('{}-extractsymbols'.format(host.name))

    def do_package(self, host):
        self.execute('{}-package'.format(host.name))

    def do_lipo(self):
        self.execute('merged-hosts-lipo')

    def execute(self, action=None, echo=None):
        (impl_env, impl_args) = self.__convert_to_impl_arguments()
        command = [BUILD_SCRIPT_IMPL_PATH] + impl_args
        if action is not None:
            command += ['--only-execute', action]
        if echo is None:
            echo = self.__args.verbose_build
        shell.call_without_sleeping(command, env=impl_env, echo=echo)

    def __convert_to_impl_arguments(self):
        """convert_to_impl_arguments() -> (env, args)

        Convert the invocation to an environment and list of arguments suitable
        for invoking `build-script-impl`.
        """

        # Create local shadows, for convenience.
        args = self.__args
        toolchain = self.__toolchain
        workspace = self.__workspace

        cmake = CMake(args=args, toolchain=toolchain)

        impl_args = [
            "--workspace", workspace.source_root,
            "--build-dir", workspace.build_root,
            "--install-prefix", args.install_prefix,
            "--host-target", args.host_target,
            "--stdlib-deployment-targets",
            " ".join(args.stdlib_deployment_targets),
            "--host-cc", toolchain.cc,
            "--host-cxx", toolchain.cxx,
            "--darwin-xcrun-toolchain", args.darwin_xcrun_toolchain,
            "--darwin-deployment-version-osx=%s" % (
                args.darwin_deployment_version_osx),
            "--darwin-deployment-version-ios=%s" % (
                args.darwin_deployment_version_ios),
            "--darwin-deployment-version-tvos=%s" % (
                args.darwin_deployment_version_tvos),
            "--darwin-deployment-version-watchos=%s" % (
                args.darwin_deployment_version_watchos),
            "--cmake", toolchain.cmake,
            "--cmark-build-type", args.cmark_build_variant,
            "--llvm-build-type", args.llvm_build_variant,
            "--swift-build-type", args.swift_build_variant,
            "--swift-stdlib-build-type", args.swift_stdlib_build_variant,
            "--lldb-build-type", args.lldb_build_variant,
            "--lldb-build-with-xcode", args.lldb_build_with_xcode,
            "--foundation-build-type", args.foundation_build_variant,
            "--libdispatch-build-type", args.libdispatch_build_variant,
            "--libicu-build-type", args.libicu_build_variant,
            "--xctest-build-type", args.build_variant,
            "--swiftpm-build-type", args.build_variant,
            "--swiftsyntax-build-type", args.build_variant,
            "--skstresstester-build-type", args.build_variant,
            "--swiftevolve-build-type", args.build_variant,
            "--llbuild-build-type", args.build_variant,
            "--swift-enable-assertions", str(args.swift_assertions).lower(),
            "--swift-stdlib-enable-assertions", str(
                args.swift_stdlib_assertions).lower(),
            "--swift-analyze-code-coverage", str(
                args.swift_analyze_code_coverage).lower(),
            "--llbuild-enable-assertions", str(
                args.llbuild_assertions).lower(),
            "--lldb-assertions", str(
                args.lldb_assertions).lower(),
            "--cmake-generator", args.cmake_generator,
            "--build-jobs", str(args.build_jobs),
            "--common-cmake-options=%s" % ' '.join(
                pipes.quote(opt) for opt in cmake.common_options()),
            "--build-args=%s" % ' '.join(
                pipes.quote(arg) for arg in cmake.build_args()),
        ]

        # Compute any product specific cmake arguments.
        for product_class in self.__product_classes:
            if not product_class.is_build_script_impl_product():
                continue

            product_name = product_class.product_name()
            product_source_name = product_class.product_source_name()
            source_dir = workspace.source_dir(product_source_name)

            if not os.path.exists(source_dir):
                diagnostics.fatal(
                    "can't find source directory for %s "
                    "(tried %s)" % (product_name, source_dir))

            product = product_class(
                args=args,
                toolchain=toolchain,
                source_dir=source_dir,
                # FIXME: This is incorrect since it always assumes the host
                # target I think?
                build_dir=workspace.build_dir(args.host_target, product_name))
            cmake_opts = product.cmake_options

            # FIXME: We should be using pipes.quote here but we run into issues
            # with build-script-impl/cmake not being happy with all of the
            # extra "'" in the strings. To fix this easily, we really need to
            # just invoke cmake from build-script directly rather than futzing
            # with build-script-impl. This makes even more sense since there
            # really isn't a security issue here.
            if cmake_opts:
                impl_args += [
                    "--{}-cmake-options={}".format(
                        product_name, ' '.join(cmake_opts))
                ]

        if args.build_stdlib_deployment_targets:
            impl_args += [
                "--build-stdlib-deployment-targets", " ".join(
                    args.build_stdlib_deployment_targets)]
        if args.cross_compile_hosts:
            impl_args += [
                "--cross-compile-hosts", " ".join(args.cross_compile_hosts)]

        if args.test_paths:
            impl_args += ["--test-paths", " ".join(args.test_paths)]

        if toolchain.ninja:
            impl_args += ["--ninja-bin=%s" % toolchain.ninja]
        if args.distcc:
            impl_args += [
                "--distcc",
                "--distcc-pump=%s" % toolchain.distcc_pump
            ]

        # *NOTE* We use normal cmake to pass through tsan/ubsan options. We do
        # NOT pass them to build-script-impl.
        if args.enable_asan:
            impl_args += ["--enable-asan"]
            # If we are on linux, disable leak detection when running ASAN. We
            # have a separate bot that checks for leaks.
            if platform.system() == 'Linux':
                os.environ['ASAN_OPTIONS'] = 'detect_leaks=0'
        if args.enable_ubsan:
            impl_args += ["--enable-ubsan"]

        # If we have lsan, we need to export our suppression list. The actual
        # passing in of the LSAN flag is done via the normal cmake method. We
        # do not pass the flag to build-script-impl.
        if args.enable_lsan:
            supp_file = os.path.join(SWIFT_SOURCE_ROOT, SWIFT_REPO_NAME,
                                     "utils",
                                     "lsan_leaks_suppression_list.txt")
            os.environ['LSAN_OPTIONS'] = 'suppressions={}'.format(supp_file)
        if args.verbose_build:
            impl_args += ["--verbose-build"]
        if args.install_symroot:
            impl_args += [
                "--install-symroot", os.path.abspath(args.install_symroot)
            ]
        if args.install_destdir:
            impl_args += [
                "--install-destdir", os.path.abspath(args.install_destdir)
            ]

        if args.skip_build:
            impl_args += ["--skip-build-cmark",
                          "--skip-build-llvm",
                          "--skip-build-swift"]
        if not args.build_benchmarks:
            impl_args += ["--skip-build-benchmarks"]
        # Currently we do not build external benchmarks by default.
        if args.build_external_benchmarks:
            impl_args += ["--skip-build-external-benchmarks=0"]
        if not args.build_foundation:
            impl_args += ["--skip-build-foundation"]
        if not args.build_xctest:
            impl_args += ["--skip-build-xctest"]
        if not args.build_lldb:
            impl_args += ["--skip-build-lldb"]
        if not args.build_llbuild:
            impl_args += ["--skip-build-llbuild"]
        if not args.build_libcxx:
            impl_args += ["--skip-build-libcxx"]
        if not args.build_libdispatch:
            impl_args += ["--skip-build-libdispatch"]
        if not args.build_libicu:
            impl_args += ["--skip-build-libicu"]
        if not args.build_swiftpm:
            impl_args += ["--skip-build-swiftpm"]
        if not args.build_swiftsyntax:
            impl_args += ["--skip-build-swiftsyntax"]
        if not args.build_skstresstester:
            impl_args += ["--skip-build-skstresstester"]
        if not args.build_swiftevolve:
            impl_args += ["--skip-build-swiftevolve"]
        if not args.build_playgroundsupport:
            impl_args += ["--skip-build-playgroundsupport"]
        if args.build_swift_dynamic_stdlib:
            impl_args += ["--build-swift-dynamic-stdlib"]
        if args.build_swift_static_stdlib:
            impl_args += ["--build-swift-static-stdlib"]
        if args.build_swift_stdlib_unittest_extra:
            impl_args += ["--build-swift-stdlib-unittest-extra"]
        if args.build_swift_dynamic_sdk_overlay:
            impl_args += ["--build-swift-dynamic-sdk-overlay"]
        if args.build_swift_static_sdk_overlay:
            impl_args += ["--build-swift-static-sdk-overlay"]

        if not args.build_linux:
            impl_args += ["--skip-build-linux"]
        if not args.build_freebsd:
            impl_args += ["--skip-build-freebsd"]
        if not args.build_cygwin:
            impl_args += ["--skip-build-cygwin"]
        if not args.build_osx:
            impl_args += ["--skip-build-osx"]
        if not args.build_ios_device:
            impl_args += ["--skip-build-ios-device"]
        if not args.build_ios_simulator:
            impl_args += ["--skip-build-ios-simulator"]
        if not args.build_tvos_device:
            impl_args += ["--skip-build-tvos-device"]
        if not args.build_tvos_simulator:
            impl_args += ["--skip-build-tvos-simulator"]
        if not args.build_watchos_device:
            impl_args += ["--skip-build-watchos-device"]
        if not args.build_watchos_simulator:
            impl_args += ["--skip-build-watchos-simulator"]
        if not args.build_android:
            impl_args += ["--skip-build-android"]

        if not args.test and not args.long_test and not args.stress_test:
            impl_args += ["--skip-test-swift"]
        if not args.test:
            impl_args += ["--skip-test-cmark",
                          "--skip-test-lldb",
                          "--skip-test-llbuild",
                          "--skip-test-swiftpm",
                          "--skip-test-swiftsyntax",
                          "--skip-test-skstresstester",
                          "--skip-test-swiftevolve",
                          "--skip-test-xctest",
                          "--skip-test-foundation",
                          "--skip-test-libdispatch",
                          "--skip-test-libicu",
                          "--skip-test-playgroundsupport"]
        if not args.test_linux:
            impl_args += ["--skip-test-linux"]
        if not args.test_freebsd:
            impl_args += ["--skip-test-freebsd"]
        if not args.test_cygwin:
            impl_args += ["--skip-test-cygwin"]
        if not args.test_osx:
            impl_args += ["--skip-test-osx"]
        if not args.test_ios_host:
            impl_args += ["--skip-test-ios-host"]
        if not args.test_ios_simulator:
            impl_args += ["--skip-test-ios-simulator"]
        if not args.test_ios_32bit_simulator:
            impl_args += ["--skip-test-ios-32bit-simulator"]
        if not args.test_tvos_host:
            impl_args += ["--skip-test-tvos-host"]
        if not args.test_tvos_simulator:
            impl_args += ["--skip-test-tvos-simulator"]
        if not args.test_watchos_host:
            impl_args += ["--skip-test-watchos-host"]
        if not args.test_watchos_simulator:
            impl_args += ["--skip-test-watchos-simulator"]
        if not args.test_android:
            impl_args += ["--skip-test-android"]
        if not args.test_android_host:
            impl_args += ["--skip-test-android-host"]
        if args.build_runtime_with_host_compiler:
            impl_args += ["--build-runtime-with-host-compiler"]
        if args.validation_test:
            impl_args += ["--validation-test"]
        if args.long_test:
            impl_args += ["--long-test"]
        if args.stress_test:
            impl_args += ["--stress-test"]
        if not args.benchmark:
            impl_args += ["--skip-test-benchmarks"]
        if not args.test_optimized:
            impl_args += ["--skip-test-optimized"]
        if not args.test_optimize_for_size:
            impl_args += ["--skip-test-optimize-for-size"]
        if not args.test_optimize_none_implicit_dynamic:
            impl_args += ["--skip-test-optimize-none-implicit-dynamic"]
        if args.build_libparser_only:
            impl_args += ["--build-libparser-only"]
        if args.android:
            impl_args += [
                "--android-arch", args.android_arch,
                "--android-ndk", args.android_ndk,
                "--android-api-level", args.android_api_level,
                "--android-ndk-gcc-version", args.android_ndk_gcc_version,
                "--android-icu-uc", args.android_icu_uc,
                "--android-icu-uc-include", args.android_icu_uc_include,
                "--android-icu-i18n", args.android_icu_i18n,
                "--android-icu-i18n-include", args.android_icu_i18n_include,
                "--android-icu-data", args.android_icu_data,
            ]
        if args.android_deploy_device_path:
            impl_args += [
                "--android-deploy-device-path",
                args.android_deploy_device_path,
            ]

        if platform.system() == 'Darwin':
            impl_args += [
                "--toolchain-prefix",
                targets.darwin_toolchain_prefix(
                    args.install_prefix),
                "--host-lipo", toolchain.lipo,
            ]

        if toolchain.libtool is not None:
            impl_args += [
                "--host-libtool", toolchain.libtool,
            ]

        # If we have extra_swift_args, combine all of them together and then
        # add them as one command.
        if args.extra_swift_args:
            impl_args += [
                "--extra-swift-args=%s" % ';'.join(args.extra_swift_args)
            ]

        # If we have extra_cmake_options, combine all of them together and then
        # add them as one command.
        if args.extra_cmake_options:
            impl_args += [
                "--extra-cmake-options=%s" % ' '.join(
                    pipes.quote(opt) for opt in args.extra_cmake_options)
            ]

        if args.lto_type is not None:
            impl_args += [
                "--llvm-enable-lto=%s" % args.lto_type,
                "--swift-tools-enable-lto=%s" % args.lto_type
            ]
            if args.llvm_max_parallel_lto_link_jobs is not None:
                impl_args += [
                    "--llvm-num-parallel-lto-link-jobs=%s" %
                    min(args.llvm_max_parallel_lto_link_jobs, args.build_jobs)
                ]
            if args.swift_tools_max_parallel_lto_link_jobs is not None:
                impl_args += [
                    "--swift-tools-num-parallel-lto-link-jobs=%s" %
                    min(args.swift_tools_max_parallel_lto_link_jobs,
                        args.build_jobs)
                ]

        impl_args += args.build_script_impl_args

        if args.dry_run:
            impl_args += ["--dry-run"]

        if args.clang_profile_instr_use:
            impl_args += [
                "--clang-profile-instr-use=%s" %
                os.path.abspath(args.clang_profile_instr_use)
            ]

        if args.lit_args:
            impl_args += ["--llvm-lit-args=%s" % args.lit_args]

        if args.coverage_db:
            impl_args += [
                "--coverage-db=%s" %
                os.path.abspath(args.coverage_db)
            ]

        # Compute the set of host-specific variables, which we pass through to
        # the build script via environment variables.
        host_specific_variables = self.__compute_host_specific_variables()
        impl_env = {}
        for (host_target, options) in host_specific_variables.items():
            for (name, value) in options.items():
                # We mangle into an environment variable we can easily evaluate
                # from the `build-script-impl`.
                impl_env["HOST_VARIABLE_{}__{}".format(
                    host_target.replace("-", "_"), name)] = value

        return (impl_env, impl_args)

    def __compute_host_specific_variables(self):
        """compute_host_specific_variables(args) -> dict

        Compute the host-specific options, organized as a dictionary keyed by
        host of options.
        """

        args = self.__args

        options = {}
        for host_target in [args.host_target] + args.cross_compile_hosts:
            # Compute the host specific configuration.
            config = HostSpecificConfiguration(args, host_target)

            # Convert into `build-script-impl` style variables.
            options[host_target] = {
                "SWIFT_SDKS": " ".join(sorted(
                    config.sdks_to_configure)),
                "SWIFT_STDLIB_TARGETS": " ".join(
                    config.swift_stdlib_build_targets),
                "SWIFT_BENCHMARK_TARGETS": " ".join(
                    config.swift_benchmark_build_targets),
                "SWIFT_RUN_BENCHMARK_TARGETS": " ".join(
                    config.swift_benchmark_run_targets),
                "SWIFT_TEST_TARGETS": " ".join(
                    config.swift_test_run_targets),
            }

        return options
