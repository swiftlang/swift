# build_script/main_driver.py -----------------------------------*- python -*-
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
The main workflow of the build-script
"""
# ----------------------------------------------------------------------------

from __future__ import print_function
from __future__ import absolute_import

import os
import os.path

from . import env
from . import shell
from .host import host
from .workspace import Workspace
from .cmake import CMake
from .exceptions import BuildError
from .driver_arguments import create_argparser, Args
from . import products
from .utils import printf


# Main entry point for the normal mode.
def main(argv):
    # remove argv[0] and dash-dash ("--")
    argv = [arg for arg in argv[1:] if arg != "--"]

    args = Args()

    # Temporary option default value because of some workarounds here.

    # FIXME: Make the option of building using the host clang the default
    #        as a workaround until we fix the ASAN bot.
    #        https://github.com/apple/swift/commit/0b1e858
    args.build_runtime_with_host_compiler = True

    # Parse arguments
    parser = create_argparser()
    args = parser.parse_args(argv, namespace=args)

    # Setup environment
    shell.dry_run = args.dry_run

    if host.is_darwin():
        host.xcrun_toolchain = args.xcrun_toolchain

    # Check host environment
    if args.host_target is None:
        raise BuildError("Unsupported host operating system.")

    if not os.path.isdir(env.SWIFT_SOURCE_ROOT):
        raise BuildError("Source root directory %s does not exist. "
                         "Forgot to set $SWIFT_SOURCE_ROOT environment "
                         "variable?" % env.SWIFT_SOURCE_ROOT)

    # check clang available
    host_cc = args.host_cc
    host_cxx = args.host_cxx
    if host_cc is None:
        host_cc = host.find_clang_cc()
    if host_cxx is None:
        host_cxx = host.find_clang_cxx()
    is_host_clang_ok = (
        host_cc is not None and os.access(host_cc, os.X_OK) and
        host_cxx is not None and os.access(host_cxx, os.X_OK))
    if not is_host_clang_ok:
        raise BuildError("Can't find clang. Please install clang-3.5 or a "
                         "later version.")
    # check cmake
    host_cmake = args.cmake
    if host_cmake is None:
        host_cmake = host.find_cmake()
    elif not os.access(host_cmake, os.X_OK):
        host_cmake = None
    if host_cmake is None:
        raise BuildError("Can't find CMake.  Please install CMake")

    # check distcc is requested
    host_distcc = None
    host_distcc_pump = None
    if args.distcc:
        host_distcc = host.find_distcc()
        host_distcc_pump = host.find_distcc_pump()
        if host_distcc is None or host_distcc_pump is None:
            raise BuildError("Can't find distcc")

    # Sanitize arguments
    if args.symbols_package:
        if not os.path.isabs(args.symbols_package):
            raise BuildError('--symbols-package must be an absolute path '
                             '(was \'%s\')' % (args.symbols_package, ))
        if not args.install_symroot:
            raise BuildError("--install-symroot is required when specifying "
                             "--symbols-package.")
    install_something = any(
        getattr(args, 'install_' + p) for p in [
            'cmark', 'swift', 'llbuild', 'swiftpm',
            'libdispatch', 'foundation', 'xctest'])
    if not install_something:
        install_something = len(args.llvm_install_components) > 0
    if install_something and args.install_destdir is None:
        raise BuildError("--install-destdir is required when install "
                         "something.")

    # Tweaks

    # We need ninja even if cmake_generator is *not* `Ninja`, because some
    # products use ninja directry.
    if host.find_ninja() is None:
        args.build_ninja = True

    # Linux ARM targets require the gold linker.
    gold_linker_required_deployment_targets = [
        'linux-armv6',
        'linux-armv7',
        'linux-aarch64']
    if args.host_target in gold_linker_required_deployment_targets:
        args.use_gold_linker = True

    # Workarounds

    args.skip_compiler_rt = False  # Temporary attribute for the workaround.

    # FIXME: We currently do not support building compiler-rt with the
    #        Xcode generator.
    if args.cmake_generator == 'Xcode':
        args.skip_compier_rt = True

    # FIXME: We currently do not support cross-compiling swift with
    #        compiler-rt.
    if len(args.cross_compile_tools_deployment_targets):
        args.skip_compiler_rt = True

    # Make arguments object immutable.

    args.freeze__()

    # Initialize workspace

    def compute_build_root():

        root_parent = env.SWIFT_BUILD_ROOT
        # If --build-subdir is specified, use that.
        if args.build_subdir is not None:
            return os.path.join(root_parent, args.build_subdir)

        # Create a name for the build directory.
        build_subdir = args.cmake_generator.replace(" ", "_")

        cmark_build_dir_label = args.cmark_build_type
        if args.cmark_enable_assertions:
            cmark_build_dir_label += "Assert"

        llvm_build_dir_label = args.llvm_build_type
        if args.llvm_enable_assertions:
            llvm_build_dir_label += "Assert"

        swift_build_dir_label = args.swift_build_type
        if args.swift_enable_assertions:
            swift_build_dir_label += "Assert"
        if args.swift_analyze_code_coverage != "false":
            swift_build_dir_label += "Coverage"

        swift_stdlib_build_dir_label = args.swift_stdlib_build_type
        if args.swift_stdlib_enable_assertions:
            swift_stdlib_build_dir_label += "Assert"

        if (llvm_build_dir_label == swift_build_dir_label and
                llvm_build_dir_label == swift_stdlib_build_dir_label and
                llvm_build_dir_label == cmark_build_dir_label):
            # Use a simple directory name if all projects use the same build
            # type.
            build_subdir += "-" + llvm_build_dir_label
        elif (llvm_build_dir_label != swift_build_dir_label and
                llvm_build_dir_label == swift_stdlib_build_dir_label and
                llvm_build_dir_label == cmark_build_dir_label):
            # Swift build type differs.
            build_subdir += "-" + llvm_build_dir_label
            build_subdir += "+swift-" + swift_build_dir_label
        elif (llvm_build_dir_label == swift_build_dir_label and
                llvm_build_dir_label != swift_stdlib_build_dir_label and
                llvm_build_dir_label == cmark_build_dir_label):
            # Swift stdlib build type differs.
            build_subdir += "-" + llvm_build_dir_label
            build_subdir += "+stdlib-" + swift_stdlib_build_dir_label
        elif (llvm_build_dir_label == swift_build_dir_label and
                llvm_build_dir_label == swift_stdlib_build_dir_label and
                llvm_build_dir_label != cmark_build_dir_label):
            # cmark build type differs.
            build_subdir += "-" + llvm_build_dir_label
            build_subdir += "+cmark-" + cmark_build_dir_label
        else:
            # We don't know how to create a short name, so just mangle in all
            # the information.
            build_subdir += "+llvm-" + llvm_build_dir_label
            build_subdir += "+swift-" + swift_build_dir_label
            build_subdir += "+stdlib-" + swift_stdlib_build_dir_label

        return os.path.join(root_parent, build_subdir)

    build_root = args.build_root
    if build_root is None:
        build_root = compute_build_root()

    source_root = args.source_root
    if source_root is None:
        source_root = env.SWIFT_SOURCE_ROOT

    workspace = Workspace(source_root=source_root,
                          build_root=build_root)

    # Potentionally file system mutating operations from here.

    if os.path.exists(build_root):
        # Clean if requested
        if args.clean:
            printf("-- Clean previous build --")
            shell.rmtree(build_root)

    if not os.path.exists(build_root):
        # Create build directory if not exists
        shell.makedirs(build_root)
    else:
        # TODO: Implement auto reconfigure.
        #       To do that we should serialze `args` object, and store it in
        #       build_basedir. Then, at the next build, if saved args are
        #       different from this invocation, we should reconfigure.
        pass

    # Prepare Product builder
    products.CMark.prepare(workspace)
    products.LLVM.prepare(workspace)
    products.Swift.prepare(workspace)
    if not args.skip_build_lldb:
        products.LLDB.prepare(workspace)
    if not args.skip_build_libdispatch:
        products.Libdispatch.prepare(workspace)
    if not args.skip_build_foundation:
        products.Foundation.prepare(workspace)
    if not args.skip_build_xctest:
        products.XCTest.prepare(workspace)
    if not args.skip_build_llbuild:
        products.LLBuild.prepare(workspace)
    if not args.skip_build_swiftpm:
        products.SwiftPM.prepare(workspace)

    # Build Ninja if needed
    ninja_path = None
    ninja_build = None
    if args.build_ninja:
        products.Ninja.prepare(workspace)
        ninja_build = products.Ninja(
            deployment_target=args.host_target,
            target_build_dir=workspace.build_dir(args.host_target, 'ninja'),
            **args)

        ninja_build.configure()
        ninja_build.build()

        # Set cmake to use the just built ninja executable path
        ninja_path = ninja_build.ninja_bin_path

    # Prepare common CMake
    cmake = CMake(
        path=host_cmake,
        host_cc=host_cc,
        host_cxx=host_cxx,
        host_distcc=host_distcc,
        host_distcc_pump=host_distcc_pump,
        ninja_path=ninja_path,
        args=args)

    # Create cross tools cross compilation target list
    # FIXME: Emit warning or error if unsupported deployment targets are
    #        requested.
    cross_tools_deployment_targets = [
        target for target in args.cross_compile_tools_deployment_targets if
        target != args.host_target]

    all_deployment_targets = [args.host_target, ]
    all_deployment_targets += cross_tools_deployment_targets

    # Instantiate all builders for each deployment targets for each products.

    native_swift_build = None
    native_llvm_build = None
    native_cmark_build = None
    native_llvm_tblgen_bin_path = None
    native_clang_tblgen_bin_path = None

    builds = []
    for deployment_target in all_deployment_targets:

        # Where to install
        install_destdir = args.install_destdir
        if install_destdir is None:
            install_destdir = '/'  # dummy
        if not args.cross_compile_tools_deployment_targets:
            target_install_destdir = install_destdir
        else:
            # If cross compiling tools, install into a deployment target
            # specific subdirectory.
            if not args.skip_merge_lipo_cross_compile_tools:
                target_install_destdir = os.path.join(workspace.build_root(),
                                                      'intermediate-install',
                                                      deployment_target)
            else:
                target_install_destdir = os.path.join(install_destdir,
                                                      deployment_target)

        # Start to instantiate product builders.

        cmark_build_dir = workspace.build_dir(deployment_target, 'cmark')
        cmark_build = products.CMark(
            deployment_target=deployment_target,
            target_build_dir=cmark_build_dir,
            target_install_destdir=target_install_destdir,
            cmake=cmake,
            args=args)
        builds.append(cmark_build)

        llvm_build_dir = workspace.build_dir(deployment_target, 'llvm')
        llvm_build = products.LLVM(
            deployment_target=deployment_target,
            target_build_dir=llvm_build_dir,
            target_install_destdir=target_install_destdir,
            native_llvm_tblgen_bin_path=native_llvm_tblgen_bin_path,
            native_clang_tblgen_bin_path=native_clang_tblgen_bin_path,
            cmake=cmake,
            args=args)
        builds.append(llvm_build)

        swift_build_dir = workspace.build_dir(deployment_target, 'swift')
        swift_build = products.Swift(
            deployment_target=deployment_target,
            target_build_dir=swift_build_dir,
            target_install_destdir=target_install_destdir,
            cmake=cmake,
            cmark_build=cmark_build,
            llvm_build=llvm_build,
            args=args)
        builds.append(swift_build)

        if deployment_target == args.host_target:
            native_cmark_build = cmark_build
            native_llvm_build = llvm_build
            native_swift_build = swift_build
            native_llvm_tblgen_bin_path = llvm_build.llvm_tblgen_bin_path
            native_clang_tblgen_bin_path = llvm_build.clang_tblgen_bin_path

        # Optional products.

        lldb_build = None
        if not args.skip_build_lldb:
            lldb_build_dir = workspace.build_dir(deployment_target, 'lldb')
            lldb_build = products.LLDB(
                deployment_target=deployment_target,
                target_build_dir=lldb_build_dir,
                target_install_destdir=target_install_destdir,
                cmark_build=cmark_build,
                llvm_build=llvm_build,
                swift_build=swift_build,
                cmake=cmake,
                args=args)
            builds.append(lldb_build)

        libdispatch_build = None
        if not args.skip_build_libdispatch:
            libdispatch_build_dir = workspace.build_dir(deployment_target,
                                                        'libdispatch')
            libdispatch_build = products.Libdispatch(
                deployment_target=deployment_target,
                target_build_dir=libdispatch_build_dir,
                target_install_destdir=target_install_destdir,
                args=args)
            builds.append(libdispatch_build)

        foundation_build = None
        if not args.skip_build_foundation:
            foundation_build_dir = workspace.build_dir(deployment_target,
                                                       'foundation')
            foundation_build = products.Foundation(
                deployment_target=deployment_target,
                target_build_dir=foundation_build_dir,
                target_install_destdir=target_install_destdir,
                llvm_build=llvm_build,
                swift_build=swift_build,
                libdispatch_build=libdispatch_build,
                ninja_build=ninja_build,
                args=args)
            builds.append(foundation_build)

        xctest_build = None
        if not args.skip_build_xctest:
            xctest_build_dir = workspace.build_dir(deployment_target,
                                                   'xctest')
            xctest_build = products.XCTest(
                deployment_target=deployment_target,
                target_build_dir=xctest_build_dir,
                target_install_destdir=target_install_destdir,
                swift_build=swift_build,
                foundation_build=foundation_build,
                args=args)
            builds.append(xctest_build)

            # FIXME: Foundation *tests* depends on xctest build.
            #        XCTest *build* depends on foundation build.
            #        Is there better way?
            if foundation_build:
                foundation_build.xctest_build = xctest_build

        llbuild_build = None
        if not args.skip_build_llbuild:
            llbuild_build_dir = workspace.build_dir(deployment_target,
                                                    'llbuild')
            llbuild_build = products.LLBuild(
                deployment_target=deployment_target,
                target_build_dir=llbuild_build_dir,
                target_install_destdir=target_install_destdir,
                llvm_build=llvm_build,
                cmake=cmake,
                args=args)
            builds.append(llbuild_build)

        swiftpm_build = None
        if not args.skip_build_swiftpm:
            swiftpm_build_dir = workspace.build_dir(deployment_target,
                                                    'swiftpm')
            swiftpm_build = products.SwiftPM(
                deployment_target=deployment_target,
                target_build_dir=swiftpm_build_dir,
                target_install_destdir=target_install_destdir,
                swift_build=swift_build,
                llbuild_build=llbuild_build,
                foundation_build=foundation_build,
                xctest_build=xctest_build,
                args=args)
            builds.append(swiftpm_build)

    # Provide some infomation

    (stdlib_targets,
     benchmark_targets,
     test_targets,
     run_benchmarks_targets) = native_swift_build._stdlib_build_targets()
    printf("Building the standard library for: {0}",
           ", ".join(stdlib_targets))
    if len(test_targets):
        printf("Running Swift tests for: {0}",
               ", ".join(test_targets))
    if len(run_benchmarks_targets):
        printf("Running Swift benchmarks for: {0}",
               ", ".join(run_benchmarks_targets))

    # Configure and Build
    for build in builds:
        build.configure()
        build.build()
    # FIXME: Since some products create intermediate files in its source
    #        directory, we have to configure and build in a row.
    # This should be:
    #
    # for build in builds:
    #     build.configre()
    # for build in builds:
    #     build.build()

    # Test
    for build in builds:
        build.test()

    # Insttall
    for build in builds:
        build.install()

    # Package works
    #do_package(args)  # TODO
    return 0
