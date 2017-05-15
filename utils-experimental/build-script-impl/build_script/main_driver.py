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
from . import swift_utils
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
        host.xcrun_toolchain = args.darwin_xcrun_toolchain

    # Unset environment variables that might affect how tools behave.
    for v in [
            'MAKEFLAGS',
            'SDKROOT',
            'MACOSX_DEPLOYMENT_TARGET',
            'IPHONEOS_DEPLOYMENT_TARGET',
            'TVOS_DEPLOYMENT_TARGET',
            'WATCHOS_DEPLOYMENT_TARGET']:
        os.environ.pop(v, None)

    # Check host environment
    if args.host_target is None:
        raise BuildError("Unsupported host operating system.")

    if not os.path.isdir(env.SWIFT_SOURCE_ROOT):
        raise BuildError("Source root directory %s does not exist. "
                         "Forgot to set $SWIFT_SOURCE_ROOT environment "
                         "variable?" % env.SWIFT_SOURCE_ROOT)

    # check clang available
    if args.host_cc is None:
        args.host_cc = host.find_clang_cc()
    if args.host_cxx is None:
        args.host_cxx = host.find_clang_cxx()
    is_host_clang_ok = (
        args.host_cc is not None and os.access(args.host_cc, os.X_OK) and
        args.host_cxx is not None and os.access(args.host_cxx, os.X_OK))
    if not is_host_clang_ok:
        raise BuildError("Can't find clang. Please install clang-3.5 or a "
                         "later version.")
    # check cmake
    if args.cmake is None:
        args.cmake = host.find_cmake()
    elif not os.access(args.cmake, os.X_OK):
        args.cmake = None
    if args.cmake is None:
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

    if not args.skip_build_android:
        if None in [args.android_ndk, args.android_ndk_version,
                    args.android_icu_uc, args.android_icu_uc_include,
                    args.android_icu_i18n, args.android_icu_i18n_include]:
            raise BuildError("When building for Android, "
                             "--android-ndk, --android-ndk-version, "
                             "--android-icu-uc, --android-icu-uc-include, "
                             "--android-icu-i18n, and "
                             "--android-icu-i18n-include must be speficied")

    # Tweaks

    if host.is_darwin():
        if args.toolchain_prefix is None:
            args.toolchain_prefix = host.toolchain_prefix(args.install_prefix)

    # If not explictly set --host-test, disable them all.
    if not args.host_test:
        args.skip_test_ios_host = True
        args.skip_test_tvos_host = True
        args.skip_test_watchos_host = True

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

    # Show SDKs if requested
    if args.show_sdks:
        host.show_sdks()

    # Make arguments object immutable.

    Args.freeze(args)

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

    # Replace swift_utils base directory just in case `source_root` is not
    # *this* source root.
    swift_utils.basedir = os.path.join(source_root, 'swift', 'utils')

    # Potentionally file system mutating operations from here.

    if os.path.exists(build_root):
        # Clean if requested
        if args.clean:
            printf("-- Clean previous build --")
            shell.rmtree(build_root)
            # FIXME: Souldn't we clean install_destdir also?

    if not os.path.exists(build_root):
        # Create build directory if not exists
        shell.makedirs(build_root)
    else:
        # TODO: Implement auto reconfigure.
        #       To do that, we should serialze `args` object, and store it in
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

    # Create cross tools compilation target list
    # FIXME: We should emit warning or error if unsupported deployment
    #        targets are  requested.
    cross_tools_deployment_targets = [
        target for target in args.cross_compile_tools_deployment_targets if
        target != args.host_target]

    all_deployment_targets = [args.host_target, ]
    all_deployment_targets += cross_tools_deployment_targets

    # Initialize Builders list

    builds = []

    # Instantiate all builders for each deployment targets for each products.

    # At first, build Ninja if needed
    ninja_build = None
    if args.build_ninja:
        products.Ninja.prepare(workspace)
        ninja_build = products.Ninja(
            deployment_target=args.host_target,
            target_build_dir=workspace.build_dir(args.host_target, 'ninja'),
            args=args)
        builds.append(ninja_build)

    # Prepare common CMake
    cmake = CMake(
        path=args.cmake,
        host_cc=args.host_cc,
        host_cxx=args.host_cxx,
        host_distcc=host_distcc,
        host_distcc_pump=host_distcc_pump,
        ninja_build=ninja_build,
        args=args)

    host_swift_build = None
    host_llvm_build = None
    need_merge_lipo = (args.cross_compile_tools_deployment_targets and
                       not args.skip_merge_lipo_cross_compile_tools)

    for deployment_target in all_deployment_targets:

        # Where to install
        install_destdir = args.install_destdir
        if install_destdir is None:
            install_destdir = '/tmp'  # dummy
        if not args.cross_compile_tools_deployment_targets:
            target_install_destdir = install_destdir
        else:
            # If cross compiling tools, install into a deployment target
            # specific subdirectory.
            if need_merge_lipo:
                target_install_destdir = os.path.join(
                    workspace.build_root_dir(),
                    'intermediate-install',
                    deployment_target)
            else:
                target_install_destdir = os.path.join(
                    install_destdir,
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
            host_llvm_build=host_llvm_build,
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
            host_llvm_build=host_llvm_build,
            host_swift_build=host_swift_build,
            args=args)
        builds.append(swift_build)

        if deployment_target == args.host_target:
            host_llvm_build = llvm_build
            host_swift_build = swift_build

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
                swift_build=swift_build,
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

            # FIXME: Foundation *tests* optionally depends on xctest build.
            #        XCTest *build* depends on foundation build.
            #        Is there a better way?
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

    # Print out some infomation

    (stdlib_targets,
     benchmark_targets,
     test_targets,
     run_benchmarks_targets) = host_swift_build._stdlib_build_targets()
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
    # FIXME: Since some products create intermediate files in its *source*
    #        directory, we have to configure and build in a row.
    #        *Except Ninja* because other configure requires pre-built ninja.
    # This should be:
    #
    # if ninja_build is not None:
    #     ninja_build.configure()
    #     ninja_build.build()
    #
    # for build in builds:
    #     build.configre()
    # for build in builds:
    #     build.build()

    # Test
    for build in builds:
        build.test()

    # Install
    for build in builds:
        build.install()

    # FIXME: Factor out below so that we can unit test them

    # Merge lipo if required
    if need_merge_lipo and args.install_destdir:
        printf("--- Merging and running lipo for {0} ---",
               str(cross_tools_deployment_targets))
        recursive_lipo_cmd = [
            swift_utils('recursive-lipo'),
            "--lipo=" + host.xcrun_find('lipo'),
            "--copy-subdirs={0} {1}".format(
                os.path.join(args.install_prefix,
                             'lib', 'swift'),
                os.path.join(args.install_prefix,
                             'lib', 'swift_static')),
            "--destination=" + args.install_destdir]
        for deployment_target in cross_tools_deployment_targets:
            recursive_lipo_cmd += [
                os.path.join(args.build_dir,
                             'intermediate-install',
                             deployment_target)]
        shell.invoke(recursive_lipo_cmd)

    # Install symbols
    if args.darwin_install_extract_symbols:
        printf('--- Installing symbols ---')
        cmd = 'find ./{0} -perm -0111 -type f -print | cpio -pdm "{1}"'.format(
            args.toolchain_prefix.lstrip('/'),
            args.install_symroot)
        with shell.pushd(args.install_destdir):
            shell.invoke(["sh", "-c", cmd])

        cmd = ("find ./{0} -perm -0111 -type f -print | "
               "grep -v swift-stdlib-tool | "
               "grep -v crashlog.py | "
               "grep -v symbolication.py | "
               "xargs -n 1 -P {1} {2}")
        cmd = cmd.format(args.toolchain_prefix.lstrip('/'),
                         args.build_jobs,
                         host.xcrun_find('dsymutil'))
        with shell.pushd(args.install_symroot):
            shell.invoke(["sh", "-c", cmd])

        cmd = ('find {0}/{1} '
               '"(" -perm -0111 -or -name "*.a" ")" -type f -print | '
               'xargs -n 1 -P {2} {3} -S')
        cmd = cmd.format(args.install_destdir,
                         args.toolchain_prefix.lstrip('/'),
                         args.build_jobs,
                         host.xcrun_find('strip'))
        shell.invoke(["sh", "-c", cmd])

    # Installable package
    if args.installable_package:
        printf("--- Creating installable package ---")
        printf("-- Package file: {0} --", args.installable_package)

        if host.is_darwin():
            # Copy swift-stdlib-tool laucher if neccesary.
            swift_stdlib_tool_bin_path = os.path.join(
                args.install_destdir,
                args.install_prefix.lstrip('/'),
                'bin', 'swift-stdlib-tool')
            if not os.path.exists(swift_stdlib_tool_bin_path):
                printf("--- Copy swift-stdlib-tool ---")
                shell.copy(
                    swift_build.swift_stdlib_tool_source_path,
                    swift_stdlib_tool_bin_path)

            # Create plist for xctoolchain.
            printf("-- Create Info.plist --")
            darwin_toolchain_install_location = os.path.join(
                '/Library/Developer/Toolchains',
                '%s.xctoolchain' % args.darwin_toolchain_name)
            darwin_toolchain_info_plist_path = os.path.join(
                args.install_destdir,
                args.toolchain_prefix.lstrip('/'),
                "Info.plist")
            darwin_toolchain_report_url = "https://bugs.swift.org/"

            if os.path.exists(darwin_toolchain_info_plist_path):
                printf("-- Removing: {0}", darwin_toolchain_info_plist_path)
                shell.remove(darwin_toolchain_info_plist_path)

            plist_buddy_bin = "/usr/libexec/PlistBuddy"

            def plist_buddy(name, ty, value=None):
                val = 'Add {0} {1}'.format(name, ty)
                if value is not None:
                    val += ' "{0}"'.format(value)
                command = [plist_buddy_bin, '-c', val]
                command += [darwin_toolchain_info_plist_path, ]
                shell.invoke(command)

            plist_buddy('DisplayName', 'string',
                        args.darwin_toolchain_display_name)
            plist_buddy('Version', 'string',
                        args.darwin_toolchain_version)
            plist_buddy('CFBundleIdentifier', 'string',
                        args.darwin_toolchain_bundle_identifier)
            plist_buddy('ReportProblemURL', 'string',
                        darwin_toolchain_report_url)
            plist_buddy('Aliases', 'array')
            plist_buddy('Aliases:0', 'string',
                        args.darwin_toolchain_alias)

            shell.invoke(['chmod', 'a+r', darwin_toolchain_info_plist_path])

            # Codesign
            if args.darwin_toolchain_application_cert is not None:
                printf("-- Codesign xctoolchain --")
                shell.invoke([
                    swift_utils('toolchain-codesign'),
                    args.darwin_toolchain_application_cert,
                    os.path.join(args.install_destdir,
                                 args.toolchain_prefix.lstrip('/'))])

            # Installer
            if args.darwin_toolchain_installer_package is not None:
                printf("-- Create Installer --")

                shell.invoke([
                    swift_utils('toolchain-installer'),
                    os.path.join(args.install_destdir,
                                 args.toolchain_prefix.lstrip('/')),
                    args.darwin_toolchain_bundle_identifier,
                    args.darwin_toolchain_installer_cert,
                    args.darwin_toolchain_installer_package,
                    darwin_toolchain_install_location,
                    args.darwin_toolchain_version,
                    swift_utils('darwin-installer-scripts')])

            # Create tarball
            with shell.pushd(args.install_destdir):
                shell.invoke(['tar', '-c', '-z',
                              '-f', args.installable_package,
                              args.install_prefix.lstrip('/')])
        else:
            # Just create tarball
            with shell.pushd(args.install_destdir):
                shell.invoke(['tar', '-c', '-z',
                              '--owner=0', '--group=0',
                              '-f', args.installable_package,
                              args.install_prefix.lstrip('/')])

    # Test Installable package
    if args.installable_package and args.test_installable_package:
        printf("-- Test Installable Package --")

        # Collect paths
        pkg_tests_source_dir = workspace.subdir('swift-integration-tests')
        pkg_tests_sandbox_parent = workspace.build_dir('none',
                                                       'integration-tests')
        if host.is_darwin():
            pkg_tests_sandbox = os.path.join(
                pkg_tests_sandbox_parent, args.toolchain_prefix.lstrip('/'))
        else:
            pkg_tests_sandbox = pkg_tests_sandbox_parent

        # Clean up tests directory
        if os.path.exists(pkg_tests_sandbox_parent):
            shell.rmtree(pkg_tests_sandbox_parent)
        shell.makedirs(pkg_tests_sandbox)

        # Install the package into sandbox and run tests
        with shell.pushd(pkg_tests_sandbox_parent):
            shell.invoke(['tar', '-x', '-z',
                          '-f', args.installable_package])

        lit_bin_path = host_llvm_build.lit_bin_path
        filecheck_bin_path = host_llvm_build.filecheck_bin_path
        with shell.pushd(pkg_tests_source_dir):
            shell.invoke(['python', lit_bin_path, '.', '-sv',
                          '--param', 'package-path=' + pkg_tests_sandbox,
                          '--param', 'filecheck=' + filecheck_bin_path])

    # Symbols package
    if args.symbols_package:
        printf('--- Creating symbols package ---')
        printf('-- Package file: {0} --', args.symbols_package)

        if host.is_darwin():
            prefix = host.toolchain_prefix(args.install_prefix)
        else:
            prefix = args.install_prefix

        # As a security measure, `tar` normally strips leading '/' from paths
        # it is archiving. To stay safe, we change working directories, then
        # run `tar` without the leading '/' (we remove it ourselves to keep
        # `tar` from emitting a warning).
        with shell.pushd(args.install_symroot):
            shell.invoke(['tar', '-c', '-z', '-f',
                         args.symbols_package,
                         prefix.lstrip('/')])

    return 0
