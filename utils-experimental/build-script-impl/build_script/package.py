from __future__ import absolute_import

import os.path

from . import shell
from . import products
from .cmake import CMake
from .host import host
from .workspace import workspace
from .utils import printf


def do_installable_package(args, lit_bin_path, filecheck_bin_path):
    '''\
    Create and test installalble package
    '''

    printf("--- Creating installable package ---")
    printf("-- Package file: %s --", args.installable_package)

    if host.is_dawin:
        # Copy swift-stdlib-tool laucher if neccesary.
        swift_stdlib_tool_bin_path = os.path.join(
            args.install_destdir, args.install_prefix,
            'bin', 'swift-stdlib-tool')
        if not os.path.exists(swift_stdlib_tool_bin_path):
            swift_stdlib_tool_source_path = os.path.join(
                    args.swift_source_dir,
                    'utils', 'swift-stdlib-tool-substitute')
            printf("--- Copy swift-stdlib-tool ---")
            shell.copy(
                swift_stdlib_tool_source_path,
                swift_stdlib_tool_bin_path)

        # Create plist for xctoolchain.
        printf("-- Create Info.plist --")
        darwin_toolchain_install_location = os.path.join(
            '/Library/Developer/Toolchains',
            '%s.xctoolchain' % args.darwin_toolchain_name)
        darwin_toolchain_info_plist_path = os.path.join(
            args.install_dest_dir, args.toolchain_prefix, "Info.plist")
        darinw_toolchain_report_url = "https://bugs.swift.org/"

        if os.path.exists(darwin_toolchain_info_plist_path):
            printf("-- Removing: ${DARWIN_TOOLCHAIN_INFO_PLIST}")
            shell.remove(darwin_toolchain_info_plist_path)

        plist_buddy_bin = "/usr/libexec/PlistBuddy"
        def plist_buddy(*args):
            command = [plist_buddy_bin, '-c', 'Add']
            command += args
            command += (darwin_toolchain_info_plist_path,)
            shell.invoke(command)

        plist_buddy('DisplayName', 'string',
                    args.darwin_toolchain_display_name)
        plist_buddy('Version', 'string',
                    args.darwin_toolchain_version)
        plist_buddy('CFBundleIdentifier', 'string',
                    args.darwin_toolchain_bundle_identifier)
        plist_buddy('ReportProblemURL', 'string',
                    args.darwin_toolchain_report_url)
        plist_buddy('Aliases', 'array')
        plist_byddy('Aliases:0', 'string',
                    args.darwin_toolchain_alias)

        shell.invoke(['chmod', 'a+r', darwin_toolchain_info_plist_path])

        # Codesign
        if args.darwin_toolchain_application_cert is not None:
            printf("-- Codesign xctoolchain --")
            conditional_invoke([
                os.path.join(args.swift_source_dir,
                    'utils', 'toolchain-codesign'),
                args.darwin_toolchain_application_cert,
                os.path.join(args.install_dest_dir, args.toolchain_prefix)])

        # Installer
        if args.darwin_toolchain_installer_package is not None:
            printf("-- Create Installer --")
            conditional_invoke([
                os.path.join(args.swift_source_dir,
                    'utils', 'toolchain-installer'),
                os.path.join(args.install_dest_dir, args.toolchain_prefix),
                args.darwin_toolchain_bundle_identifier,
                args.darwin_toolchain_installer_cert,
                args.darwin_toolchain_installer_package,
                args.darwin_toolchain_install_location,
                args.darwin_toolchain_version,
                os.path.join(args.swift_source_dir,
                    'utils', 'darwin-installer-scripts')])

        # Create tarball
        with shell.pushd(args.install_dest_dir):
            shell.invoke(['tar', '-c', '-z',
                          '-f', args.installable_package,
                          args.install_prefix.lstrip('/')])
    else:
        # Just create tarball
        with shell.pushd(args.install_dest_dir):
            shell.invoke(['tar', '-c', '-z',
                          '--owner=0', '--group=0'
                          '-f', args.installable_package,
                          args.install_prefix.lstrip('/')])

    # Package tests
    if args.test_installable_package:

        # Collect paths
        pkg_tests_source_dir = workspace.subdir('swift-integration-tests')
        pkg_tests_sandbox_parent = args.product_build_dir(
            'integration-tests', None)
        if host.is_darwin:
            pkg_tests_sandbox = os.path.join(
                pkg_tests_sandbox_parent, args.toolchain_prefix)
        else:
            pkg_tests_sandbox = pkg_tests_sandbox_parent

        printf("-- Test Installable Package --")

        # Clean up tests directory
        shell.rmtree(pkg_tests_sandbox_parent)
        shell.makedirs(pkg_tests_sandbox)

        # Install the package into sandbox and run tests
        with shell.pushd(pkg_tests_sandbox_parent):
            conditional_invoke(['tar', '-x', '-z',
                '-f', args.installable_package])

        with shell.pushd(pkg_tests_source_dir):
            conditional_invoke(
                ['python', lit_bin_path, '.', '-sv',
                 '--package_path=%s' % pkg_tests_sandbox,
                 '--param', 'filecheck=%s' % filecheck_bin_path])

    if args.symbols_package:
        printf('--- Creating symbols package ---')
        printf('-- Package file: %s --', args.symbols_package)

        if host.is_darwin():
            prefix = host.toolchain_prefix(args.install_prefix)
        else:
            prefix = args.install_prefix

        # As a security measure, `tar` normally strips leading '/' from paths
        # it is archiving. To stay safe, we change working directories, then
        # run `tar` without the leading '/' (we remove it ourselves to keep
        # `tar` from emitting a warning).
        with shell.pushd(args.install_symroot):
            swift_build_support.tar.tar(source=prefix.lstrip('/'),
                                        destination=args.symbols_package)
