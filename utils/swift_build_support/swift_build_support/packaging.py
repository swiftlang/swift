# swift_build_support/packaging.py - Creating packages          -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#

import os
import shutil
import sys

import shell

import tar

import targets


class Package(object):
    def __init__(self, args, workspace, host_name):
        self.__args = args
        self.__workspace = workspace
        self.__host_name = host_name

    def build(self):
        if self.__args.installable_package is None:
            return

        if self.__args.install_destdir is None:
            print("--install-destdir required to build a package. Skipping.")

        package_for_host = self.__package_name()
        print("--- Creating installable package ---")
        print("-- Package file: {} --".format(package_for_host))

        # Assume the lipo builds are (or include) an OS X host and build an
        # xctoolchain
        if self.__host_name.startswith('macosx-') or \
                self.__host_name == 'merged-hosts':
            self.__build_for_osx(package_for_host)
        else:
            self.__build_for_others(package_for_host)

    def test(self):
        if self.__args.installable_package is None:
            return

        if self.__args.install_destdir is None:
            print("--install-destdir required to test a package. Skipping.")

        if not self.__args.test_installable_package:
            return

        pkg_tests_source_dir = self.__workspace.source_dir(
            'swift-integration-tests')
        pkg_tests_sandbox_parent = self.__workspace.build_dir(
            'none', 'swift_package_sandbox_{}'.format(self.__host_name))
        pkg_tests_temps = os.path.join(pkg_tests_sandbox_parent, 'tests')

        if self.__host_name.startswith('macosx-'):
            toolchain_prefix = targets.darwin_toolchain_prefix(
                self.__args.install_prefix)
            pkg_tests_sandbox = os.path.join(
                pkg_tests_sandbox_parent, toolchain_prefix)
        else:
            pkg_tests_sandbox = pkg_tests_sandbox_parent

        lit_executable_path = os.path.join(
            self.__workspace.source_dir('llvm'), 'utils', 'lit', 'lit.py')
        llvm_bin_dir = os.path.join(
            self.__workspace.build_dir(self.__args.host_target, 'llvm'),
            'bin')

        print("-- Test Installable Package --")
        shutil.rmtree(pkg_tests_sandbox_parent, True)
        os.makedirs(pkg_tests_sandbox)

        with shell.pushd(pkg_tests_sandbox_parent):
            tar.untar(self.__package_name())

        with shell.pushd(pkg_tests_source_dir):
            shell.call([
                sys.executable,
                lit_executable_path,
                '.',
                '-sv',
                '--param', 'package-path={}'.format(pkg_tests_sandbox),
                '--param', 'test-exec-root={}'.format(pkg_tests_temps),
                '--param', 'llvm-bin-dir={}'.format(llvm_bin_dir)
            ])

    def __build_for_osx(self, package_for_host):
        # TODO: this branch is avoided in build-script
        raise NotImplementedError("macosx not currently supported")

    def __build_for_others(self, package_for_host):
        install_destdir = self.__install_destdir()

        install_prefix = self.__install_prefix()
        if install_prefix.startswith(os.path.sep):
            install_prefix = os.path.relpath(install_prefix, os.path.sep)

        with shell.pushd(install_destdir):
            tar.tar(install_prefix, package_for_host)

    def __install_destdir(self):
        if self.__args.cross_compile_hosts:
            # if cross compiling tools, install into a host-specific
            # subdirectory
            if self.__should_include_in_lipo():
                # if this is one of the host we should lipo, install into a
                # temporary subdirectory
                destdir = os.path.join(self.__workspace.build_dir,
                                       'intermediate-install',
                                       self.__host_name)
            else:
                destdir = os.path.join(self.__args.install_destdir,
                                       self.__host_name)
        else:
            destdir = self.__args.install_destdir

        # ensure the value ends in the directory separator; it's a directory
        return os.path.join(destdir, '')

    def __install_prefix(self):
        if self.__is_cross_tools_host() and \
                self.__args.cross_compile_install_prefixes:
            prefixes = self.__args.cross_compile_install_prefixes.split(';')

            try:
                host_index = self.__args.cross_compile_host.index(
                    self.__host_name)
            except ValueError:
                host_index = None

            if host_index is not None and host_index < len(prefixes):
                host_install_prefix = prefixes[host_index]
            else:
                # if there is no explicit install prefix for this host, use the
                # last one in the list
                host_install_prefix = prefixes[-1]
        else:
            host_install_prefix = self.__args.install_prefix

        # should always begin with the root path; otherwise CMake will expand
        # it as a relative path from the build folder.
        host_install_prefix = os.path.join(os.path.sep, host_install_prefix)

        # should always end in a directory separator; it's a directory
        return os.path.join(host_install_prefix, '')

    def __package_name(self):
        if self.__args.cross_compile_hosts:
            return '{}-{}'.format(self.__args.installable_package,
                                  self.__host_name)
        else:
            return self.__args.installable_package

    def __should_include_host_in_lipo(self):
        if self.__args.cross_compile_hosts and \
                self.__args.skip_merge_lipo_cross_compile_tools:
            return self.__host_name.startswith('iphone') or \
                self.__host_name.startswith('appletv') or \
                self.__host_name.startswith('watch')

        return False

    def __is_cross_tools_host(self):
        return self.__host_name in self.__args.cross_compile_hosts
