# build_script/products/llvm.py ---------------------------------*- python -*-
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
LLVM builder
"""
# ----------------------------------------------------------------------------

import os.path

from .. import targets
from ..host import host
from ..cmake import CMakeOptions
from ..defaults import LLVM_TARGETS_TO_BUILD
from .. import shell
from ..utils import printf
from ..exceptions import BuildError


class LLVM(object):

    source_dir = None
    clang_source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('llvm')
        if cls.source_dir is None:
            raise BuildError("Couldn't find LLVM source directory.")

        # Symlink clang into the llvm tree.
        cls.clang_source_dir = os.path.join(cls.source_dir, 'tools', 'clang')
        clang_checkout_dir = workspace.subdir('clang', no_exist=True)
        if not os.path.exists(cls.clang_source_dir):
            if not os.path.exists(clang_checkout_dir):
                raise BuildError("Couldn't find clang source directory."
                                 "Tried (%s and %s)" % (
                                     clang_checkout_dir,
                                     cls.clang_source_dir))

            printf("Symlink clang source directory to %s",
                   cls.clang_source_dir)
            try:
                shell.symlink(clang_checkout_dir,
                              cls.clang_source_dir,
                              relative=True)
            except Exception:
                raise BuildError("Couldn't symlink clang source directory "
                                 "into %s" % cls.clang_source_dir)

        # Symlink compiler-rt into llvm tree if exists
        compiler_rt_source_dir = os.path.join(
            cls.source_dir, 'projects', 'compiler-rt')
        compiler_rt_checkout_dir = workspace.subdir('compiler-rt')
        if not os.path.exists(compiler_rt_source_dir):
            if compiler_rt_checkout_dir is not None:
                try:
                    shell.symlink(compiler_rt_checkout_dir,
                                  compiler_rt_source_dir,
                                  relative=True)
                except OSError:
                    raise BuildError("Couldn't symlink compiler-rt source "
                                     "directory into %s" % (
                                         compiler_rt_source_dir))

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 host_llvm_build,
                 cmake,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir
        self.host_llvm_build = host_llvm_build
        self.cmake = cmake
        self.args = args

    @property
    def bin_dir(self):
        if self.args.cmake_generator == 'Xcode':
            return os.path.join(self.build_dir, 'bin', self.llvm_build_type)
        else:
            return os.path.join(self.build_dir, 'bin')

    @property
    def lib_dir(self):
        return os.path.join(self.build_dir, 'lib')

    @property
    def include_dir(self):
        return os.path.join(self.build_dir, 'include')

    @property
    def llvm_bin_path(self):
        return os.path.join(self.bin_dir, 'llvm')

    @property
    def clang_bin_path(self):
        return os.path.join(self.bin_dir, 'clang')

    @property
    def llvm_config_bin_path(self):
        return os.path.join(self.bin_dir, 'llvm-config')

    @property
    def llvm_tblgen_bin_path(self):
        return os.path.join(self.bin_dir, 'llvm-tblgen')

    @property
    def clang_tblgen_bin_path(self):
        return os.path.join(self.bin_dir, 'clang-tblgen')

    @property
    def filecheck_bin_path(self):
        return os.path.join(self.bin_dir, 'FileCheck')

    @property
    def lit_bin_path(self):
        return os.path.join(self.source_dir, 'utils', 'lit', 'lit.py')

    def _host_triple_target_arch(self):
        version_osx = self.args.darwin_deployment_version_osx
        version_ios = self.args.darwin_deployment_version_ios
        version_tvos = self.args.darwin_deployment_version_tvos
        version_watchos = self.args.darwin_deployment_version_watchos

        t = self.deployment_target
        if t == 'macosx-x86_64':
            return ("x86_64-apple-macosx" + version_osx, None)
        if t == 'iphonesimulator-i386':
            return ("i386-apple-ios" + version_ios, "X86")
        if t == 'iphonesimulator-x86_64':
            return ("x86_64-apple-ios" + version_ios, "X86")
        if t == 'iphoneos-armv7':
            return ("armv7-apple-ios" + version_ios, "ARM")
        if t == 'iphoneos-armv7s':
            return ("armv7s-apple-ios" + version_ios, "ARM")
        if t == 'iphoneos-arm64':
            return ("arm64-apple-ios" + version_ios, "AArch64")
        if t == 'appletvsimulator-x86_64':
            return ("x86_64-apple-tvos" + version_tvos, "X86")
        if t == 'appletvos-arm64':
            return ("arm64-apple-tvos" + version_tvos, "AArch64")
        if t == 'watchsimulator-i386':
            return ("i386-apple-watchos" + version_watchos, "X86")
        if t == 'watchos-armv7k':
            return ("armv7k-apple-watchos" + version_watchos, "ARM")

        raise BuildError("Unknown deployment target")

    def _c_flags(self):
        cflags = []
        cflags += self.cmake.common_cross_c_flags(self.deployment_target)
        if self.cmake.is_release_build_type(self.args.llvm_build_type):
            cflags += ['-fno-stack-protector', ]
        return cflags

    def configure(self):
        if (not self.args.reconfigure and
                self.cmake.is_configured(self.build_dir)):
            return

        printf("--- Configuring LLVM/Clang ---")
        cmake_options = CMakeOptions()
        define = cmake_options.define

        if self.args.use_gold_linker:
            define('CMAKE_EXE_LINKER_FLAGS:STRING', "-fuse-ld=gold")
            define('CMAKE_SHARED_LINKER_FLAGS:STRING', "-fuse-ld=gold")

        # Note: we set the variable:
        #
        # LLVM_TOOL_SWIFT_BUILD
        #
        # below because this script builds swift separately, and people
        # often have reasons to symlink the swift directory into
        # llvm/tools, e.g. to build LLDB.
        cflags = " ".join(self._c_flags())
        define('CMAKE_C_FLAGS', cflags)
        define('CMAKE_CXX_FLAGS', cflags)
        define('CMAKE_BUILD_TYPE:STRING', self.args.llvm_build_type)
        define('LLVM_ENABLE_ASSERTIONS:BOOL', self.args.llvm_enable_assertions)
        define('LLVM_TOOL_SWIFT_BUILD:BOOL', False)
        define('LLVM_TARGETS_TO_BUILD', LLVM_TARGETS_TO_BUILD)
        define('LLVM_INCLUDE_TESTS:BOOL', self.args.source_tree_includes_tests)
        define('LLVM_INCLUDE_DOCS:BOOL', True)

        # Darwin specific
        if targets.is_darwin_type(self.deployment_target):
            (sys, arch) = targets.split(self.deployment_target)
            (host_triple, target_arch) = (self._host_triple_target_arch())
            sdk_path = host.sdk_path(sys)

            cmake_osx_deployment_target = (
                self.args.darwin_deployment_version_osx)
            if sys != 'macosx':
                cmake_osx_deployment_target = ""

            define('CMAKE_OSX_DEPLOYMENT_TARGET', cmake_osx_deployment_target)
            define('CMAKE_OSX_SYSROOT:PATH', sdk_path)
            define('LLVM_HOST_TRIPLE:STRING', host_triple)
            define('LLVM_ENABLE_LIBCXX:BOOL', True)
            define('LLVM_TOOL_COMPILER_RT_BUILD:BOOL',
                   not self.args.skip_compiler_rt)
            define('COMPILER_RT_ENABLE_IOS:BOOL',
                   not self.args.skip_build_ios)
            define('COMPILER_RT_ENABLE_WATCHOS:BOOL', False)
            define('COMPILER_RT_ENABLE_TVOS:BOOL', False)

            if self.args.llvm_enable_lto:
                if self.cmake.needs_to_specify_standard_computed_defaults():
                    define('CMAKE_C_STANDARD_COMPUTED_DEFAULT', 'AppleClang')
                    define('CMAKE_CXX_STANDARD_COMPUTED_DEFAULT', 'AppleClang')
                define('CMAKE_C_FLAGS',
                       '-O2 -gline-tables-only -fno-stack-protector')
                define('CMAKE_CXX_FLAGS',
                       '-O2 -gline-tables-only -fno-stack-protector')
                define('CMAKE_C_FLAGS_RELWITHDEBINFO',
                       '-O2 -gline-tables-only -fno-stack-protector')
                define('CMAKE_CXX_FLAGS_RELWITHDEBINFO',
                       '-O2 -gline-tables-only -fno-stack-protector')

                # Currently with -gline-tables-only clang is ~3.5GB on Darwin.
                # Use the formula GB Memory/3.5GB to get the number of parallel
                # link threads we can support.
                define('LLVM_PARALLEL_LINK_JOBS',
                       self.cmake.num_parallel_lto_link_jobs(3.5))

            if target_arch is not None:
                define('LLVM_TARGET_ARCH', target_arch)

        # Vender
        if self.args.compiler_vendor == 'none':
            pass
        elif self.args.compiler_vendor == 'apple':
            define('CLANG_VENDOR', 'Apple')
            define('CLANG_VENDOR_UTI', 'com.apple.compilers.llvm.clang')
            define('PACKAGE_VERSION', self.args.clang_user_visible_version)
        else:
            raise BuildError('unknown compiler vendor')

        # Install prefix
        define('CMAKE_INSTALL_PREFIX:PATH', self.args.install_prefix)
        define('INTERNAL_INSTALL_PREFIX', 'local')

        if self.args.clang_compiler_version is not None:
            define('CLANG_REPOSITORY_STRING',
                   "clang-%s" % self.args.clang_compiler_version)

        if self.deployment_target != self.args.host_target:
            llvm_tblgen = self.host_llvm_build.llvm_tblgen_bin_path
            clang_tblgen = self.host_llvm_build.clang_tblgen_bin_path
            define('LLVM_TABLEGEN', llvm_tblgen)
            define('CLANG_TABLEGEN', clang_tblgen)

        # Do configure
        self.cmake.configure(
            source_dir=self.source_dir,
            build_dir=self.build_dir,
            options=cmake_options)

    def build(self):
        build_targets = ['all', ]

        if not self.args.build_llvm:
            # FIXME: I cannot imagine anyone using `--build-llvm=0` option.
            build_targets = ['clean', ]

        if self.args.skip_build_llvm:
            # We can't skip the build completely because the standalone
            # build of Swift depend on these.
            build_targets = ['llvm-config', 'llvm-tblgen', 'clang-headers']

        config_opts = []
        if self.args.cmake_generator == 'Xcode':
            config_opts += [
                '--config', self.args.cmark_build_type]

        # Do build
        printf("--- Building LLVM/Clang ---")
        self.cmake.build(
            build_dir=self.build_dir,
            build_targets=build_targets,
            config_opts=config_opts)

        # Create symlinks to the c++ headers.

        built_cxx_include_link = os.path.join(
            self.build_dir, 'include', 'c++')

        # Find the location of the c++ header dir.
        if host.is_darwin():
            host_cxx_dir = os.path.dirname(self.cmake.host_cxx)
            host_cxx_headers_dir = os.path.join(
                host_cxx_dir, '..', '..', 'usr', 'include', 'c++')
        else:
            # Linux etc.
            host_cxx_headers_dir = os.path.normpath('/usr/include/c++')

        printf("Symlinking the system headers ({0}) into the local "
               "clang build directory ({1}).",
               host_cxx_headers_dir, built_cxx_include_link)

        try:
            if os.path.exists(built_cxx_include_link):
                shell.remove(built_cxx_include_link)
            shell.symlink(host_cxx_headers_dir, built_cxx_include_link)
        except OSError:
            raise BuildError("Couldn't symlink clang source "
                             "directory into %s" % (
                                 built_cxx_include_link))

    def test(self):
        pass  # We don't test LLVM

    def install(self):
        if not len(self.args.llvm_install_components):
            return

        install_targets = ["install-" + component for component in
                           self.args.llvm_install_components]
        printf("--- Installing LLVM/Clang ---")
        self.cmake.install(
            build_dir=self.build_dir,
            dest_dir=self.install_destdir,
            install_targets=install_targets)
