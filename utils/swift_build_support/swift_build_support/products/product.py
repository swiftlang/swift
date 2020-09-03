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

import abc
import os

from .. import cmake
from .. import  shell
from .. import targets


def is_release_variant(build_variant):
    return build_variant in ['Release', 'RelWithDebInfo']


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

        llvm_projects = ['clang',
                         'clang-tools-extra',
                         'compiler-rt',
                         'libcxx',
                         'lldb',
                         'llvm']

        if cls.product_name() in llvm_projects:
            return "llvm-project/{}".format(cls.product_name())
        return cls.product_name()

    @classmethod
    def is_build_script_impl_product(cls):
        """is_build_script_impl_product -> bool

        Whether this product is produced by build-script-impl.
        """
        raise NotImplementedError

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        """is_swiftpm_unified_build_product -> bool

        Whether this product should be built in the unified build of SwiftPM
        products.
        """
        return False

    @classmethod
    def is_nondarwin_only_build_product(cls):
        """Returns true if this target should be skipped in darwin builds when
        inferring dependencies.
        """
        return False

    @classmethod
    def get_dependencies(cls):
        """Return a list of products that this product depends upon"""
        raise NotImplementedError

    def should_clean(self, host_target):
        """should_clean() -> Bool

        Whether or not this product should be cleaned before being built
        """
        return False

    def clean(self, host_target):
        """clean() -> void

        Perform the clean, for a non-build-script-impl product.
        """
        raise NotImplementedError

    def should_build(self, host_target):
        """should_build() -> Bool

        Whether or not this product should be built with the given arguments.
        """
        raise NotImplementedError

    def build(self, host_target):
        """build() -> void

        Perform the build, for a non-build-script-impl product.
        """
        raise NotImplementedError

    def should_test(self, host_target):
        """should_test() -> Bool

        Whether or not this product should be tested with the given arguments.
        """
        raise NotImplementedError

    def test(self, host_target):
        """test() -> void

        Run the tests, for a non-build-script-impl product.
        """
        raise NotImplementedError

    def should_install(self, host_target):
        """should_install() -> Bool

        Whether or not this product should be installed with the given
        arguments.
        """
        raise NotImplementedError

    def install(self, host_target):
        """install() -> void

        Install to the toolchain, for a non-build-script-impl product.
        """
        raise NotImplementedError

    def __init__(self, args, toolchain, source_dir, build_dir):
        """
        Parameters
        ----------
        args : `argparse.Namespace`
            The arguments passed by the user to the invocation of the script.
        toolchain : `swift_build_support.toolchain.Toolchain`
            The toolchain being used to build the product. The toolchain will
            point to the tools that the builder should use to build (like the
            compiler or the linker).
        build_dir: string
            The directory in which the product should put all of its build
            products.
        """
        self.args = args
        self.toolchain = toolchain
        self.source_dir = source_dir
        self.build_dir = build_dir
        self.cmake_options = cmake.CMakeOptions()

    def is_release(self):
        """is_release() -> Bool

        Whether or not this target is built as a release variant
        """
        return is_release_variant(self.args.build_variant)

    def install_toolchain_path(self, host_target):
        """toolchain_path() -> string

        Returns the path to the toolchain that is being created as part of this
        build.
        """
        install_destdir = self.args.install_destdir
        if self.args.cross_compile_hosts:
            build_root = os.path.dirname(self.build_dir)
            install_destdir = '%s/intermediate-install/%s' % (build_root, host_target)
        return targets.toolchain_path(install_destdir,
                                      self.args.install_prefix)

    def build_with_cmake(self, build_targets, build_type, build_args):
        assert self.toolchain.cmake is not None
        cmake_build = []
        _cmake = cmake.CMake(self.args, self.toolchain)

        if self.toolchain.distcc_pump:
            cmake_build.append(self.toolchain.distcc_pump)
        cmake_build.extend([self.toolchain.cmake, "--build"])

        generator_output_path = ""
        if self.args.cmake_generator == "Ninja":
            generator_output_path = os.path.join(self.build_dir, "build.ninja")

        cmake_cache_path = os.path.join(self.build_dir, "CMakeCache.txt")
        if self.args.reconfigure or not os.path.isfile(cmake_cache_path) or \
            (generator_output_path and not os.path.isfile(generator_output_path)):
            if not os.path.exists(self.build_dir):
                os.makedirs(self.build_dir)

            # Use `cmake-file-api` in case it is available.
            query_dir = os.path.join(self.build_dir, ".cmake", "api", "v1", "query")
            if not os.path.exists(query_dir):
                os.makedirs(query_dir)
            open(os.path.join(query_dir, "codemodel-v2"), 'a').close()
            open(os.path.join(query_dir, "cache-v2"), 'a').close()

            env = None
            if self.toolchain.distcc:
                env = {
                    "DISTCC_HOSTS": "localhost,lzo,cpp"
                }

            with shell.pushd(self.build_dir):
                shell.call([self.toolchain.cmake] + list(self.cmake_options) + \
                            list(_cmake.common_options()) + \
                            self.args.extra_cmake_options + [self.source_dir],
                            env=env)

        if not self.args.skip_build or self.product_name() == "llvm":
            if self.args.cmake_generator == "Xcode":
                # Xcode generator uses "ALL_BUILD" instead of "all".
                # Also, xcodebuild uses -target instead of bare names.
                build_targets = build_targets.copy()
                build_targets = [val for target in build_targets \
                                    for val in ["-target", target if target != "all" else "ALL_BUILD"]]

                # Xcode can't restart itself if it turns out we need to reconfigure.
                # Do an advance build to handle that.
                shell.call(cmake_build + [self.build_dir, build_type])

            shell.call(cmake_build + [self.build_dir, "--config", build_type, "--"] + build_args + build_targets)


class ProductBuilder(object):
    """
    Abstract base class for all ProductBuilders.

    An specific ProductBuilder will implement the interface methods depending
    how the product want to be build. Multiple products can use the same
    product builder if parametrized right (for example all the products build
    using CMake).

    Ideally a ProductBuilder will be initialized with references to the
    invocation arguments, the calculated toolchain, the calculated workspace,
    and the target host, but the base class doesn't impose those requirements
    in order to be flexible.

    NOTE: Python doesn't need an explicit abstract base class, but it helps
    documenting the interface.
    """

    @abc.abstractmethod
    def __init__(self, product_class, args, toolchain, workspace):
        """
        Create a product builder for the given product class.

        Parameters
        ----------
        product_class : class
            A subtype of `Product` which describes the product being built by
            this builder.
        args : `argparse.Namespace`
            The arguments passed by the user to the invocation of the script. A
            builder should consider this argument read-only.
        toolchain : `swift_build_support.toolchain.Toolchain`
            The toolchain being used to build the product. The toolchain will
            point to the tools that the builder should use to build (like the
            compiler or the linker).
        workspace : `swift_build_support.workspace.Workspace`
            The workspace where the source code and the build directories have
            to be located. A builder should use the workspace to access its own
            source/build directory, as well as other products source/build
            directories.
        """
        pass

    @abc.abstractmethod
    def build(self):
        """
        Perform the build phase for the product.

        This phase might also imply a configuration phase, but each product
        builder is free to determine how to do it.
        """
        pass

    @abc.abstractmethod
    def test(self):
        """
        Perform the test phase for the product.

        This phase might build and execute the product tests.
        """
        pass

    @abc.abstractmethod
    def install(self):
        """
        Perform the install phase for the product.

        This phase might copy the artifacts from the previous phases into a
        destination directory.
        """
        pass
