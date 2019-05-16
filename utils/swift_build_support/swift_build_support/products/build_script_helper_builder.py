# swift_build_support/product_builders/build_script_helper_b... -*- python -*-
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

import abc
import os
import platform

from . import product
from .. import shell, targets


class BuildScriptHelperBuilder(product.ProductBuilder):
    def __init__(self, product_class, args, toolchain, workspace, host):
        self.__source_dir = workspace.source_dir(
            product_class.product_source_name())
        self.__build_dir = workspace.build_dir(host.name,
                                               product_class.product_name())
        self.__args = args

    def build(self):
        self.__run_build_script_helper('build')

    def test(self):
        if self._should_test():
            self.__run_build_script_helper('test')

    @abc.abstractmethod
    def _should_test(self):
        pass

    def __run_build_script_helper(self, action):
        script_path = os.path.join(
            self.__source_dir, 'Utilities', 'build-script-helper.py')
        toolchain_path = self.__args.install_destdir
        if platform.system() == 'Darwin':
            # The prefix is an absolute path, so concatenate without os.path.
            toolchain_path += \
                targets.darwin_toolchain_prefix(self.__args.install_prefix)
        if self.__args.build_variant == 'Debug':
            configuration = 'debug'
        else:
            configuration = 'release'
        helper_cmd = [
            script_path,
            action,
            '--verbose',
            '--package-path', self.__source_dir,
            '--build-path', self.__build_dir,
            '--configuration', configuration,
            '--toolchain', toolchain_path,
        ]
        shell.call(helper_cmd)
