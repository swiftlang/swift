# swift_build_support/products/cmark.py -------------------------*- python -*-
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

from . import product
from .. import shell


class CMark(product.Product):
    @classmethod
    def builder_class(cls):
        return CMarkBuilder

class CMarkBuilder(product.CMakeProductBuilder):
    def __init__(self, product_class, args, toolchain, workspace, host):
        product.CMakeProductBuilder.__init__(self, product_class, args,
                                             toolchain, workspace, host)

        self._cmake_options.define('CMAKE_BUILD_TYPE:STRING', self._build_variant)

        if self._host.platform.is_darwin:
            cmark_c_flags = self._common_cross_cflags
            if self._is_release_build_variant:
                cmark_c_flags += ' -fno-stack-protector'
            self._cmake_options.define('CMAKE_C_FLAGS', cmark_c_flags)
            self._cmake_options.define('CMAKE_CXX_FLAGS', cmark_c_flags)
            self._cmake_options.define('CMAKE_OSX_SYSROOT:PATH', shell.capture(['xcrun', '--sdk', host.platform.name, '--show-sdk-path']))
            if self._host.name == 'macosx-x86_64':
                self._cmake_options.define('CMAKE_OSX_DEPLOYMENT_TARGET', self._args.darwin_deployment_version_osx)

    @property
    def _build_variant(self):
        return self._args.cmark_build_variant

    @property
    def _test_executable_target(self):
        return 'api_test'

    @property
    def _test_results_targets(self):
        if self._args.cmake_generator == 'Xcode':
            return ['RUN_TESTS']
        else:
            return ['test']