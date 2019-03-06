# swift_build_support/products/curl.py --------------------------*- python -*-
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

import os.path

class Curl(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def builder_class(cls):
        return CurlBuilder


class CurlBuilder(object):
    def __init__(self, product_class, args, toolchain, workspace, host):
        self.__source_dir = workspace.source_dir(
            product_class.product_source_name())

    @property
    def static_lib_path(self):
        # TODO: this path depends on the host and Curl build system
        return os.path.join(
            self.__source_dir, 'builds',
            'libcurl-vc15-x64-release-static-ipv6-sspi-winssl', 'lib',
            'libcurl_a.lib')

    @property
    def include_dir(self):
        # TODO: this path depends on the host and Curl build system
        return os.path.join(
            self.__source_dir, 'builds',
            'libcurl-vc15-x64-release-static-ipv6-sspi-winssl', 'include')

    def do_build(self):
        with shell.pushd(self.__source_dir):
            shell.call(['buildconf.bat'])
        with shell.pushd(os.path.join(self.__source_dir, 'winbuild')):
            # TODO: Choose MACHINE depending on the host architecture.
            shell.call([
                'nmake', '/f', 'Makefile.vc', 'mode=static', 'VC=15',
                'MACHINE=x64'])

    def do_test(self):
        pass # No testing

    def do_install(self):
        # TODO
        pass
