# swift_build_support/products/libxml2.py -----------------------*- python -*-
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

class LibXML2(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def builder_class(cls):
        return LibXML2Builder


class LibXML2Builder(object):
    def __init__(self, product_class, args, toolchain, workspace, host):
        self.__source_dir = workspace.source_dir(
            product_class.product_source_name())

    @property
    def static_lib_path(self):
        # TODO: this path depends on the host and libxml2 build system
        return os.path.join(
            self.__source_dir, 'win32', 'bin.msvc', 'libxml2_a.lib')

    @property
    def include_dir(self):
        return os.path.join(self.__source_dir, 'include')

    def do_build(self):
        # TODO: this invocation should configure depending on the host
        with shell.pushd(os.path.join(self.__source_dir, 'win32')):
            shell.call(['cscript', 'configure.js', 'iconv=no'])
            shell.call(['nmake', '/f', 'Makefile.msvc'])

    def do_test(self):
        pass # No testing

    def do_install(self):
        # TODO
        pass
