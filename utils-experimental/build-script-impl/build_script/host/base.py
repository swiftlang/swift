# build_script/host/base.py -------------------------------------*- python -*-
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
Host object base class
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

from ..utils import which


class Base(object):
    """Represents native host system.

    Attributes:
        install_prefix (str): The path at which built Swift products (like
            bin, lib, and include) will be installed. The default value
            depends on the host machine's operating system.

        deployment_target(str): LLVM, Clang, and Swift will be built for
            this target. The default value is determined from machine's
            operation system and CPU architecture.
    """

    def __init__(self, os_type, deployment_target):
        self._os_type = os_type
        self._deployment_target = deployment_target

    def deployment_target(self):
        return self._deployment_target

    def os_type(self):
        return self._os_type

    def is_darwin(self):
        return self.os_type() == "Darwin"

    def is_freebsd(self):
        return self.os_type() == "FreeBSD"

    def is_linux(self):
        return self.os_type() == "Linux"

    def is_windows(self):
        return self.os_type() == "Windows"

    def find_clang_cc(self):
        '''Return Clang host tool path if found. `None` otherwise.
        '''
        raise NotImplementedError()

    def find_clang_cxx(self):
        '''Return Clang host tool path if found. `None` otherwise.
        '''
        raise NotImplementedError()

    def find_cmake(self):
        '''Return CMake host tool path if found. `None` otherwise.
        '''
        raise NotImplementedError()

    def find_ninja(self):
        '''Return Ninja host tool path if found. `None` otherwise.
        '''
        return which('ninja') or which('ninja-build')

    def find_distcc(self):
        '''Return distcc host tool path if found. `None` otherwise.
        '''
        return which('distcc')

    def find_distcc_pump(self):
        '''Return distcc-pump host tool path if found. `None` otherwise.
        '''
        return which('distcc-pump') or which('pump')
