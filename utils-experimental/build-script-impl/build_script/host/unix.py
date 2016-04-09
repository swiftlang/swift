# build_script/host/unix.py -------------------------------------*- python -*-
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
Unix like host class
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import os.path
import re
import subprocess

from .base import Base
from ..utils import which, CachedProperty, printf
from .. import shell

__all__ = [
    'Linux',
    'FreeBSD',
    'Cygwin',
]


class GenericUnix(Base):

    def _find_clang(self, tool):
        for suffix in ['', '-3.8', '-3.7', '-3.6', '-3.5']:
            found = which(tool + suffix)
            if found:
                return found
        return None

    def find_clang_cc(self):
        return self._find_clang('clang')

    def find_clang_cxx(self):
        return self._find_clang('clang++')

    def find_cmake(self):
        '''\
        Return `CMake` host tool object if found. `None` otherwise.
        '''
        return which('cmake')

    def system_memory_in_bytes(self):
        # Currently, not used
        try:
            from psutil import virtual_memory
            return virtual_memory().total
        except ImportError:
            pass

        if not os.path.exists('/proc/meminfo'):
            raise NotImplementedError()

        with open('/proc/meminfo') as f:
            for line in f:
                if "MemTotal" in line:
                    return int(re.finditer('\d+').next().group(0)) * 1024

    def show_sdks(self):
        """
        Print the host machine's `clang` version
        """
        printf('{0}\n', shell.query(['clang', '--version'], echo_=False))


class Linux(GenericUnix):
    pass


class FreeBSD(GenericUnix):

    @CachedProperty
    def _release_date():
        """
        Return the release date for FreeBSD operating system on this host.
        If the release date cannot be ascertained, return None.
        """
        try:
            # For details on `sysctl`, see:
            # http://www.freebsd.org/cgi/man.cgi?sysctl(8)
            return int(shell.query(['sysctl', '-n', 'kern.osreldate']))
        except subprocess.CalledProcessError:
            return None

    def _find_clang(self, tool):
        rel_date = self._release_date
        if rel_date is None or rel_date <= 1100000:
            for suffix in ['38', '37', '36', '35']:
                found = which(tool + suffix)
                if found:
                    return found
            return None
        else:
            return which(tool)


class Cygwin(GenericUnix):
    pass
