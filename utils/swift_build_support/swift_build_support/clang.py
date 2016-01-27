# swift_build_support/clang.py - Detect host machine's Clang -*- python -*-
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
#
# Find the path to a Clang executable on the host machine that is most
# suitable for building Swift.
#
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import collections
import platform
import subprocess

from . import xcrun
from .which import which


# A named tuple consisting of two paths:
# 1. 'cc' is the path to a program used for compiling C.
# 2. 'cxx' is the path to a program used for compiling C++.
CompilerExecutable = collections.namedtuple('CompilerExecutable', 'cc cxx')


def _freebsd_release_date():
    """
    Return the release date for FreeBSD operating system on this host.
    If the release date cannot be ascertained, return None.
    """
    try:
        # For details on `sysctl`, see:
        # http://www.freebsd.org/cgi/man.cgi?sysctl(8)
        return int(subprocess.check_output(
            ['sysctl', '-n', 'kern.osreldate']).rstrip())
    except subprocess.CalledProcessError:
        return None


def _first_clang(suffixes):
    """
    Return a CompilerExecutable with the first available versions of clang
    and clang++, searching in the order of the given suffixes.

    If no Clang executables are found, return None.
    """
    for suffix in suffixes:
        cc_path = which('clang{}'.format(suffix))
        cxx_path = which('clang++{}'.format(suffix))
        if cc_path and cxx_path:
            return CompilerExecutable(cc=cc_path, cxx=cxx_path)

    return None


def host_clang(xcrun_toolchain):
    """
    Return a CompilerExecutable for the host platform.
    If no appropriate compilers can be found, return None.
    """
    if platform.system() == 'Darwin':
        cc = xcrun.find(xcrun_toolchain, 'clang')
        cxx = xcrun.find(xcrun_toolchain, 'clang++')
        if cc and cxx:
            return CompilerExecutable(cc=cc, cxx=cxx)
        else:
            return None
    elif platform.system() == 'FreeBSD':
        # See: https://github.com/apple/swift/pull/169
        # Building Swift from source requires a recent version of the Clang
        # compiler with C++14 support.
        freebsd_release_date = _freebsd_release_date()
        if freebsd_release_date and freebsd_release_date >= 1100000:
            # On newer releases of FreeBSD, the default Clang is sufficient.
            return CompilerExecutable(cc='clang', cxx='clang++')
        else:
            # On older releases, or on releases for which we cannot determine
            # the release date, we search for the most modern version
            # available.
            return _first_clang(['38', '37', '36', '35'])
    else:
        return _first_clang(['', '-3.8', '-3.7', '-3.6', '-3.5'])
