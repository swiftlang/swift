# -*- python -*-
# swift_build_support/toolchain.py - Detect host machine's versioned
# executables
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

import platform
import subprocess

from . import xcrun
from .which import which


class Toolchain(object):
    """
    A class that stores accessible paths to system commands.

    It requires a 'cc' and 'cxx' tool in its keyword args.
    """
    def __init__(self, cc=None, cxx=None, **kwargs):
        self.cc = cc
        self.cxx = cxx
        self.tools = [cc, cxx]
        for tool in kwargs:
            path = kwargs[tool]
            self.tools.append(path)
            setattr(self, tool, path)


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


def _first_common_toolchain(tools, suffixes=None):
    """
    Return a Toolchain of resolved paths where each path has
    the same suffix.

    If there is no common version of all binaries found, return None.
    """
    if suffixes is None:
        # No suffixes provided, default to using empty suffix only
        suffixes = ['']

    for suffix in suffixes:
        path_map = dict()
        for name in tools:
            tool = tools[name]
            path = which(tool + suffix)
            if not path:
                break
            path_map[name] = path
        if len(path_map) == len(tools):
            return Toolchain(**path_map)
    return None


def host_toolchain(xcrun_toolchain='default', tools=None, suffixes=None):
    """
    Return a Toolchain with the first available versions of all
    specified tools, plus clang and clang++, searching in the order of the
    given suffixes.

    If no matching executables are found, return None.
    """
    if tools is None:
        tools = dict()

    # Require cc and cxx for a toolchain; this ensures whatever llvm tool we
    # find matches the version of clang returned by a previous run of
    # this tool
    tools['cc'] = 'clang'
    tools['cxx'] = 'clang++'

    if platform.system() == 'Darwin':
        # Only use xcrun on Darwin
        path_map = {}
        for name in tools:
            tool = tools[name]
            path = xcrun.find(xcrun_toolchain, tool)
            if not path:
                return None
            path_map[name] = path
        return Toolchain(**path_map)
    else:
        return _first_common_toolchain(tools, suffixes=suffixes)


def host_clang(xcrun_toolchain):
    """
    Return a Toolchain for the host platform.
    If no appropriate compilers can be found, return None.
    """
    if platform.system() == 'FreeBSD':
        # See: https://github.com/apple/swift/pull/169
        # Building Swift from source requires a recent version of the Clang
        # compiler with C++14 support.
        freebsd_release_date = _freebsd_release_date()
        if freebsd_release_date and freebsd_release_date >= 1100000:
            # On newer releases of FreeBSD, the default Clang is sufficient.
            return host_toolchain(xcrun_toolchain)
        else:
            # On older releases, or on releases for which we cannot determine
            # the release date, we search for the most modern version
            # available.
            return host_toolchain(xcrun_toolchain,
                                  suffixes=['38', '37', '36', '35'])
    return host_toolchain(xcrun_toolchain,
                          suffixes=['', '-3.8', '-3.7', '-3.6', '-3.5'])
