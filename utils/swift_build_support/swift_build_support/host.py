# swift_build_support/host.py ----------- Migrating build-script -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# -----------------------------------------------------------------------------
#
# This file contains routines for determining information about the host for
# use in utils/build-script.
#
# -----------------------------------------------------------------------------

from __future__ import absolute_import

import platform

from . import shell


# Utilities
def _return_none_fun():
    return None


def _return_none_fun_pair():
    return (_return_none_fun, _return_none_fun)


def _compute_system_key():
    return (platform.system(), platform.machine())


# System Memory
def _darwin_system_memory():
    # Output looks like "hw.memsize: \d+\n"
    return int(shell.capture(["sysctl", "hw.memsize"],
                             dry_run=False, echo=False,
                             optional=False).strip().split(" ")[1])

_PER_PLATFORM_SYSTEM_MEMORY = {
    ('Darwin', 'x86_64'): _darwin_system_memory
}


def system_memory():
    return _PER_PLATFORM_SYSTEM_MEMORY.get(_compute_system_key(),
                                           _return_none_fun)()


# Max Num CPU Threads for use with LTO
def _darwin_max_num_llvm_parallel_lto_link_jobs():
    # *WARNING! HEURISTIC!*
    #
    # Use the formula (GB Memory - 3)/6.0GB to get the number of
    # parallel link threads we can support. This gives the OS 3 GB of
    # room to work with.
    #
    # This is a bit conservative, but I have found that this number
    # prevents me from swapping on my test machine.
    return int((_darwin_system_memory()/1000000000.0 - 3.0)/6.0)


def _darwin_max_num_swift_parallel_lto_link_jobs():
    # *WARNING! HEURISTIC!*
    #
    # Use the formula (GB Memory - 3)/8.0GB to get the number of
    # parallel link threads we can support. This gives the OS 3 GB of
    # room to work with.
    #
    # This is a bit conservative, but I have found that this number
    # prevents me from swapping on my test machine.
    return int((_darwin_system_memory()/1000000000.0 - 3.0)/8.0)

_PER_PLATFORM_MAX_PARALLEL_LTO_JOBS = {
    ('Darwin', 'x86_64'): (_darwin_max_num_llvm_parallel_lto_link_jobs,
                           _darwin_max_num_swift_parallel_lto_link_jobs)
}


def max_lto_link_job_counts():
    key = _compute_system_key()
    info = _PER_PLATFORM_MAX_PARALLEL_LTO_JOBS.get(key,
                                                   _return_none_fun_pair())
    return {'llvm': info[0](), 'swift': info[1]()}
