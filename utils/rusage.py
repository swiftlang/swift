#!/usr/bin/python
# utils/rusage.py - Utility to measure resource usage -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#
# Simple informative/supervisory wrapper around getrusage() and, optionally,
# setrlimit()
#
# By default this script lets its subprocess run to completion whatever its
# consumed limits, and only prints a limit-violation if it occurs after the
# fact. If you want to be a bit more abrupt you can run with --enforce and
# it will also run subprocesses under setrlimit().
#
# Note: setrlimit(RLIMIT_RSS) does nothing on macOS. It's unenforced.
#
# If there's a limit-violation observed (by getrusage()), it's printed to
# stderr and the wrapper exits with an error. Whether or not the violation
# was caused by enforcement or mere observation.
#
# If there's no limit-violation, the subprocess' own exit code is
# propagated, either silently or, if --verbose is passed, after printing
# the actual getrusage() memory and time values (such that they can be used
# as a limit in future runs).
#

import argparse
import resource
import subprocess
import sys


class MemAction(argparse.Action):
    def __init__(self, *args, **kwargs):
        super(MemAction, self).__init__(*args, **kwargs)

    def __call__(self, parser, namespace, v, option_string=None):
        r = None
        if v.endswith('K'):
            r = int(v[:-1]) * 1024
        elif v.endswith('M'):
            r = int(v[:-1]) * 1024 * 1024
        elif v.endswith('G'):
            r = int(v[:-1]) * 1024 * 1024 * 1024
        else:
            r = int(v)
        setattr(namespace, self.dest, r)


class TimeAction(argparse.Action):
    def __init__(self, *args, **kwargs):
        super(TimeAction, self).__init__(*args, **kwargs)

    def __call__(self, parser, namespace, v, option_string=None):
        r = None
        if v.endswith('ms'):
            r = float(v[:-2]) / 1000.0
        elif v.endswith('us'):
            r = float(v[:-2]) / 1000000.0
        else:
            r = float(v)
        setattr(namespace, self.dest, r)


parser = argparse.ArgumentParser()
parser.add_argument("--mem",
                    metavar="M",
                    help="memory (in bytes, or ..'K', 'M', 'G')",
                    action=MemAction)
parser.add_argument("--time",
                    metavar="T",
                    help="time (in secs, or ..'ms', 'us')",
                    action=TimeAction)
parser.add_argument("--enforce",
                    action='store_true',
                    default=False,
                    help="call setrlimit() before running subprocess")
parser.add_argument("--verbose",
                    action='store_true',
                    default=False,
                    help="always report status and usage")
parser.add_argument('remainder', nargs=argparse.REMAINDER,
                    help="subcommand to run under supervision")

args = parser.parse_args()
if len(args.remainder) == 0:
    parser.print_help()
    sys.exit(1)

if args.enforce:
    if args.time is not None:
        secs = max(1, int(args.time))
        (soft, hard) = resource.getrlimit(resource.RLIMIT_CPU)
        secs = min(soft, hard, secs)
        if args.verbose:
            sys.stderr.write("rusage: setrlimit(RLIMIT_CPU, %d)\n"
                             % secs)
        resource.setrlimit(resource.RLIMIT_CPU, (secs, secs))
    if args.mem is not None:
        (soft, hard) = resource.getrlimit(resource.RLIMIT_RSS)
        mem = min(soft, hard, args.mem)
        if args.verbose:
            sys.stderr.write("rusage: setrlimit(RLIMIT_RSS, %d)\n"
                             % mem)
        resource.setrlimit(resource.RLIMIT_RSS, (mem, mem))
ret = subprocess.call(args.remainder)
used = resource.getrusage(resource.RUSAGE_CHILDREN)

if args.verbose:
    sys.stderr.write("rusage: subprocess exited %d\n" % ret)

over_mem = args.mem is not None and used.ru_maxrss > args.mem
over_time = args.time is not None and used.ru_utime > args.time

if args.verbose or over_mem:
    sys.stderr.write("rusage: subprocess mem: %d bytes\n"
                     % used.ru_maxrss)
if over_mem:
    sys.stderr.write("rusage: exceeded limit: %d bytes\n"
                     % args.mem)

if args.verbose or over_time:
    sys.stderr.write("rusage: subprocess time: %.6f secs\n"
                     % used.ru_utime)
if over_time:
    sys.stderr.write("rusage:  exceeded limit: %.6f secs\n"
                     % args.time)

if over_mem or over_time:
    sys.exit(-1)
sys.exit(ret)
