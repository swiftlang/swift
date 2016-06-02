# swift_build_support/shell.py ----------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
# ----------------------------------------------------------------------------
"""
Centralized command line and file system interface for the build script.
"""
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import pipes
import shutil
import subprocess
import sys
from contextlib import contextmanager

dry_run = False


def _quote(arg):
    return pipes.quote(str(arg))


def quote_command(args):
    """
    quote_command(args) -> str

    Quote the command for passing to a shell.
    """
    return ' '.join([_quote(a) for a in args])


def _coerce_dry_run(dry_run_override):
    if dry_run_override is None:
        return dry_run
    else:
        return dry_run_override


def _print_command(dry_run, command, env=None, prompt="+ "):
    output = []
    if env is not None:
        output += ['env'] + [_quote("%s=%s" % (k, v)) for k, v in env]
    output += [_quote(arg) for arg in command]
    file = sys.stderr
    if dry_run:
        file = sys.stdout
    print(prompt + ' '.join(output), file=file)
    file.flush()


def call(command, stderr=None, env=None, dry_run=None):
    dry_run = _coerce_dry_run(dry_run)
    _print_command(dry_run, command, env=env)
    if dry_run:
        return
    _env = None
    if env is not None:
        _env = dict(os.environ)
        _env.update(env)
    subprocess.check_call(command, env=_env, stderr=stderr)


@contextmanager
def pushd(path, dry_run=None):
    dry_run = _coerce_dry_run(dry_run)
    old_dir = os.getcwd()
    _print_command(dry_run, ["pushd", path])
    if not dry_run:
        os.chdir(path)
    yield
    _print_command(dry_run, ["popd"])
    if not dry_run:
        os.chdir(old_dir)


def makedirs(path, dry_run=None):
    dry_run = _coerce_dry_run(dry_run)
    _print_command(dry_run, ['mkdir', '-p', path])
    if dry_run:
        return
    if not os.path.isdir(path):
        os.makedirs(path)


def rmtree(path, dry_run=None):
    dry_run = _coerce_dry_run(dry_run)
    _print_command(dry_run, ['rm', '-rf', path])
    if dry_run:
        return
    if os.path.exists(path):
        shutil.rmtree(path)


def copytree(src, dest, dry_run=None):
    dry_run = _coerce_dry_run(dry_run)
    _print_command(dry_run, ['cp', '-r', src, dest])
    if dry_run:
        return
    shutil.copytree(src, dest)
