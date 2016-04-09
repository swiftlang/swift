# build_script/shell.py -----------------------------------------*- python -*-
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
Implementaion detail of build_script.shell module.
"""
# ----------------------------------------------------------------------------
from __future__ import print_function

import os
import sys
import shutil
import pipes
import subprocess
from contextlib import contextmanager


def _print_line(line):
    print(line)
    # To prevent mixed output with executed command output.
    sys.stdout.flush() 

def _print_command(args, env=None):
    output = []
    q = pipes.quote

    if env is not None:
        output += ("%s=%s" % (q(k), q(v)) for k, v in env)
    output += (q(arg) for arg in args)
    _print_line('+ ' + ' '.join(output))

@contextmanager
def pushd(path, echo=True, dry_run=False):
    old_dir = os.getcwd()
    if echo or dry_run:
        _print_command(["pushd", path])
    if not dry_run:
        os.chdir(path)
    yield
    if echo or dry_run:
        _print_command(["popd"])
    if not dry_run:
        os.chdir(old_dir)

def chdir(path, echo=True, dry_run=False):
    if echo or dry_run:
        _shell.print_command(["cd", path])
    if not dry_run:
        os.chdir(path)

def query(args, env=None, stderr=subprocess.PIPE, echo=False, strip=True):
    '''
    Run command and returns its output.
    '''
    if echo:
        _print_command(args, env)

    out = subprocess.check_output(args)
    if strip:
        out = out.strip()
    return out


def execv(args, echo=True, dry_run=False):
    '''
    Execute given command, replacing current process. Never return.
    '''
    if echo or dry_run:
        _print_command(args)
        if dry_run:
            # FIXME: I'm not sure what to to for `execv` dry-run.
            sys.exit(0)

    os.execv(args[0], args)


def invoke(args, env=None, echo=True, dry_run=False):
    '''
    Invoke given command
    '''
    if echo or dry_run:
        _print_command(args, env)
        if dry_run:
            return

    if env is not None:
        subprocess.check_call(args, env=dict(env))
    else:
        subprocess.check_call(args)



def copy(src, dst, echo=True, dry_run=False):
    if echo or dry_run:
        _print_command(['cp', src, dst])
        if dry_run:
            return

    shutil.copy(src, dst)


def remove(path, echo=True, dry_run=False):
    if echo or dry_run:
        _print_command(['rm', path])
        if dry_run:
            return

    os.remove(file)


def makedirs(directory, echo=True, dry_run=False):
    if echo or dry_run:
        _print_command(['mkdir', '-p', directory])
        if dry_run:
            return

    os.makedirs(directory)


def rmtree(path, echo=True, dry_run=False):
    if echo or dry_run:
        _print_command(['rm', '-rf', path])
        if dry_run:
            return

    shutil.rmtree(path)


def copytree(src, dst, symlinks=False, echo=True, dry_run=False):
    if echo or dry_run:
        _print_command(['cp', '-r', src, dst])
        if dry_run:
            return

    shutil.copytree(src, dst, symlinks=symlinks)


def symlink(source, link_name, echo=True, dry_run=False):
    '''
    Create symbolic link
    '''

    if echo or dry_run:
        _print_command(['ln', '-s', source, link_name])
        if dry_run:
            return

    os.symlink(source, link_name)
