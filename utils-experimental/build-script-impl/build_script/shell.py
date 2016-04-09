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
Centralized command line and file system interface for the build script.
"""
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import sys
import subprocess

from . import _shell

echo = True
dry_run = False

# FIXME: I hate this kind of silent mode change.
os.umask(0o022)


def query(args, env=None, stderr=subprocess.PIPE):
    return _shell.query(args, env=env, stderr=stderr,
                        echo=echo)


def execv(args):
    _shell.execv(args,
                 echo=echo, dry_run=dry_run)


def invoke(args, env=None):
    _shell.invoke(args, env=env,
                  echo=echo, dry_run=dry_run)


def copy(src, dst):
    _shell.copy(src, dst,
                echo=echo, dry_run=dry_run)


def symlink(source, link_name):
    _shell.symlink(source, link_name,
                   echo=echo, dry_run=dry_run)


def remove(path):
    _shell.remove(path,
                  echo=echo, dry_run=dry_run)


def makedirs(directory):
    _shell.makedirs(directory,
                  echo=echo, dry_run=dry_run)


def rmtree(path):
    _shell.rmtree(path,
                  echo=echo, dry_run=dry_run)


def copytree(src, dst, symlinks=False):
    _shell.copytree(src, dst, symlink,
                    echo=echo, dry_run=dry_run)

def pushd(path):
    return _shell.pushd(path,
                        echo=echo, dry_run=dry_run)

def chdir(path):
    _shelll.chdir(path,
                  echo=echo, dry_run=dry_run)
