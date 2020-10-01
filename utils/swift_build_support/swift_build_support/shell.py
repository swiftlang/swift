# swift_build_support/shell.py ----------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
# ----------------------------------------------------------------------------
"""
Centralized command line and file system interface for the build script.
"""
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import pipes
import platform
import shutil
import subprocess
import sys
from contextlib import contextmanager


DEVNULL = getattr(subprocess, 'DEVNULL', subprocess.PIPE)

dry_run = False


def _fatal_error(message):
    """Raises a SystemExit error with the given message.
    """

    raise SystemExit('ERROR: {}\n'.format(message))


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


def _echo_command(dry_run, command, env=None, prompt="+ "):
    output = []
    if env is not None:
        output += ['env'] + [_quote("%s=%s" % (k, v))
                             for (k, v) in sorted(env.items())]
    output += [_quote(arg) for arg in command]
    file = sys.stderr
    if dry_run:
        file = sys.stdout
    print(prompt + ' '.join(output), file=file)
    file.flush()


def call(command, stderr=None, env=None, dry_run=None, echo=True):
    """
    call(command, ...) -> str

    Execute the given command.

    This function will raise an exception on any command failure.
    """
    dry_run = _coerce_dry_run(dry_run)
    if dry_run or echo:
        _echo_command(dry_run, command, env=env)
    if dry_run:
        return
    _env = None
    if env is not None:
        _env = dict(os.environ)
        _env.update(env)
    try:
        subprocess.check_call(command, env=_env, stderr=stderr)
    except subprocess.CalledProcessError as e:
        _fatal_error(
            "command terminated with a non-zero exit status " +
            str(e.returncode) + ", aborting")
    except OSError as e:
        _fatal_error(
            "could not execute '" + quote_command(command) +
            "': " + e.strerror)


def call_without_sleeping(command, env=None, dry_run=False, echo=False):
    """
    Execute a command during which system sleep is disabled.

    By default, this ignores the state of the `shell.dry_run` flag.
    """

    # Disable system sleep, if possible.
    if platform.system() == 'Darwin':
        # Don't mutate the caller's copy of the arguments.
        command = ["caffeinate"] + list(command)

    call(command, env=env, dry_run=dry_run, echo=echo)


def capture(command, stderr=None, env=None, dry_run=None, echo=True,
            optional=False, allow_non_zero_exit=False):
    """
    capture(command, ...) -> str

    Execute the given command and return the standard output.

    This function will raise an exception on any command failure.
    """
    dry_run = _coerce_dry_run(dry_run)
    if dry_run or echo:
        _echo_command(dry_run, command, env=env)
    if dry_run:
        return
    _env = None
    if env is not None:
        _env = dict(os.environ)
        _env.update(env)
    try:
        out = subprocess.check_output(command, env=_env, stderr=stderr)
        # Coerce to `str` hack. not py3 `byte`, not py2 `unicode`.
        return str(out.decode())
    except subprocess.CalledProcessError as e:
        if allow_non_zero_exit:
            return str(e.output.decode())
        if optional:
            return None
        _fatal_error(
            "command terminated with a non-zero exit status " +
            str(e.returncode) + ", aborting")
    except OSError as e:
        if optional:
            return None
        _fatal_error(
            "could not execute '" + quote_command(command) +
            "': " + e.strerror)


@contextmanager
def pushd(path, dry_run=None, echo=True):
    dry_run = _coerce_dry_run(dry_run)
    old_dir = os.getcwd()
    if dry_run or echo:
        _echo_command(dry_run, ["pushd", path])
    if not dry_run:
        os.chdir(path)
    yield
    if dry_run or echo:
        _echo_command(dry_run, ["popd"])
    if not dry_run:
        os.chdir(old_dir)


def makedirs(path, dry_run=None, echo=True):
    dry_run = _coerce_dry_run(dry_run)
    if dry_run or echo:
        _echo_command(dry_run, ['mkdir', '-p', path])
    if dry_run:
        return
    if not os.path.isdir(path):
        os.makedirs(path)


def rmtree(path, dry_run=None, echo=True):
    dry_run = _coerce_dry_run(dry_run)
    if dry_run or echo:
        _echo_command(dry_run, ['rm', '-rf', path])
    if dry_run:
        return
    if os.path.exists(path):
        shutil.rmtree(path)


def copytree(src, dest, dry_run=None, echo=True):
    dry_run = _coerce_dry_run(dry_run)
    if dry_run or echo:
        _echo_command(dry_run, ['cp', '-r', src, dest])
    if dry_run:
        return
    shutil.copytree(src, dest)


def symlink(source, dest, dry_run=None, echo=True):
    dry_run = _coerce_dry_run(dry_run)
    if dry_run or echo:
        _echo_command(dry_run, ['ln', '-s', source, dest])
    if dry_run:
        return
    os.symlink(source, dest)


# Initialized later
lock = None


def run(*args, **kwargs):
    repo_path = os.getcwd()
    echo_output = kwargs.pop('echo', False)
    dry_run = kwargs.pop('dry_run', False)
    env = kwargs.pop('env', None)
    if dry_run:
        _echo_command(dry_run, *args, env=env)
        return(None, 0, args)

    my_pipe = subprocess.Popen(
        *args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)
    (stdout, stderr) = my_pipe.communicate()
    ret = my_pipe.wait()

    if lock:
        lock.acquire()
    if echo_output:
        print(repo_path)
        _echo_command(dry_run, *args, env=env)
        if stdout:
            print(stdout, end="")
        if stderr:
            print(stderr, end="")
        print()
    if lock:
        lock.release()

    if ret != 0:
        eout = Exception()
        eout.ret = ret
        eout.args = args
        eout.repo_path = repo_path
        eout.stderr = stderr
        raise eout
    return (stdout, 0, args)
