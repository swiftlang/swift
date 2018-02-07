# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Shell utilities wrapper module.
"""


import os
import shutil
import subprocess
import sys
from contextlib import contextmanager
from copy import copy
from subprocess import CalledProcessError

try:
    # Python 3
    from shutil import quote
except ImportError:
    from pipes import quote


__all__ = [
    'CommandExecutor',
    'NullExecutor',

    'PIPE',
    'STDOUT',
]


# Re-export subprocess constants
PIPE = subprocess.PIPE
STDOUT = subprocess.STDOUT


class _EchoAction(object):

    DEFAULT_PREFIX = '>>> '

    def __init__(self, output_stream, prefix=None):
        self.output_stream = output_stream
        self.prefix = prefix or _EchoAction.DEFAULT_PREFIX

    def __call__(self, command):
        line = unicode(self.prefix + ' '.join(command) + '\n')
        self.output_stream.write(line)


class _FakePopen():
    """Fake class which implements the same interface as subprocess.Popen.
    """

    def __init__(self, pid=0, returncode=0):
        self.stdin = None
        self.stdout = None
        self.stderr = None
        self.pid = pid
        self.returncode = returncode

    def poll(self):
        return self.returncode

    def wait(self):
        return self.returncode

    def communicate(**kwargs):
        return (None, None)

    def send_signal(self, signal):
        return

    def terminate(self):
        return

    def kill(self):
        return


def _quote_command(command):
    return list(map(quote, command))


# -----------------------------------------------------------------------------
# Executors

class CommandExecutor(object):
    """Wrapper class for executing shell commands while maintaining a command
    history.
    """

    def __init__(self, env=None,
                 stdin=None, stdout=None, stderr=None):
        self._env = env
        self._stdin = stdin or sys.stdin
        self._stdout = stdout or sys.stderr
        self._stderr = stderr or sys.stdout

        # Actions performed before executing each command
        self._actions = []

        # Executed command history
        self._command_history = []

        def store_history_action(command):
            self._command_history.append(command)

        self.add_action(store_history_action)

    def _exec_actions(self, command):
        command = _quote_command(command)

        for action in self._actions:
            action(command)

    def add_action(self, action):
        """Add an action to the executor which will be called prior to excuting
        commands. Actions must be callable and accept a single argument, the
        command being executed.
        """

        self._actions.append(action)

    def add_echo_action(self, output_stream=None, prefix=None):
        """Adds an echo action to the executor which will output the command
        to an output stream or stdout. An optional prefix can be used to format
        the final command.
        """

        self.add_action(_EchoAction(output_stream or self._stdout, prefix))

    def _kwargs_set_defaults(self, kwargs):
        defaults = {
            'env': self._env,
            'stdin': self._stdin,
            'stdout': self._stdout,
            'stderr': self._stderr,
        }

        for key, value in defaults.items():
            kwargs.setdefault(key, value)

        return kwargs

    def _fake_popen(self, command):
        self._exec_actions(command)
        return _FakePopen()

    def _popen(self, command, **kwargs):
        self._exec_actions(command)

        kwargs = self._kwargs_set_defaults(kwargs)
        p = subprocess.Popen(command, **kwargs)

        try:
            p.wait()
        except Exception as e:
            p.kill()
            p.wait()
            raise e

        return p

    def popen(self, command, **kwargs):
        """Create and return an instance of subprocess.Popen with the
        given command and kwargs.
        """

        return self._popen(command, **kwargs)

    def call(self, command):
        """Run command. Wait for the command to exit and then return the
        returncode.
        """

        return self.popen(command).returncode

    def check_call(self, command):
        """Run command. Wait for the command to exit. If the returncode was 0
        then return, otherwise raises CalledProcessError.
        """

        p = self.popen(command)
        if p.returncode:
            raise CalledProcessError(p.returncode, command)

        return 0

    def check_output(self, command):
        """Run command. Wait for the command to exit. If the returncode was 0
        then return the output, otherwise raises CalledProcessError.
        """

        p = self.popen(command, stdout=subprocess.PIPE)
        output, _ = p.communicate()

        returncode = p.poll()
        if returncode:
            raise CalledProcessError(returncode, command, output=output)

        return output

    def history(self):
        """Return the executor's command history.
        """

        return copy(self._command_history)

    # -------------------------------------------------------------------------
    # Common shell utility wrappers

    def cd(self, path):
        self._fake_popen(['cd', path])
        os.chdir(path)

    def cp(self, source, destination):
        self._fake_popen(['cp', '-r', source, destination])
        if os.path.isfile(source):
            shutil.copyfile(source, destination)
        elif os.path.isdir(source):
            shutil.copytree(source, destination)
        else:
            raise OSError('{} does not exist'.format(source))

    def ls(self, path=None):
        path = path or os.getcwd()
        self._fake_popen(['ls', path])
        return os.listdir(path)

    def mkdir(self, path):
        self._fake_popen(['mkdir', '-p', path])
        if not os.path.isdir(path):
            os.makedirs(path)

    def mv(self, source, destination):
        self._fake_popen(['mv', source, destination])
        shutil.move(source, destination)

    @contextmanager
    def pushd(self, path):
        cwd = os.getcwd()
        self._fake_popen(['pushd', path])
        os.chdir(path)

        yield

        self._fake_popen(['popd'])
        os.chdir(cwd)

    def rm(self, path):
        self._fake_popen(['rm', '-rf', path])
        if os.path.isfile(path):
            os.remove(path)
        elif os.path.isdir(path):
            shutil.rmtree(path)
        else:
            raise OSError('{} does not exist'.format(path))

    def which(self, name):
        try:
            return self.check_output(['which', name]).strip()
        except CalledProcessError:
            return None


class NullExecutor(CommandExecutor):
    """Variant of the CommandExecutor that does not execute any subprocess
    commands. This class should be useful for testing and iterating on scripts
    while still retaining output logging and history functionality.
    """

    def popen(self, command, **kwargs):
        return self._fake_popen(command, **kwargs)

    # Overriding commands that mutate the system

    def cd(self, path):
        self._fake_popen(['cd', path])

    def cp(self, source, destination):
        self._fake_popen(['cp', '-r', source, destination])

    def mkdir(self, path):
        self._fake_popen(['mkdir', '-p', path])

    def mv(self, source, destination):
        self._fake_popen(['mv', source, destination])

    @contextmanager
    def pushd(self, path):
        self._fake_popen(['pushd', path])
        yield
        self._fake_popen(['popd'])

    def rm(self, path):
        self._fake_popen(['rm', '-rf', path])
